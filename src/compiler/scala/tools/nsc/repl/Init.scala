/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import java.util.concurrent.{ Callable, Future, FutureTask, LinkedBlockingQueue }
import Spawn._

/** Initializing the interpreter is a very, very delicate process.  While it
 *  is of course likely this code can be improved, be wary of "obvious" improvements
 *  because this structure is born of a great deal of trial and error.
 *
 *  Here are the goals it is trying to achieve:
 *
 *  1) The compiler object must be stored in a val, not a var, so it is a stable id.
 *  2) The compiler must not be used for anything until it is fully initialized, which
 *  means (at least) an instance of compiler.Run() has been created.
 *  3) Since arbitrary code is written to use the compiler lazy val, if it is
 *  referenced before initialization is complete, it must not finish unthunking
 *  until initialization is done.
 *  4) ...and 3) must be done without deadlocking.
 *  5) The repl has a far more responsive feel if we allow people to start typing
 *  right away, which they can't do if the main thread is blocking waiting for the
 *  compiler to initialize.  So we need to be able to initialize asynchronously.
 *  6) ...but we may also be created in an environment where we can't create
 *  threads such as GAE, so if async init fails, we fail to sync init.
 *  7) ...or we may be created programmatically, in which case we can't count
 *  on anything taking place between "new Interpreter" and "repl.compiler".
 *
 *  Additionally there are various niceties for the interpreter loop, such as
 *  completion for injected variables like "settings" working as soon as possible:
 *  for this we need for additional code to be run after the compiler is created.
 */
trait Init {
  self: Interpreter =>

  /** For signalling that the compiler is fully initialized. */
  val initLatch = new Latch()
  
  /** Enqueue a command to be run as soon as the compiler is set up. */
  def postInit(f: Interpreter => Unit) = postInitQueue put f
  
  /** This info is only seen under -Yrepl-debug.
   */
  private def printTimerInfo(fmtString: String, x: Any) = DBG(fmtString format x)
  private def printCompilerInitInfo(x: Any) = printTimerInfo("Repl compiler initialized in %s seconds", x)
  private def printPostInitInfo(x: Any)     = printTimerInfo("Post init queue took %s seconds", x)

  /** We're going to go to some trouble to initialize the compiler asynchronously.
   *  It's critical that nothing call into it until it's been initialized or we will
   *  run into unrecoverable issues, but the perceived repl startup time goes
   *  through the roof if we wait for it.  So we initialize it with a future and
   *  use a lazy val to ensure that any attempt to use the compiler object waits
   *  on the future.
   */  
  private def createCompiler(code: String) = {
    try {
      val c: Global = newCompiler(settings, reporter)
      new c.Run() compileSources List(new BatchSourceFile("<init>", code))
      c
    }
    catch {
      case MissingRequirementError(msg) => println("""
        |Failed to initialize compiler: %s not found.
        |** Note that as of 2.8 scala does not assume use of the java classpath.
        |** For the old behavior pass -usejavacp to scala, or if using a Settings
        |** object programatically, settings.usejavacp.value = true.""".stripMargin.format(msg)
      )
      null
    }
  }
  
  private def source = """
    |// this is assembled to force the loading of approximately the
    |// classes which will be loaded on the first expression anyway.
    |object $repl_init {
    |  scala.runtime.ScalaRunTime.stringOf(
    |    "abc".reverse.length + (5 max 5)
    |  )
    |}
    |""".stripMargin

  /** A buffer for commands which need an initialized compiler to run: they wait here
   *  until postInit() is reached, at which point they're all run.
   */
  private val postInitQueue   = new LinkedBlockingQueue[Interpreter => Unit]
  private def processQueue()  = while (!postInitQueue.isEmpty()) postInitQueue.take()(self)

  private val timedInit       = timedCallable(() => createCompiler(source))(x => printCompilerInitInfo(x.elapsedSeconds))
  private val timedPostInit   = timedCallable(() => processQueue())(x => printPostInitInfo(x.elapsedSeconds))
  
  /** A custom FutureTask so we can override done() and run the post init queue.
   */
  class CompilerInitTask(callable: Callable[Global]) extends FutureTask(callable) {
    override protected def done() = {
      DBG("CompilerInitTask done, %d items in queue".format(postInitQueue.size))
      initLatch.signal(timedPostInit.call())
    }
  }
  private def syncInit()  = timedInit.call()
  private def asyncInit() = new CompilerInitTask(callable(syncInit()))

  private def newInitStrategy() = asyncOrSync(() => asyncInit(), () => syncInit())

  private var initStrategy: SyncOrAsyncAction[Global] = null
  private def setFallback() = initStrategy = new SyncInit(() => syncInit())
  def isInitDone = initStrategy != null && initStrategy.isDone
  
  /** Normal interactive usage tries to use async.
   */
  def initializeAsync() =
    if (initStrategy == null)
      initStrategy = newInitStrategy()
  
  /** Calling this forces immediate synchronous initialization.
   */
  def initializeSync() = {
    setFallback()
    compiler
    processQueue()
    ()
  }

  /** The public compiler.  If we still have no strategy at this point, we
   *  go straight to synchronous initialization to avoid deadlock issues.
   *  This mostly comes up when using the interpreter programmatically, where
   *  being synchronous is desirable anyway.
   */
  lazy val compiler: Global = {
    if (initStrategy == null)
      setFallback()

    initStrategy.get
  }
}
