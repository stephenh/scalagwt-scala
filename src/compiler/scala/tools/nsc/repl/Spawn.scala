/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{ Future, FutureTask, Callable, Executors }
import util.{ timed, returning, ifPermitted }
import java.security.{ Permission, AccessControlException }

/** Even if we wanted to use actors, the bootstrap process currently doesn't
 *  allow for it, so we roll some low intensity parallelism abstractions designed
 *  for failing gracefully.
 */
object Spawn {
  private def securityFail  = throw new SecurityException("")  // testing
  private def securityMgr   = System.getSecurityManager
  private val threadPerms   = List("modifyThread", "modifyThreadGroup") map (x => new RuntimePermission(x))
  private def checkPerm(p: Permission) = securityMgr checkPermission p
  
  // AccessController.checkPermission doesn't work on google app engine, and given
  // that it's about the only reason for this to exist, we use the older security
  // manager interface.
  def canCreateThreads = (securityMgr == null) || ifPermitted(threadPerms foreach checkPerm).isDefined

  /** A wrapper around Callable which measures the call's duration, and optionally
   *  performs some action upon completion.
   */
  class TimedCallable[T](callable: Callable[T], whenDone: TimedCallable[T] => Unit) extends Callable[T] {
    def this(c: Callable[T]) = this(c, _ => ())
    def this(f: () => T) = this(callableFn(f))
    
    private var callMillis: Long = 0
    private def timeCall(millis: Long) = callMillis = millis
    
    def elapsedSeconds  = callMillis.toDouble / 1000
    def call(): T       = returning(timed(timeCall)(callable.call()))(_ => whenDone(this))
    def callTimed(): T  = call()   // for implicit conversion
    override def toString = "TimedCallable(%s seconds)".format(callMillis)
  }  
  
  /** A simple abstraction for something which may or may not have the
   *  chance to run asynchronously.
   */
  sealed abstract class SyncOrAsyncAction[T] {
    def create: () => T
    def isStarted: Boolean
    def isDone: Boolean
    def get: T
  }
  trait FuturizedAction[T] extends SyncOrAsyncAction[T] {
    def futurize: () => FutureTask[T]
    def create = futurize().get
  }
  class AsyncInit[T](val futurize: () => FutureTask[T]) extends SyncOrAsyncAction[T] with FuturizedAction[T] {
    private lazy val future: Option[FutureTask[T]] = ifPermitted { returning(futurize())(submit) }

    def isStarted = future.isDefined
    def isDone    = future exists (_.isDone())
    def get       = future.get.get
  }
  class SyncInit[T](val create: () => T) extends SyncOrAsyncAction[T] {
    private lazy val result: T = create()
    
    def isStarted = result != null
    def isDone    = isStarted
    def get       = result
  }
  
  /** Some boilerplate around a Condition.
   */
  class Latch() {
    @volatile private var isFlipped = false
    private val lock = new ReentrantLock()
    private val condition = lock.newCondition()
    
    def await() {
      lock.lock()
      while (!isFlipped) {
        try condition.await()
        finally lock.unlock()
      }
    }
    
    def signal[T](body: => T): T = {
      var result: T = null.asInstanceOf[T]
      lock.lock()
      try {
        result = body
        isFlipped = true
        condition.signal()
      }
      finally lock.unlock()
      
      result
    }
  }
  
  def asyncOrSync[T](async: () => FutureTask[T], sync: () => T): SyncOrAsyncAction[T] = {
    val firstTry = new AsyncInit(async)
    if (firstTry.isStarted) firstTry
    else new SyncInit(sync)
  }

  def timedCallable[T](body: () => T)(whenDone: TimedCallable[T] => Unit): TimedCallable[T] =
    new TimedCallable[T](callableFn(body), whenDone)
  
  def runnable(body: => Unit): Runnable     = new Runnable { override def run() = body }
  def callable[T](body: => T): Callable[T]  = new Callable[T] { override def call() = body }
  def spawn[T](body: => T): Future[T]       = Executors.newSingleThreadExecutor() submit callable[T](body)
  def submit(runnable: Runnable)            = Executors.newSingleThreadExecutor() submit runnable

  def runnableFn(f: () => Unit): Runnable   = runnable(f())
  def callableFn[T](f: () => T): Callable[T]= callable(f())
  def spawnFn[T](f: () => T): Future[T]     = spawn(f())
}
