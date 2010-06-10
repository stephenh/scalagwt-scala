/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.tools.nsc
package repl

import Predef.{ println => _, _ }
import annotation.tailrec
import scala.tools.cmd.program.GlobalTokenizer
import io.{ Path, File, Process }

/** The 
 *  <a href="http://scala-lang.org/" target="_top">Scala</a>
 *  interactive shell.  It provides a read-eval-print loop around
 *  the Interpreter class.
 *  After instantiation, clients should call the <code>main()</code> method.
 *
 *  <p>If no in0 is specified, then input will come from the console, and
 *  the class will attempt to provide input editing feature such as
 *  input history.
 *
 *  @author Moez A. Abdel-Gawad
 *  @author  Lex Spoon
 *  @version 1.2
 */
class InterpreterLoop(in0: Option[BufferedReader], protected val out: PrintWriter) extends LoopCommands {
  def this(in0: BufferedReader, out: PrintWriter) = this(Some(in0), out)
  def this() = this(None, new PrintWriter(Console.out))

  /** The input stream from which commands come, set by main() */
  var in: InteractiveReader = _
  def ifJline[T](f: JLineReader => T): Option[T] = in match {
    case x: JLineReader => Some(f(x))
    case _              => None
  }

  /** The context class loader at the time this object was created */
  protected val originalClassLoader = Thread.currentThread.getContextClassLoader
  
  var settings: Settings = _    // set by main()
  var repl: Interpreter = _     // set by createInterpreter()
  def DBG(s: String) = repl.DBG(s)
  def isInteractive: Boolean = true

  // classpath entries added via :cp
  var addedClasspath: String = ""

  /** A reverse list of commands to replay if the user requests a :replay */
  var replayCommandStack: List[String] = Nil

  /** A list of commands to replay if the user requests a :replay */
  def replayCommands = replayCommandStack.reverse

  /** Record a command for replay should the user request a :replay */
  def addReplay(cmd: String) = replayCommandStack ::= cmd
  
  /** All commands whether for replay or not */
  val allSessionCommands = new ListBuffer[String]
  
  private val CONTINUATION_STRING = "     | "
  private val PROMPT_STRING = "scala> "
  
  /** Prompt to print when awaiting input */
  val prompt = Properties.shellPromptString

  /** Close the interpreter and set the var to <code>null</code>. */
  def closeInterpreter() {
    if (repl ne null) {
      repl.close
      repl = null
      Thread.currentThread.setContextClassLoader(originalClassLoader)
    }
  }

  /** Create a new repl. */
  def createInterpreter() { createInterpreter(classOf[InterpreterLoop].getClassLoader) }
  def createInterpreter(cl: ClassLoader) {
    if (addedClasspath != "")
      settings.classpath append addedClasspath
      
    repl = new Interpreter(settings, out) {
      override protected def parentClassLoader = cl
      override def toString = "Interpreter(%s)".format(settings.classpath)
    }
    repl.setContextClassLoader()
  }

  /** Print a welcome message */
  def printWelcome() {
    import Properties._
    val welcomeMsg = 
     """|Welcome to Scala %s (%s, Java %s).
        |Type in expressions to have them evaluated.
        |Type :help for more information.""" .
    stripMargin.format(versionString, javaVmName, javaVersion)
        
    plushln(welcomeMsg)
  }

  /** Some print conveniences */
  def println(x: Any) = out println x
  def plush(x: Any)   = { out print x ; out.flush() }
  def plushln(x: Any) = { out println x ; out.flush() }

  /** Lazy vals are good for run-once code too.  If isInteractive is false
   *  (which you have to go out of your way to accomplish, by subclassing
   *  InterpreterLoop) the standard convenience variables are not injected,
   *  and the Interpreter object is initialized synchronously.
   */
  private lazy val loopInitialize: Unit = {
    out.flush
    if (isInteractive) {
      def enqueue(f: Interpreter => Unit): Unit = repl postInit f
      enqueue(_.injectReplVal("history", "scala.collection.immutable.List[String]", in.historyList))
      enqueue(_.injectReplVal[CmdCompletion]("cmds", Cmds))
      enqueue(repl => in.completion foreach (x => repl.injectReplVal("completion", "scala.tools.nsc.repl.Completion", x)))
      enqueue(_.compileReplVals())
      repl.initializeAsync()
    }
    else repl.initializeSync()
  }

  /** The main read-eval-print loop for the repl.  It calls
   *  command() with each line of input, stopping at end of input
   *  or when command() returns false.
   */
  def loop() {
    loopInitialize

    def readOneLine() = {
      out.flush
      in readLine prompt
    }
    def processLine(line: String): Boolean =
      if (line eq null) false               // null should mean EOF
      else command(line) match {
        case Result(false, _)           => false
        case Result(_, Some(finalLine)) => addReplay(finalLine) ; true
        case _                          => true
      }

    while (processLine(readOneLine)) { }
  }

  /** interpret all lines from a specified file */
  def interpretAllFrom(file: File) {    
    val oldIn = in
    val oldReplay = replayCommandStack
    
    try file applyReader { reader =>
      in = new SimpleReader(reader, out, false)
      plushln("Loading " + file + "...")
      loop()
    }
    finally {
      in = oldIn
      replayCommandStack = oldReplay
    }
  }

  def withFile(filename: String)(action: File => Unit) {
    val f = File(filename)
    
    if (f.exists) action(f)
    else out.println("That file does not exist")
  }

  def demandPatience() {
    if (isInteractive)
      plush("Interpreter is still initializing, please wait...")
      
    // force the lazy val and then wait for the signal
    repl.compiler
    repl.initLatch.await()

    if (isInteractive)
      plushln("done.")
  }
  
  object tokenizer extends GlobalTokenizer {
    override def global =
      if (repl != null && repl.isInitDone) repl.compiler
      else super.global
    
    def apply(code: String): Unit = {
      val xs: List[Any] = fromScalaString(code)
      repl.bindT(repl.getVarName, xs)
      // repl.bind(name, tpe, value)
      
      // injectAny(xs)
    } 
  }
  
  /** Run one command submitted by the user.  Two values are returned:
    * (1) whether to keep running, (2) the line to record for replay,
    * if any. */
  def command(line: String): Result = {
    def withError(msg: String) = {
      out println msg
      Result(true, None)
    }
    def ambiguous(cmds: List[Command]) = "Ambiguous: did you mean " + cmds.map(":" + _.name).mkString(" or ") + "?"

    // Notice if the compiler isn't ready
    if (!repl.isInitDone) demandPatience()
    // Notice if compiler creation failed
    if (repl.compiler == null) return Result(false, None)

    // not a command
    if (!line.startsWith(":"))
      return Result(true, interpretStartingWith(line))

    val tokens = (line drop 1 split """\s+""").toList
    if (tokens.isEmpty)
      return withError(ambiguous(commands))
    
    val (cmd :: args) = tokens
    
    // this lets us add commands willy-nilly and only requires enough command to disambiguate
    commands.filter(_.name startsWith cmd) match {
      case List(x)  => allSessionCommands += line ; x(args)
      case Nil      => withError("Unknown command.  Type :help for help.")
      case xs       => withError(ambiguous(xs))
    }
  }
  
  /** If it looks like they're pasting in a scala interpreter
   *  transcript, remove all the formatting we inserted so we
   *  can make some sense of it.
   */
  private var pasteStamp: Long = 0

  /** Returns true if it's long enough to quit. */
  def updatePasteStamp(): Boolean = {
    /* Enough milliseconds between readLines to call it a day. */
    val PASTE_FINISH = 1000

    val prevStamp = pasteStamp
    pasteStamp = System.currentTimeMillis
    
    (pasteStamp - prevStamp > PASTE_FINISH)
  
  }
  /** TODO - we could look for the usage of resXX variables in the transcript.
   *  Right now backreferences to auto-named variables will break.
   */
  
  /** The trailing lines complication was an attempt to work around the introduction
   *  of newlines in e.g. email messages of repl sessions.  It doesn't work because
   *  an unlucky newline can always leave you with a syntactically valid first line,
   *  which is executed before the next line is considered.  So this doesn't actually
   *  accomplish anything, but I'm leaving it in case I decide to try harder.
   */
  case class PasteCommand(cmd: String, trailing: ListBuffer[String] = ListBuffer[String]())
  
  /** Commands start on lines beginning with "scala>" and each successive
   *  line which begins with the continuation string is appended to that command.
   *  Everything else is discarded.  When the end of the transcript is spotted,
   *  all the commands are replayed.
   */
  @tailrec private def cleanTranscript(lines: List[String], acc: List[PasteCommand]): List[PasteCommand] = lines match {
    case Nil                                    => acc.reverse      
    case x :: xs if x startsWith PROMPT_STRING  =>
      val first = x stripPrefix PROMPT_STRING
      val (xs1, xs2) = xs span (_ startsWith CONTINUATION_STRING)
      val rest = xs1 map (_ stripPrefix CONTINUATION_STRING)
      val result = (first :: rest).mkString("", "\n", "\n")
      
      cleanTranscript(xs2, PasteCommand(result) :: acc)
      
    case ln :: lns =>
      val newacc = acc match {
        case Nil => Nil
        case PasteCommand(cmd, trailing) :: accrest =>
          PasteCommand(cmd, trailing :+ ln) :: accrest
      }
      cleanTranscript(lns, newacc)
  }

  /** The timestamp is for safety so it doesn't hang looking for the end
   *  of a transcript.  Ad hoc parsing can't be too demanding.  You can
   *  also use ctrl-D to start it parsing.
   */
  @tailrec private def interpretAsPastedTranscript(lines: List[String]) {
    val line = in.readLine("")
    val finished = updatePasteStamp()

    if (line == null || finished || line.trim == PROMPT_STRING.trim) {
      val xs = cleanTranscript(lines.reverse, Nil)
      println("Replaying %d commands from interpreter transcript." format xs.size)
      for (PasteCommand(cmd, trailing) <- xs) {
        out.flush()
        def runCode(code: String, extraLines: List[String]) {
          (repl interpret code) match {
            case IR.Incomplete if extraLines.nonEmpty =>
              runCode(code + "\n" + extraLines.head, extraLines.tail)
            case _ => ()
          }
        }
        runCode(cmd, trailing.toList)
      }
    }
    else
      interpretAsPastedTranscript(line :: lines)
  }

  /** Interpret expressions starting with the first line.
    * Read lines until a complete compilation unit is available
    * or until a syntax error has been seen.  If a full unit is
    * read, go ahead and interpret it.  Return the full string
    * to be recorded for replay, if any.
    */
  def interpretStartingWith(code: String): Option[String] = {
    // signal completion that non-completion input has been received
    in.completion foreach (_.resetVerbosity())
    
    def reallyInterpret = repl.interpret(code) match {
      case IR.Error       => None
      case IR.Success     =>
        allSessionCommands += code
        Some(code)
      case IR.Incomplete  =>
        if (in.interactive && code.endsWith("\n\n")) {
          out.println("You typed two blank lines.  Starting a new command.")
          None
        } 
        else in.readLine(CONTINUATION_STRING) match {
          case null =>
            // we know compilation is going to fail since we're at EOF and the
            // parser thinks the input is still incomplete, but since this is
            // a file being read non-interactively we want to fail.  So we send
            // it straight to the compiler for the nice error message.
            repl.compileString(code)
            None

          case line => interpretStartingWith(code + "\n" + line)
        }
    }
    
    /** Here we place ourselves between the user and the interpreter and examine
     *  the input they are ostensibly submitting.  We intervene in several cases:
     * 
     *  1) If the line starts with "scala> " it is assumed to be an interpreter paste.
     *  2) If the line starts with "." (but not ".." or "./") it is treated as an invocation
     *     on the previous result.
     *  3) If the Completion object's execute returns Some(_), we inject that value
     *     and avoid the interpreter, as it's likely not valid scala code.
     */
    if (code == "") None
    else if (code startsWith PROMPT_STRING) {
      updatePasteStamp()
      interpretAsPastedTranscript(List(code))
      None
    }
    else if (Completion.looksLikeInvocation(code) && repl.latestVar.nonEmpty) {
      interpretStartingWith(repl.latestVar.get + code)
    }
    else {
      val result = for (comp <- in.completion ; res <- comp execute code) yield res
      result match {
        // completion took responsibility, so do not parse
        case Some(res)  =>
          allSessionCommands += code
          injectAny(res)
          None  
        case _  => 
          reallyInterpret
      }
    }
  }

  def main(settings: Settings) {
    this.settings = settings
    createInterpreter()
    
    // sets in to some kind of reader depending on environmental cues
    in = in0 match {
      case Some(in0)  => new SimpleReader(in0, out, true)
      case None       =>        
        // the interpreter is passed as an argument to expose tab completion info
        if (settings.Xnojline.value || Properties.isEmacsShell) new SimpleReader
        else if (settings.noCompletion.value) InteractiveReader.createDefault()
        else InteractiveReader.createDefault(repl)
    }

    loadFiles(settings)
    try {
      // it is broken on startup; go ahead and exit
      if (repl.reporter.hasErrors) return
      
      printWelcome()
      loop()
    }
    finally closeInterpreter()
  }
  
  def inject[T: OptManifest](name: String, value: T) = {
    val tpe = typeString(value)
    repl.quietlyBind(name, tpe, value)
    (name, tpe)
  }
  def inject[T: OptManifest](value: T): (String, String) =
    inject(repl.getVarName, value)
    
  def injectAny(value: Any) = {
    val name = repl.getVarName
    val tpe = typeStringJava(value)
    repl.bind(name, tpe, value)
    (name, tpe)
  }

  /** process command-line arguments and do as they request */
  def main(args: Array[String]) {
    def error1(msg: String) = out println ("scala: " + msg)
    val command = new InterpreterCommand(args.toList, error1)
    def neededHelp(): String =
      (if (command.settings.help.value) command.usageMsg + "\n" else "") +
      (if (command.settings.Xhelp.value) command.xusageMsg + "\n" else "")
    
    // if they asked for no help and command is valid, we call the real main
    neededHelp() match {
      case ""     => if (command.ok) main(command.settings) // else nothing
      case help   => plush(help)
    }
  }
}
