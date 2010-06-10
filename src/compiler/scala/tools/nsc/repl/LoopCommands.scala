/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.tools.nsc
package repl

import Predef.{ println => _, _ }
import util.{ ClassPath, ShowPickled }
import io.{ Path, File, Process }
import ByteCode.scalaSigBytesForClassBytes

trait CmdCompletion extends CompletionAware {
  val loop: InterpreterLoop
  import loop._
  
  def update(cmdName: String, f: InterpreterLoop => String => Unit): Unit = {
    val newCommand = LineArg(cmdName, "no help", (x: String) => { f(loop)(x) ; defaultResult })
    addCommand(newCommand)
  }
  def completions = commands map (_.name) sorted
}

trait LoopCommands {
  self: InterpreterLoop =>
  
  // most commands do not want to micromanage the Result, but they might want
  // to print something to the console, so we accomodate Unit and String returns.
  object CommandImplicits {
    implicit def irresult2r(x: IR.Result): Result = defaultResult
    implicit def unit2r(x: Unit): Result = defaultResult
    implicit def string2r(s: String): Result = {
      out println s
      defaultResult
    }
  }
  
  /** The cmds object completion. */
  object Cmds extends {
    val loop: self.type = self
  } with ExecCompletionAware[Command] with CmdCompletion {
    override def completions(verbosity: Int) = completions
    override def execute(id: String): Option[Command] = commands find (_.name == id)
  }
  
  // the default result means "keep running, and don't record that line"
  val defaultResult = Result(true, None)
  
  // a single interpreter command
  sealed abstract class Command extends Function1[List[String], Result] {
    def name: String
    def help: String
    def error(msg: String) = {
      out.println(":" + name + " " + msg + ".")
      Result(true, None)
    }
    def usage(): String
  }
  
  case class NoArgs(name: String, help: String, f: () => Result) extends Command {
    def usage(): String = ":" + name
    def apply(args: List[String]) = if (args.isEmpty) f() else error("accepts no arguments")
  }
  
  case class LineArg(name: String, help: String, f: (String) => Result) extends Command {
    def usage(): String = ":" + name + " <line>"
    def apply(args: List[String]) = f(args mkString " ")
  }

  case class OneArg(name: String, help: String, f: (String) => Result) extends Command {
    def usage(): String = ":" + name + " <arg>"
    def apply(args: List[String]) =
      if (args.size == 1) f(args.head)
      else error("requires exactly one argument")
  }

  case class VarArgs(name: String, help: String, f: (List[String]) => Result) extends Command {
    def usage(): String = ":" + name + " [arg]"
    def apply(args: List[String]) = f(args)
  }

  // the result of a single command
  case class Result(keepRunning: Boolean, lineToRecord: Option[String])
  
  /** Available commands */
  def commands: List[Command] = standardCommands ++ addedCommands
  
  /** Standard commands **/
  val standardCommands: List[Command] = {
    import CommandImplicits._
    List(
       LineArg("basetype", "show the base type sequence of a type", baseType),
       OneArg("cp", "add an entry (jar or directory) to the classpath", addClasspath),
       VarArgs("edit", "[-f] [file] open repl session in an editor", edit),
       NoArgs("decls", "show the decl history", () => repl.declHistory foreach println),
       NoArgs("help", "print this help message", printHelp),
       VarArgs("history", "show the history (optional arg: lines to show)", printHistory),
       LineArg("h?", "search the history", searchHistory),
       VarArgs("javap", "disassemble a class", printJavap),
       OneArg("load", "load and interpret a Scala file", load),
       OneArg("pickled", "show a type's scala signature data", showPickled),
       NoArgs("quit", "exit the interpreter", () => Result(false, None)),
       NoArgs("replay", "reset execution and replay all previous commands", replay),
       NoArgs("reqs", "show the req history", () => repl.reqHistory foreach println),
       LineArg("sh", "fork a shell and run a command", runShellCmd),
       NoArgs("silent", "disable/enable automatic printing of results", verbosity),
       LineArg("sym", "obtain the compiler Symbol for the given identifier", x => bindSym(x): Unit),
       LineArg("tokens", "tokenize the given source", x => tokenizer(x)),
       LineArg("tree", "obtain the compiler Tree for the given identifier", x => bindTree(x): Unit),
       LineArg("type", "obtain the compiler Type for the given identifier", x => bindType(x): Unit),
       NoArgs("unleash", "import all the compiler magic", () => repl.unleash())
    )
  }
  
  /** Custom commands */
  val addedCommands: ListBuffer[Command] = new ListBuffer[Command]()
  def addCommand(cmd: Command) = addedCommands += cmd

  /** print a friendly help message */
  def printHelp() = {
    out println "All commands can be abbreviated - for example :he instead of :help.\n"
    val cmds = commands map (x => (x.usage, x.help))
    val width: Int = cmds map { case (x, _) => x.length } max
    val formatStr = "%-" + width + "s %s"
    cmds foreach { case (usage, help) => out println formatStr.format(usage, help) }
  } 

  /** Show the history */
  def printHistory(xs: List[String]) {
    val defaultLines = 20
    if (in.history.isEmpty)
      return println("No history available.")

    val current = in.history.get.index
    val count = try xs.head.toInt catch { case _: Exception => defaultLines }
    val lines = in.historyList takeRight count
    val offset = current - lines.size + 1

    for ((line, index) <- lines.zipWithIndex)
      println("%d %s".format(index + offset, line))
  }

  /** Search the history */
  def searchHistory(_cmdline: String) {
    val cmdline = _cmdline.toLowerCase
    
    if (in.history.isEmpty)
      return println("No history available.")
    
    val current = in.history.get.index
    val offset = current - in.historyList.size + 1
    
    for ((line, index) <- in.historyList.zipWithIndex ; if line.toLowerCase contains cmdline)
      println("%d %s".format(index + offset, line))
  }
  
  def edit(args: List[String]): Unit = {    
    val file =
      if (args.isEmpty) File.makeTemp("repl_edit_", ".scala")
      else {
        val f = File(args.last)
        if (f.exists && (args.size != 2 || args.head != "-f"))
          return println(f + " already exists: use -f to overwrite.")
        
        if (f.isDirectory)
          return println("Cannot edit " + f)

        f
      }
    
    file writeAll allSessionCommands.mkString("", "\n\n", "\n")
    Desktop edit file.jfile
  }
  
  def baseType(arg: String): Unit =
    repl.atTyper[List[Global#Type]](bindType(arg).baseTypeSeq.toList) foreach println

  def bindTree(arg: String) =
    repl.TreeBinder(arg, println("Cannot locate tree '%s'".format(arg)))
  
  def bindSym(arg: String) =
    repl.SymBinder(arg, println("Cannot locate symbol '%s'".format(arg)))
  
  def bindType(arg: String) =
    repl.TypeBinder(arg, println("Cannot locate type '%s'".format(arg)))

  /** create a new interpreter and replay all commands so far */
  def replay() {
    closeInterpreter()
    createInterpreter()
    for (cmd <- replayCommands) {
      plushln("Replaying: " + cmd)  // flush because maybe cmd will have its own output
      command(cmd)
      out.println
    }
  }
    
  /** fork a shell and run a command */
  def runShellCmd(line: String) {
    // we assume if they're using :sh they'd appreciate being able to pipeline
    repl.quietly {
      repl.interpret("import scala.tools.nsc.io.Process.Pipe._")
    }
    val p = Process(line)
    // only bind non-empty streams
    def add(name: String, it: Iterator[String]) =
      if (it.hasNext)
        inject(name, it.toList)
    
    List(("stdout", p.stdout), ("stderr", p.stderr)) foreach (add _).tupled
  }
  
  def printJavap(args: List[String]): Unit = args match {
    case Nil      => println("Usage: javap <className> [method method ...]")
    case c :: Nil => repl.javap(c)
    case c :: ms  => repl.javap(c, x => ms contains (x.getName))
  }
  
  def load(arg: String) = {
    var shouldReplay: Option[String] = None
    withFile(arg)(f => {
      interpretAllFrom(f)
      shouldReplay = Some(":" + "load " + arg)
    })
    Result(true, shouldReplay)
  }
  
  def showPickled(arg: String): Unit = {
    val bytes = repl.nonEmptyBytes(arg) getOrElse {
      println("Couldn't find classfile bytes for '%s'".format(arg))
      return
    }      
    val sig = scalaSigBytesForClassBytes(bytes) getOrElse {
      println("Couldn't find scala sig for '%s'".format(arg))
      return
    }      
    val picklebuffer = ShowPickled.fromBytes(sig) getOrElse {
      println("Couldn't parse pickled data for '%s'".format(arg))
      return
    }
      
    ShowPickled.printFile(picklebuffer, Console.out)
  }

  def addClasspath(arg: String): Unit = {
    val f = File(arg).normalize
    if (f.exists) {
      addedClasspath = ClassPath.join(addedClasspath, f.path)
      val totalClasspath = ClassPath.join(settings.classpath.value, addedClasspath)
      println("Added '%s'.  Your new classpath is:\n%s".format(f.path, totalClasspath))
      replay()
    }
    else out.println("The path '" + f + "' doesn't seem to exist.")
  }
  
  def verbosity() = {
    val old = repl.printResults
    repl.printResults = !old
    out.println("Switched " + (if (old) "off" else "on") + " result printing.")
  }

  // runs :load <file> on any files passed via -i
  def loadFiles(settings: Settings) = settings match {
    case settings: GenericRunnerSettings =>
      for (filename <- settings.loadfiles.value) {
        val cmd = ":" + "load " + filename
        command(cmd)
        addReplay(cmd)
        out.println()
      }
    case _ =>
  }
}

