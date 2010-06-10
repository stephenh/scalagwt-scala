/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package repl

import Predef.{ println => _, _ }

import scala.collection.{ mutable, immutable }
import mutable.{ HashMap, ArrayBuffer }
import scala.util.control.Exception.{ ultimately }
import scala.tools.util.PathResolver

import io.{ VirtualDirectory }
import reporters.{ ConsoleReporter, Reporter }
import symtab.Flags
import util.{ SourceFile, stringFromWriter, returning }
import Strings._

trait LowPriorityInterpreter {
  self: Interpreter =>
  
  import compiler.Name
  
  implicit def name2string(n: Name): String = n.toString
  implicit val nameOrdering: Ordering[Name] = Ordering[String] on (_.toString)
}

/** <p>
 *    An interpreter for Scala code.
 *  </p>
 *  <p>
 *    The main public entry points are <code>compile()</code>,
 *    <code>interpret()</code>, and <code>bind()</code>.
 *    The <code>compile()</code> method loads a
 *    complete Scala file.  The <code>interpret()</code> method executes one
 *    line of Scala code at the request of the user.  The <code>bind()</code>
 *    method binds an object to a variable that can then be used by later
 *    interpreted code.
 *  </p>
 *  <p>
 *    The overall approach is based on compiling the requested code and then
 *    using a Java classloader and Java reflection to run the code
 *    and access its results.
 *  </p>
 *  <p>  
 *    In more detail, a single compiler instance is used
 *    to accumulate all successfully compiled or interpreted Scala code.  To
 *    "interpret" a line of code, the compiler generates a fresh object that
 *    includes the line of code and which has public member(s) to export
 *    all variables defined by that code.  To extract the result of an
 *    interpreted line to show the user, a second "result object" is created
 *    which imports the variables exported by the above object and then
 *    exports a single member named "scala_repl_result".  To accomodate user expressions
 *    that read from variables or methods defined in previous statements, "import"
 *    statements are used.
 *  </p>
 *  <p>
 *    This interpreter shares the strengths and weaknesses of using the
 *    full compiler-to-Java.  The main strength is that interpreted code
 *    behaves exactly as does compiled code, including running at full speed.
 *    The main weakness is that redefining classes and methods is not handled
 *    properly, because rebinding at the Java level is technically difficult.
 *  </p>
 *
 * @author Moez A. Abdel-Gawad
 * @author Lex Spoon
 */

class Interpreter(val settings: REPLSettings)
            extends Requests
            with LowPriorityInterpreter
            with Javap
            with TypeCache
            with Handlers
            with Binders
            with Power 
            with SymbolizedREPL
            with Naming
            with Classloading
            with Init {
  
  import settings.out
  
  def println(msg: Any) = {
    out.println(msg)
    out.flush()
  }
  def cleanprintln(msg: Any) = println(clean(msg.toString))
  
  /** construct an interpreter that reports to Console */
  def this(settings: Settings, out: PrintWriter) = this(REPLSettings(settings, out))
  def this(settings: Settings) = this(REPLSettings(settings))
  def this() = this(REPLSettings())
  
  /** directory to save .class files to */
  val virtualDirectory = new VirtualDirectory("(memory)", None)

  /** whether to print out result lines */
  private[nsc] var printResults: Boolean = true

  /** reporter */
  lazy val reporter: Reporter =
    new ConsoleReporter(settings, null, out) {
      override def printMessage(msg: String) = 
        if (printResults)
          cleanprintln(msg)
    }

  import compiler._

  /** Temporarily be quiet */
  def quietly[T](operation: => T): T = {    
    val wasPrinting = printResults    
    ultimately(printResults = wasPrinting) {
      printResults = false
      operation
    }
  }
  
  /** Reset reporter, run body, report success or failure. */
  def withReporter[T](body: => T): (T, Boolean) = {
    reporter.reset
    val result = body
    (result, !reporter.hasErrors)
  }

  /** Returns the result of the block along with whatever new files
   *  were created in the virtual directory while it ran.
   */
  def trackingFiles[T](body: => T): (T, Set[AbstractFile]) = {
    val files1 = virtualDirectory.toSet
    val result = body
    val files2 = virtualDirectory.toSet
    
    (result, files2 -- files1)
  }
  
  /** Convenience to do them together. */
  def compileWithWrappers[T](body: => T): (T, Set[AbstractFile], Boolean) = {
    val ((result, files), success) = {
      withReporter {
        trackingFiles {
          body
        }
      }
    }
    
    (result, files, success)
  }

  /** whether to bind the lastException variable */
  private[nsc] var bindLastException = true
  
  /** Temporarily stop binding lastException */
  def withoutBindingLastException[T](operation: => T): T = {
    val wasBinding = bindLastException
    ultimately(bindLastException = wasBinding) {
      bindLastException = false
      operation
    }
  }

  /** Instantiate a compiler.  Subclasses can override this to
   *  change the compiler class used by this interpreter. */
  protected def newCompiler(settings: Settings, reporter: Reporter) = {
    settings.outputDirs setSingleOutput virtualDirectory    
    new Global(settings, reporter)
  }
  
  /** the compiler's classpath, as URL's */
  lazy val compilerClasspath: List[URL] = new PathResolver(settings) asURLs

  /** Truncate a string if it is longer than isettings.maxPrintString */
  def truncPrintString(str: String, max: Int = settings.maxPrintString.value) =
    Strings.truncPrintString(str, max)

  /** Clean up a string for output */  
  def clean(str: String) = truncPrintString(
    if (settings.rawStrings.value) str
    else stripWrapperGunk(str)
  )

  /** Indent some code by the width of the scala> prompt.
   *  This way, compiler error messages read better.
   */
  private final val spaces = List.fill(7)(" ").mkString
  def indentCode(code: String) = {
    /** Heuristic to avoid indenting and thereby corrupting """-strings and XML literals. */
    val noIndent = (code contains "\n") && (List("\"\"\"", "</", "/>") exists (code contains _))
    stringFromWriter(str =>
      for (line <- code.lines) {
        if (!noIndent)
          str.print(spaces)

        str.print(line + "\n")
        str.flush()
      })
  }
  def indentString(s: String) = s split "\n" map (spaces + _ + "\n") mkString

  /** Parse a line into a sequence of trees. Returns None if the input is incomplete. */
  def parse(line: String): Option[List[Tree]] = {
    var justNeedsMore = false
    reporter.withIncompleteHandler((_, _) => justNeedsMore = true) {
      // simple parse: just parse it, nothing else
      val (trees, isSuccess) = withReporter {
        val unit = new CompilationUnit(new BatchSourceFile("<console>", line))
        val scanner = new compiler.syntaxAnalyzer.UnitParser(unit)
        
        scanner.templateStatSeq(false)._2
      }
      
      if (!isSuccess)         Some(Nil)  // the result did not parse, so stop
      else if (justNeedsMore) None
      else                    Some(trees)
    }
  }

  /** Compile an nsc SourceFile.  Returns true if there are
   *  no compilation errors, or false otherwise.
   */
  def compileSources(sources: SourceFile*): Boolean =
    withReporter(new compiler.Run() compileSources sources.toList)._2

  /** Compile a string.  Returns true if there are no
   *  compilation errors, or false otherwise.
   */
  def compileString(code: String): Boolean =
    compileSources(new BatchSourceFile("<script>", code))
  def compileAndSaveRun(label: String, code: String) = {
    if (code contains "// show") {
      DBG(Strings.ppsrc(code))
      parse(code) match {
        case Some(trees)  => trees foreach (t => DBG(compiler.asCompactString(t)))
        case _            => DBG("Parse error:\n\n" + code)
      }
    }
    returning(new compiler.Run())(_.compileSources(List(new BatchSourceFile(label, code))))
  }
  
  def loadAndRunReq(req: Req, synthetic: Boolean) = {
    val (result, succeeded) = req.loadAndRun    
    if (printResults || !succeeded)
      out print clean(result)
    
    if (succeeded) IR.Success
    else IR.Error
  }

  /** Interpret one line of input.  All feedback, including parse errors
   *  and evaluation results, are printed via the supplied compiler's 
   *  reporter.  Values defined are available for future interpreted strings.
   *  The return value is whether the line was interpreted successfully
   *  (that there were no parse or other errors.)
   */
  def interpret(line: String): IR.Result = interpret(line, false)
  def interpret(line: String, synthetic: Boolean): IR.Result = {    
    if (compiler == null) IR.Error
    else Req.fromLine(line, line, synthetic) match {
      case Left(result) => result
      case Right(req)   => 
        // null indicates a disallowed statement type; otherwise compile and
        // fail if false (implying e.g. a type error)
        if (req == null || !req.isSuccess) IR.Error
        else loadAndRunReq(req, synthetic)
    }
  }
  def quietlyRun(line: String) = quietly(interpret(line))

  /** Reset this interpreter, forgetting all user-specified requests. */
  def reset() {
    virtualDirectory.clear
    resetClassLoader()
    reqHistory.clear
    allCreators foreach (_.reset())
  }

  /** This instance is no longer needed, so release any resources
   *  it is using.  The reporter's output gets flushed.
   */
  def close() {
    reporter.flush
  }

  /** Returns the name of the most recent interpreter result.
   *  Mostly this exists so you can conveniently invoke methods on
   *  the previous result.
   */
  def latestVar: Option[Name] =
    if (latestTree.isEmpty) None
    else latestTree.get match {
      case x: ValOrDefDef           => Some(x.name)
      case Assign(Ident(name), _)   => Some(name)
      case ModuleDef(_, name, _)    => Some(name)
      case _                        => Option(varNameCreator.mostRecent) map newTermName
    }

  def typeLine(line: String): Option[Type] = {
    val lhs = getSynthVarName
    quietlyRun("val %s = { %s }".format(lhs, line))
    tpeOf(newTermName(lhs))
  }
  def typeTerm(line: String) = typeLine(line) map (_.termSymbol)
  def typeType(line: String) = typeLine(line)

  def moduleOr[T](id: String, f: Symbol => T): Option[T] =
    try Some(f(definitions.getModule(newTermName(id))))
    catch { case _: MissingRequirementError => None }
  
  def nameOrElse[T](id: String, f: Name => Option[T], fail: => T): T =
    f(newTermName(id)) orElse f(newTypeName(id)) getOrElse fail
  
  def nameOr[T](id: String, f: Name => Option[T]): Option[T] =
    f(newTermName(id)) orElse f(newTypeName(id))
  
  def stringToTreeOpt(id: String)   = nameOr(id, x => treeOf(x) orElse { parse(x) collect { case List(x) => x } })
  def stringToSymbolOpt(id: String) = nameOr(id, x => symOf(x) orElse typeTerm(x) orElse moduleOr(id, x => x))
  def stringToTypeOpt(id: String)   = nameOr(id, x => tpeOf(x) orElse typeType(x))

  def stringToResult(id: String): Option[Any] =
    reqOf(newTermName(id)) flatMap (_.resultValue)

  /** Another entry point for tab-completion, ids in scope */
  private def termFilter(name: Name) = !isSynthVarName(name) && name.isTermName
  private def allTermNames = flatCollect(allHandlers) {
    case x: AssignHandler => List(x.helperName)
    case x: ValHandler    => List(x.vname)
    case x: ModuleHandler => List(x.name)
    case x: DefHandler    => List(x.name)
    case x: ImportHandler => x.importedNames
  } 
  def termNames() = (allTermNames filter termFilter).distinct.sorted
  
  /** Types which have been wildcard imported, such as:
   *    val x = "abc" ; import x._  // type java.lang.String
   *    import java.lang.String._   // object java.lang.String
   *
   *  Used by tab completion.
   *
   *  XXX right now this gets import x._ and import java.lang.String._,
   *  but doesn't figure out import String._.  There's a lot of ad hoc
   *  scope twiddling which should be swept away in favor of digging
   *  into the compiler scopes.
   */
  def wildcardImportedTypes(): List[Type] = {
    val xs = allHandlers collect { case x: ImportHandler if x.importsWildcard => x.targetType }
    xs.flatten.reverse.distinct
  }
  
  // debugging
  def isReplDebug = settings.Yrepldebug.value
  def isCompletionDebug = settings.Ycompletion.value
  def DBG(s: String) = if (isReplDebug) cleanprintln(s)
}

object Interpreter {
  /** A result from interpreting one line of input. */
  sealed abstract class Result

  /** The line was interpreted successfully. */
  case object Success extends Result { }

  /** The line was erroneous in some way. */
  case object Error extends Result { }

  /** The input was incomplete.  The caller should request more input. */
  case object Incomplete extends Result { }
}
