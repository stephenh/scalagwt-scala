/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import scala.reflect.Manifest

trait Power {
  repl: Interpreter =>
  
  import compiler._

  abstract class Eval[+T] {
    def result: T
    def toOption: Option[T]
  }
  case class Success[+T](result: T) extends Eval[T] {
    def toOption = Some(result)
  }
  case class Failure[+T](ex: Throwable) extends Eval[T] {
    def result = throw ex
    def toOption = None
  }
  
  def completionAware(id: String) = stringToResult(id) collect { case x: CompletionAware => x }    

  case class EvalInferenceError(msg: String, value: Any) extends Exception(msg) { }
  case class EvalParseError(msg: String) extends Exception(msg) { }
  case class EvalCompileError(msg: String) extends Exception(msg) { }
  case class EvalTypeError(msg: String, value: Any) extends Exception(msg) { }  
  
  /** The user-facing eval in :power mode wraps an Option.
   */
  def eval[T: Manifest](line: String): Eval[T] = {
    val lhs = getSynthVarName
    quietlyRun("val %s = { %s }".format(lhs, line))
    val lhsType = tpeOf(newTermName(lhs))    
    def lhsMessage = lhsType map (_.toString) getOrElse "??"

    val req = Req.fromLine(line, lhs, true) match {
      case Left(result) => return Failure(EvalParseError(result.toString))
      case Right(req)   => req
    }
    if (req == null || !req.isSuccess || req.handlers.size != 1)
      return Failure(EvalCompileError("Error."))
      
    val value = req.resultValue getOrElse {
      println("No resultValue for " + req)
      return Failure(EvalCompileError("Error."))
    }
    
    if (manifest[T] eq Manifest.Nothing)
      return Failure(EvalInferenceError("Could not infer type: it's a " + lhsMessage, value))
    
    try Success(value.asInstanceOf[T])
    catch { case e: Exception => Failure(EvalTypeError(lhsMessage + " is not a " + manifest[T] + "!", value)) }
  }
  def evalExpr[T: Manifest](line: String): T = eval[T](line).result

  def quietlyEval[T: Manifest](code: String): Option[T] = quietly[Option[T]](eval[T](code).toOption)

  /** A container class for methods to be injected into the repl
   *  in power mode.
   */
  object power {
    lazy val compiler: repl.compiler.type = repl.compiler
    import compiler.{ phaseNames, atPhase, currentRun }
    
    class PowerTraverser[T](pf: PartialFunction[Tree, T]) extends Traverser {
      val hits = new ListBuffer[T]
      
      override def traverse(tree: Tree) = {
        if (pf.isDefinedAt(tree))
          hits += pf(tree)
        
        super.traverse(tree)
      }
      
      def apply(tree: Tree) = {
        hits.clear()
        traverse(tree)
        hits.toList
      }
    }
    def mkTraverser[T](pf: PartialFunction[Tree, T]) = new PowerTraverser(pf)
    
    def mkContext(code: String = "") = compiler.analyzer.rootContext(mkUnit(code))
    def mkAlias(name: String, what: String) = interpret("type %s = %s".format(name, what))
    def mkSourceFile(code: String) = new BatchSourceFile("<console>", code)
    def mkUnit(code: String) = new CompilationUnit(mkSourceFile(code))

    def mkTree(code: String): Tree = mkTrees(code).headOption getOrElse EmptyTree
    def mkTrees(code: String): List[Tree] = parse(code) getOrElse Nil
    def mkTypedTrees(code: String*): List[compiler.Tree] = {
      class TyperRun extends compiler.Run {
        override def stopPhase(name: String) = name == "superaccessors"
      }

      reporter.reset
      val run = new TyperRun
      run compileSources (code.toList.zipWithIndex map {
        case (s, i) => new BatchSourceFile("<console %d>".format(i), s)
      })
      run.units.toList map (_.body)
    }
    def mkTypedTree(code: String) = mkTypedTrees(code).head
    def mkType(id: String): compiler.Type = stringToTypeOpt(id).get
    
    lazy val allPhases: List[Phase] = phaseNames map (currentRun phaseNamed _)
    def atAllPhases[T](op: => T): List[(String, T)] = allPhases map (ph => (ph.name, atPhase(ph)(op)))
    def showAtAllPhases(op: => Any): Unit =
      atAllPhases(op.toString) foreach { case (ph, op) => Console.println("%15s -> %s".format(ph, op take 240)) }
  }
  
  def unleash(): Unit = quietly {
    interpret("import scala.tools.nsc._")
    interpret("val global: repl.compiler.type = repl.compiler")
    interpret("val power: repl.power.type = repl.power")
  }
}