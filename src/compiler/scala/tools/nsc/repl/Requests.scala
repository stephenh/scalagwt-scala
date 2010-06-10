/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import java.lang.reflect.InvocationTargetException
import util.{ returning, stringFromWriter }
import scala.util.control.Exception.{ Catcher, catching, unwrapping }
import Strings._

trait Requests extends Imports {
  self: Interpreter =>
  
  import compiler._
  import Predef.error
  
  abstract class SourceCodeUnit(val objectName: String) {
    def handlers: List[MemberHandler]
    def onSuccess(): Unit
    def preamble: String
    def postamble: String
    def codeFn(handler: MemberHandler, pw: PrintWriter): Unit

    def genSource(): String = stringFromWriter { pw =>
      pw println preamble
      handlers foreach (h => codeFn(h, pw))
      pw println postamble
    }
    
    // compile the object containing the code
    lazy val (run, files) = trackingFiles(compileAndSaveRun("<console>", genSource()))

    // compile the main object
    lazy val (_, success) = withReporter(run)

    // if compilation succeeds, execute onSuccess()
    lazy val compile = returning(success)(x => if (x) onSuccess())
  }
  
  trait CompilerLogic {
    self: Req => 

    // The types are all =>T; remove the =>
    def depoly(tpe: Type) = tpe match { 
      case PolyType(Nil, rt)  => rt
      case _                  => tpe
    }
    def atTyper[T](op: => T): T = atPhase(objectRun.typerPhase.next)(op)
    def memberAtTyper[T](sym: Symbol, name: Name) = atTyper(sym.info member name)
  
    // Given a path like a.b.c, returns a list of Symbols: List(a, b, c)
    def pathSymbols(outerName: String, path: String): List[Symbol] = {
      val outer     = definitions.getMember(definitions.EmptyPackage, newTermName(outerName))
      val segments  = accessPath split '.' toList ;
      def follow(sym: Symbol, name: String) =
        if (name == "") sym
        else memberAtTyper(sym, newTermName(name))

      segments.scanLeft(outer)(follow)
    }
    
    def wrapperChain = pathSymbols(srcObjectName, accessPath)
  }
  trait ReqLogic extends CompilerLogic {
    self: Req =>

    /** load and run the code using reflection */
    def loadAndRun: (String, Boolean) = {
      val wrapperExceptions: List[Class[_ <: Throwable]] =
        List(classOf[InvocationTargetException], classOf[ExceptionInInitializerError])
      
      /** We turn off the binding to accomodate ticket #2817 */
      def onErr: Catcher[(String, Boolean)] = {
        case t: Throwable if bindLastException =>
          withoutBindingLastException {
            quietlyBind("lastException", "java.lang.Throwable", t)
            (stringFromWriter(t.printStackTrace(_)), false)
          }
      }
      
      catching(onErr) {
        unwrapping(wrapperExceptions: _*) {
          (resultObjMethod.invoke(resultValue).toString, true)
        }
      }
    }
    
    // Generate source for the object that computes this request
    object ObjectSourceCodeUnit extends SourceCodeUnit(srcObjectName) {
      val handlers = self.handlers
      val preamble = """
        |object %s {
        |  %s%s
      """.stripMargin.format(srcObjectName, importsPreamble, indentCode(line))     
      val postamble = importsTrailer + "\n}"
      
      def codeFn(handler: MemberHandler, pw: PrintWriter) = handler.extraCodeToEvaluate(self, pw)
      def onSuccess(): Unit = {
        // extract and remember symbols and types
        decls
        // compile the result-extraction object
        resultRun
      }
    }

    // Generate source code for the object that retrieves the result
    object ResultExtractionSourceCodeUnit extends SourceCodeUnit(resObjectName) {
      val handlers = self.handlers
      private def getter: String = reqResultPath map { path =>
          """
          |lazy val scala_repl_value = {
          |  scala_repl_result
          |  %s
          |}""".stripMargin.format(path)
      } getOrElse ""

      // first line evaluates object to make sure constructor is run
      // initial "" so later code can uniformly be: + etc
      val preamble = """
      |object %s {
      |  %s
      |  val scala_repl_result: String = {
      |    %s
      |    (""
      """.stripMargin.format(resObjectName, getter, srcObjectName + accessPath)

      val postamble = """
      |    )
      |  }
      |}
      """.stripMargin
      
      def codeFn(handler: MemberHandler, pw: PrintWriter) = handler.resultExtractionCode(self, pw)
      def onSuccess(): Unit = ()
    }

    // compile the object containing the user's code
    lazy val objectRun = ObjectSourceCodeUnit.run
    lazy val objectGeneratedFiles = ObjectSourceCodeUnit.files

    // compile the object which retrieves the result
    lazy val resultRun = ResultExtractionSourceCodeUnit.run
    lazy val resultGeneratedFiles = ResultExtractionSourceCodeUnit.files
    
    private lazy val compile = returning(ObjectSourceCodeUnit.compile)(res => if (res) recordReq(self))
    def isSuccess = compile

    def loadResultValue(): Option[Any] = {
      // ensure it has run
      resultRun
      
      // load it and retrieve the value        
      try Some(resultClazz getMethod "scala_repl_value" invoke resultClazz)
      catch { case _: Exception => None }
    }
  }
  
  /** Build a request from the user.  "trees" is the the code in "line" after being parsed.
   */  
  object Req {
    def fromLine(line: String): Either[IR.Result, Req] = fromLine(line, false)
    def fromLine(line: String, isSynthetic: Boolean): Either[IR.Result, Req] = fromLine(line, line, isSynthetic)
    def fromLine(orig: String, line: String, isSynthetic: Boolean): Either[IR.Result, Req] = {
      val trees = parse(indentCode(line)) match {
        case None         => return Left(IR.Incomplete)
        case Some(Nil)    => return Left(IR.Error) // parse error or empty input
        case Some(trees)  => trees
      }

      // use synthetic vars to avoid filling up the resXX slots
      def varName = if (isSynthetic) getSynthVarName else getVarName

      // Treat a single bare expression specially. This is necessary due to it being hard to
      // modify code at a textual level, and it being hard to submit an AST to the compiler.
      if (trees.size == 1) trees.head match {
        case _:Assign                         => // we don't want to include assignments
        case _:TermTree | _:Ident | _:Select  => // ... but do want these as valdefs.
          return fromLine(orig, "val %s =\n%s".format(varName, line), isSynthetic)
        case _                                =>
      }

      // figure out what kind of request
      Right(apply(orig, line, trees))
    }

    def apply(orig: String, line: String, trees: List[Tree]): Req = new Req(orig, line, trees)
  }
    
  /** One line of code submitted by the user for interpretation */
  class Req(val orig: String, val line: String, val trees: List[Tree]) extends ReqLogic {
    val id: Int         = getRequestId()
    
    val lineName        = nme.INTERPRETER_LINE_PREFIX + id                    // unique prefix for this line
    val srcObjectName   = lineName + nme.INTERPRETER_WRAPPER_SUFFIX           // name of object which computes "line"
    val resObjectName   = "RequestResult$" + srcObjectName                    // name of object which retrieves result from srcObject
    val prefix          = virtualDirectory.name + "/" + srcObjectName + "$"   // prefix of all files generated by this compile
    val handlers        = trees map (x => MemberHandler(x))                   // one handler per top level tree

    def declaredNames   = handlers flatMap (_.declaredNames)                  // all public top-level names
    def referencedNames = handlers flatMap (_.referencedNames)                // all non-local names referenced from "line"
    def isSynthetic     = handlers exists (_.isSynthetic)                     // if this request is synthetic
    def reqResult       = handlers.last.resultName
    def hasResult       = reqResult.isDefined
    def reqResultPath   = reqResult filter (names contains _) map pathTo
    
    val prevWhich       = self.prevWhich(this) _

    /** Code to import bound names from previous lines - accessPath is code to
      * append to srcObjectName to access anything bound by request.
      */
    val ComputedImports(importsPreamble, importsTrailer, accessPath) = importsCode(referencedNames.toSet)

    def pathTo(name: Name): String    = pathTo(name.toString)
    def pathTo(name: String): String  = "%s.`%s`".format(srcObjectName + accessPath, name)
    def isUnit(name: Name)            = typeOfEnc(name) == "Unit"
    
    case class Decl(name: Name, tree: Tree, sym: Symbol, tpe: Type) {
      def req       = Req.this
      def isTerm    = name.isTermName
      def isType    = name.isTypeName
      def isModule  = sym.isModule
      def isClass   = sym.isClass

      def isCompanionOf(other: Req#Decl) =
        (sym.isClass && other.isModule && name.toTermName == other.name) ||
        (sym.isModule && other.isClass && name.toTypeName == other.name)

      def shadows     = prevWhich(_.name == name)
      def companions  = prevWhich(isCompanionOf)
      
      def typeString      = if (isModule) name + ".type" else quietString(tpe.toString)
      def termString      = if (isModule) sym.toString else name.toString
      def disambiguation  = if (shadows.nonEmpty) "#" + req.id else ""
      
      override def toString =
        if (isModule) termString + disambiguation
        else "%s%s: %s".format(termString, disambiguation, typeString)
    }
  
    def atAllPhases[T](op: => T): List[T] = {
      val allPhases = phaseNames map (objectRun phaseNamed _)
      val res = allPhases map (ph => (ph, atPhase(ph)(op)))
      phasep(res)

      res map (_._2)
    }
    def phformat(ph: Any, output: Any): (String, String) = {
      val o = if (output == null) "null" else output.toString take 240
      ("%15s -> ".format(ph), stripWrapperGunk(o))
    }
    def phasep(x: List[(Phase, _)]) = {
      def to_s(x: (Any, Any)) = "" + x._1 + x._2 + "\n"
      val result: List[(String, String)] = x map (phformat _ tupled)
      val lines = (result sliding 2 toList).foldLeft(to_s(result.head)) {
        case (str, List((p1, s1), (p2, s2))) =>
          if (s1 == s2) str + to_s((p2, "\" \" \""))
          else str + to_s((p2, s2))
      }
      println(lines) 
    }    
      
    def isClassFile(x: AbstractFile) = (x.path startsWith prefix) &&  (x.name endsWith ".class")
    def cleanFiles(xs: List[AbstractFile]) = xs filter isClassFile map (x => stripWrapperGunk(x.path stripPrefix prefix))
    
    lazy val resultClazz  =
      loadByName(resObjectName)

    lazy val resultValue  =
      if (isSuccess) loadResultValue()
      else null
    
    lazy val resultObjMethod =
      if (isSuccess) resultClazz getMethod "scala_repl_result"
      else null
    
    lazy val decls: List[Req#Decl] =
      for (handler <- handlers ; name <- handler.declaredNames) yield {
        def memberSym = wrapperChain.last.info member name
        val sym = atTyper(memberSym)
        val tpe = atTyper(memberSym.tpe)

        Decl(name, handler.member, sym, depoly(tpe))
      }
    
    /** **/
    def syms    = decls map (_.sym)
    def names   = decls map (_.name)
    def types   = decls map (_.tpe)

    def shadows     = decls collect { case x if x.shadows.nonEmpty => (x, x.shadows) }
    def companions  = decls collect { case x if x.companions.nonEmpty => (x, x.companions) }

    type DeclRef = (Req#Decl, List[Req#Decl])
    def ppDeclRef(msg: String, pair: DeclRef): String = {
      val (x, xs) = pair
      
      "%s %s %d: latest is #%s".format(x.name, msg, xs.size, xs.head.req.id)
    }

    def declNamed(name: Name)         = decls find (_.name == name) getOrElse error("No decl named '%s'".format(name))
    def typeOf(name: Name): String    = atTyper(declNamed(name).tpe.toString)    
    def typeOfEnc(name: Name)         = typeOf(compiler encode name)

    private def declaresString =
      if (decls.isEmpty) ""
      else syms.mkString("(decls: ", ", ", ")")
    
    private def referencedString =
      if (referencedNames.isEmpty) ""
      else referencedNames.distinct.sorted.mkString("(refs: ", ", ", ")")
    
    private def shadowsString =      
      if (shadows.isEmpty) ""
      else shadows map (x => ppDeclRef("shadows", x)) mkString ("(", ", ", ")")
    
    private def companionString =
      if (companions.isEmpty) ""
      else companions map (x => ppDeclRef("companion for", x)) mkString ("(", ", ", ")")
    
    private def idString = "#" + id
      
    override def toString = {
      val line1 = idString + " \"" + shortCode(line) + "\""
      val line2 = List[String](
        declaresString,
        referencedString,
        shadowsString,
        companionString
      ) filterNot (_ == "") mkString " "
      
      line1 + "\n  " + stripWrapperGunk(line2)
    }
  }
}