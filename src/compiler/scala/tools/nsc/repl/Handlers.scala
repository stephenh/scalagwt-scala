/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package repl

import PartialFunction._
import symtab.Flags
import util.Chars
import Handlers._

trait Handlers {
  self: Interpreter =>
  
  import compiler._
  import nme.USCOREkw
  
  trait MemberHandlerInterface {
    def member: Tree
    
    def declaredNames: List[Name]
    def referencedNames: List[Name]
    def resultName: Option[Name]
    def isSynthetic = resultName exists isSynthVarName

    def definesImplicit: Boolean
  }
  
  /* private[nsc] */object MemberHandler {
    def apply(member: Tree): MemberHandler = member match {
      case member: DefDef               => new DefHandler(member)
      case member: ValDef               => new ValHandler(member)
      case member@Assign(Ident(_), _)   => new AssignHandler(member)
      case member: ModuleDef            => new ModuleHandler(member)
      case member: ClassDef             => new ClassHandler(member)
      case member: TypeDef              => new TypeAliasHandler(member)
      case member: Import               => new ImportHandler(member)
      case DocDef(_, documented)        => apply(documented)
      case _                            => new GenericHandler(member)
    }
  }

  /** Class to handle one member among all the members included
   *  in a single interpreter request.
   */
  /* private[nsc] */sealed abstract class MemberHandler(val member: Tree) extends MemberHandlerInterface {
    def memberDef: Option[MemberDef] = Some(member) collect { case x: MemberDef => x }
      
    lazy val referencedNames: List[Name] = new ImportVarsTraverser apply member
    def declaredNames: List[Name] = Nil
    def resultName: Option[Name] = None
    def definesImplicit = memberDef exists (_.mods.isImplicit)

    def toLocal(name: Name) = nme getterToLocal name
    
    def extraCodeToEvaluate(req: Req, code: PrintWriter) { }
    def resultExtractionCode(req: Req, code: PrintWriter) { }

    private def label = this.getClass.toString split '.' last
    private def defString = if (referencedNames.isEmpty) "" else referencedNames.mkString("defines ", ", ", "")
    private def resString = resultName map (" => " + _) getOrElse ""
    override def toString = "%s(%s)%s".format(label, defString, resString)
  }

  /* private[nsc] */class GenericHandler(member: Tree) extends MemberHandler(member)
  
  /* private[nsc] */class ValHandler(member: ValDef) extends MemberHandler(member) {
    lazy val ValDef(mods, vname, _, _) = member    
    lazy val prettyName = NameTransformer.decode(vname)
    lazy val isLazy = mods hasFlag Flags.LAZY
    
    override lazy val declaredNames = List(vname)
    override def resultName = Some(vname)
    
    override def resultExtractionCode(req: Req, code: PrintWriter) {
      val isInternal = isGeneratedVarName(vname) && req.isUnit(vname)
      if (!mods.isPublic || isInternal) return
      
      lazy val extractor = "scala.runtime.ScalaRunTime.stringOf(%s)".format(req pathTo vname)
      
      // if this is a lazy val we avoid evaluating it here
      val resultString = if (isLazy) codegenln(false, "<lazy>") else extractor
      val codeToPrint =
        """ + "%s: %s = " + %s""".format(prettyName, string2code(req.typeOf(vname)), resultString)
      
      code print codeToPrint
    }
  }

  /* private[nsc] */class DefHandler(defDef: DefDef) extends MemberHandler(defDef) {
    lazy val DefDef(mods, name, _, vparamss, _, _) = defDef
    override lazy val declaredNames = List(name)
    // true if 0-arity
    override def resultName =
      if (vparamss.isEmpty || vparamss.head.isEmpty) Some(name)
      else None

    override def resultExtractionCode(req: Req, code: PrintWriter) =
      if (mods.isPublic) code print codegenln(name, ": ", req.typeOf(name))
  }

  /* private[nsc] */class AssignHandler(member: Assign) extends MemberHandler(member) {
    val lhs = member.lhs.asInstanceOf[Ident] // an unfortunate limitation
    val helperName = newTermName(synthVarNameCreator())
    override def resultName = Some(helperName)
    override lazy val declaredNames = List(helperName)

    override def extraCodeToEvaluate(req: Req, code: PrintWriter) =
      code println """val %s = %s""".format(helperName, lhs)

    /** Print out lhs instead of the generated varName */
    override def resultExtractionCode(req: Req, code: PrintWriter) {
      val lhsType = string2code(req typeOfEnc helperName)
      val res = string2code(req pathTo helperName)
      val codeToPrint = """ + "%s: %s = " + %s + "\n" """.format(lhs, lhsType, res)
          
      code println codeToPrint
    }
  }

  /* private[nsc] */class ModuleHandler(module: ModuleDef) extends MemberHandler(module) {
    lazy val ModuleDef(mods, name, _) = module
    override lazy val declaredNames = List(name)
    override def resultName = Some(name)

    override def resultExtractionCode(req: Req, code: PrintWriter) =
      code println codegenln("defined module ", name)
  }

  /* private[nsc] */class ClassHandler(classdef: ClassDef) extends MemberHandler(classdef) {
    lazy val ClassDef(mods, name, _, _) = classdef
    override lazy val declaredNames = 
      name :: (if (mods.isCase) List(name.toTermName) else Nil)
    
    override def resultExtractionCode(req: Req, code: PrintWriter) =
      code print codegenln("defined %s %s".format(classdef.keyword, name))
  }

  /* private[nsc] */class TypeAliasHandler(typeDef: TypeDef) extends MemberHandler(typeDef) {
    lazy val TypeDef(mods, name, _, _) = typeDef
    def isAlias() = mods.isPublic && compiler.treeInfo.isAliasTypeDef(typeDef)
    override lazy val declaredNames = if (isAlias) List(name) else Nil

    override def resultExtractionCode(req: Req, code: PrintWriter) =
      code println codegenln("defined type alias ", name)
  }
  
  /** A traverser that finds all mentioned identifiers, i.e. things
   *  that need to be imported.  It might return extra names.
   */
  /* private[nsc] */class ImportVarsTraverser extends Traverser {
    private val importVars = new ListBuffer[Name]()
    // this is inadequate (it's to avoid accidentally importing
    // local symbols) but seems to do for now.
    def importOK(name: Name) = !(name.toString startsWith "x$")

    override def traverse(tree: Tree) = tree match {
      case Ident(name) if importOK(name)  => importVars += name
      case _                              => super.traverse(tree)
    }
    
    def apply(tree: Tree) = {
      traverse(tree)
      importVars.distinct.toList
    } 
  }

  /* private[nsc] */class ImportHandler(imp: Import) extends MemberHandler(imp) {
    lazy val Import(expr, selectors) = imp
    def targetType = stringToTypeOpt(expr.toString)
    
    private def selectorWild    = selectors filter (_.name == USCOREkw)   // wildcard imports, e.g. import foo._
    private def selectorMasked  = selectors filter (_.rename == USCOREkw) // masking imports, e.g. import foo.{ bar => _ }      
    private def selectorNames   = selectors map (_.name)
    private def selectorRenames = selectors map (_.rename) filterNot (_ == null)
    
    /** Whether this import includes a wildcard import */
    val importsWildcard = selectorWild.nonEmpty
    
    /** Complete list of names imported by a wildcard */
    def wildcardImportedNames: List[Name] = (
      for (tpe <- targetType ; if importsWildcard) yield
        tpe.nonPrivateMembers filter (x => x.isMethod && x.isPublic) map (_.name) distinct
    ).toList.flatten

    /** The individual names imported by this statement */
    /** XXX come back to this and see what can be done with wildcards now that
     *  we know how to enumerate the identifiers.
     */
    val importedNames: List[Name] =
      selectorRenames filterNot (_ == USCOREkw) flatMap (x => List(x.toTypeName, x.toTermName))
    
    override def resultExtractionCode(req: Req, code: PrintWriter) =
      code println codegenln(imp.toString)
  }
}

object Handlers {
  def codegenln(leadingPlus: Boolean, xs: String*): String = codegen(leadingPlus, (xs ++ Array("\n")): _*)
  def codegenln(xs: String*): String = codegenln(true, xs: _*)

  def codegen(xs: String*): String = codegen(true, xs: _*)
  def codegen(leadingPlus: Boolean, xs: String*): String = {
    val front = if (leadingPlus) "+ " else ""
    front + (xs map string2codeQuoted mkString " + ")
  }
  
  def string2codeQuoted(str: String) = "\"" + string2code(str) + "\""

  /** Convert a string into code that can recreate the string.
   *  This requires replacing all special characters by escape
   *  codes. It does not add the surrounding " marks.  */
  def string2code(str: String): String = {    
    val res = new StringBuilder
    for (c <- str) c match {
      case '"' | '\'' | '\\'  => res += '\\' ; res += c
      case _ if c.isControl   => res ++= Chars.char2uescape(c)
      case _                  => res += c
    }
    res.toString
  }
}