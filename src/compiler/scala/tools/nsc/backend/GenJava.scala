/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 */

// $Id$

package scala.tools.nsc.backend

import java.io.{File, FileOutputStream, PrintWriter, IOException}

import symtab.Flags._

import scala.collection.mutable.ListBuffer

//import scala.collection.immutable.{Set, ListSet}
//import scala.collection.mutable.{Map, HashMap}
//import scala.tools.nsc.symtab._
//import scala.tools.nsc.util.Position

/** Generates code in the form of Java source
 *
 *  @author  Nikolay Mihaylov
 *  @version 1.0
 */
abstract class GenJava extends SubComponent {
  import global._
  import scalaPrimitives._

  val phaseName = "java"

  /** Create a new phase */
  override def newPhase(p: Phase) = new JavaPhase(p)

  /** JVM code generation phase
   */
  final class JavaPhase(prev: Phase) extends StdPhase(prev) {

    override def run: Unit = {
      scalaPrimitives.init
      super.run
    }

    def apply(unit: CompilationUnit): Unit =
      gen(unit.body)

    var pkgName: String = _

    private def gen(tree: Tree): Unit = tree match {
      case EmptyTree => ()
      case PackageDef(packaged, stats) =>
        if (packaged != nme.EMPTY_PACKAGE_NAME) pkgName = tree.symbol.fullNameString
        stats foreach gen
        pkgName = null
      case ClassDef(mods, name, tparams, impl) => {
        val clazz = tree.symbol
        val file = getFile(clazz, ".java")
        try {
          val out = new PrintWriter(new FileOutputStream(file))
          val printer = new JavaPrinter(out)
          if (pkgName != null) {
            printer.print("package ")
            printer.print(pkgName)
            printer.print(";")
            printer.println
          }
          printer.print(tree)
          if (!clazz.isNestedClass)
            dumpMirrorClass(printer)(clazz)
          out.close()
          currentRun.symData -= clazz
          currentRun.symData -= clazz.linkedSym
        } catch {
          case ex: IOException =>
            if (settings.debug.value) ex.printStackTrace()
          error("could not write file " + file)
        }
      }
    }

    def isStaticSymbol(s: Symbol): Boolean =
      s.hasFlag(STATIC) || s.hasFlag(STATICMEMBER) || s.owner.isImplClass;

    def dumpMirrorClass(printer: JavaPrinter)(clazz: Symbol): Unit = {
      import printer.{print, println, printType, indent, undent}

      print("package"); print(pkgName); print(";"); println
      print("public final class "); print(javaName(clazz));
      print("{"); indent; println
      for (val m <- clazz.tpe.nonPrivateMembers;
           m.owner != definitions.ObjectClass && !m.hasFlag(PROTECTED) &&
           m.isMethod && !m.hasFlag(CASE) && !m.isConstructor && !isStaticSymbol(m) )
      {
        print("public final static "); printType(m.tpe.resultType); print(" ");
        print(m.name); print("(");
        val paramTypes = m.tpe.paramTypes
        for (val i <- 0 until paramTypes.length) {
          if (i > 0) print(", ");
          printType(paramTypes(i)); print(" x_" + i)
        }
        print(") { return ");

        print(javaName(clazz)); print("."); print(nme.MODULE_INSTANCE_FIELD); print("(");
        for (val i <- 0 until paramTypes.length) {
          if (i > 0) print(", ");
          print("x_" + i)
        }
        print("); }");
      }
      undent; println; print("}"); println
    }

  }

  private final class JavaPrinter(out: PrintWriter) extends treePrinters.TreePrinter(out) {

    override def printRaw(tree: Tree): Unit = printRaw(tree, false)

    def printRaw(tree: Tree, ret: Boolean): Unit = tree match {
      case EmptyTree => ;
      case ClassDef(mods, name, _, Template(superclass :: ifaces, _, body)) =>
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        def nameSuffix = if (tree.symbol.isModuleClass) "$" else ""
        print((if (mods hasFlag TRAIT) "interface " else "class ") + symName(tree, name) + nameSuffix)
        print(" extends ")
        print(superclass)
        if (!ifaces.isEmpty) {
          print(" implements ")
          printRow(ifaces, ", ")
        }
        printColumn(body, " {", "", "}")
        println

      case ValDef(mods, name, tp, rhs) =>
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        printType(tp.tpe)
        print(" ")
        print(symName(tree, name))
        if (!rhs.isEmpty) { print(" = "); print(rhs) }
        if (tree.symbol.owner.isClass) print(";")

      case DefDef(mods, name, tparams, vparamss, tp, rhs) =>
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        if (name == nme.CONSTRUCTOR) print(tree.symbol.owner.name)
        else {
          printType(tp.tpe)
          print(" ")
          print(symName(tree, name))
        }
        vparamss foreach printValueParams
        if (rhs.isEmpty) {
          print(";")
        }
        else {
          rhs match {
            case Block(_, _) => printRaw(rhs, true)
            case _ =>
              print(" {"); indent; println
              if (rhs.tpe.typeSymbol != definitions.UnitClass) print("return ");
              print(rhs); print(";");
              undent; println; print("}")
          }
        }

      case Block(stats, expr) =>
        print("{");indent; println
        printSeq(stats) {s => print(s); print(";")} {println}
        println
        expr match {
          case Block(_, _) if (ret) => printRaw(expr, ret)
          case Literal(Constant(())) if (ret) => ;
          case _ if (expr.tpe.typeSymbol == definitions.UnitClass) => print(expr); print(";")
          case _ if (ret) => print("return "); print(expr); print(";")
          case _ => print(expr); print(";")
        }
        undent; println; print("}")

      case LabelDef(lname1, List(), If(cond,
                                       Block(List(body), Apply(Ident(lname2), List())),
                                       Literal(Constant(()))))
      if (lname1.startsWith("while") && lname1 == lname2) =>
      print("while ("); print(cond); print(") {");
      indent; println; print(body); undent; println; print("}");

      case Apply(t @ Select(New(tpt), nme.CONSTRUCTOR), args) if (tpt.tpe.typeSymbol == definitions.ArrayClass) =>
        tpt.tpe match {
          case TypeRef(_, _, List(elemType)) =>
            print("new "); printType(elemType)
            print("["); print(args.head); print("]")
        }

      case Apply(fun @ Select(receiver, name), args) if isPrimitive(fun.symbol) =>
        getPrimitive(fun.symbol) match {
          case POS | NEG | NOT | ZNOT =>
            print(name.decode); print("("); print(receiver); print(")");
          case ADD | SUB | MUL | DIV | MOD | OR | XOR | AND |
               LSL | LSR | ASR |EQ | NE | LT | LE | GT | GE | ZOR | ZAND=>
            print(receiver); print(" "); print(name.decode); print(" "); print(args.head)
          case APPLY => print(receiver); print("["); print(args.head); print("]")
          case UPDATE =>
            print(receiver); print("["); print(args.head); print("] = ");
            print(args.tail.head); print("")
          case SYNCHRONIZED => print("synchronized ("); print(receiver); print(") {");
            indent; println; print(args.head); undent; println; print("}");
          case _ => print("Unhandled primitive: " + tree)
        }

      case This(_) => print("this")

      case Super(_, _) | Select(Super(_, _), nme.CONSTRUCTOR) => print("super")

      case _ => super.printRaw(tree)
    }



    override def printParam(tree: Tree): Unit = tree match {
      case ValDef(mods, name, tp, rhs) =>
        //printAttributes(tree)
        printType(tp.tpe); print(" "); print(symName(tree, name));
    }

    def printFlags(sym: Symbol): Unit = {
      val flags = sym.flags
      val fs = new ListBuffer[String]
      def printFlag(f: Long, s: String) =
        if ((f & flags) != 0)
          fs += s
      if ((flags & (PRIVATE | PROTECTED)) == 0 && sym.owner.isClass)
        fs += "public"
      else {
        printFlag(PRIVATE, "private")
        printFlag(PROTECTED, "protected")
      }
      printFlag(STATIC, "static")
      printFlag(FINAL, "final")
      if ((flags & (DEFERRED | ABSTRACT)) != 0)
        fs += "abstract"
      val flagstr = fs.mkString("", " ", "")
      if (flagstr.length != 0) { print(flagstr); print(" "); }
    }

    def printType(typ: Type): Unit = {
      val tsym = typ.typeSymbol
      if (tsym == definitions.IntClass) print("int")
      else if (tsym == definitions.DoubleClass) print("double")
      else if (tsym == definitions.BooleanClass) print("boolean")
      else if (tsym == definitions.LongClass) print("long")
      else if (tsym == definitions.DoubleClass) print("double")
      else if (tsym == definitions.CharClass) print("char")
      else if (tsym == definitions.ByteClass) print("byte")
      else if (tsym == definitions.FloatClass) print("float")
      else if (tsym == definitions.ShortClass) print("short")
      else if (tsym == definitions.UnitClass) print("void")
      else if (tsym == definitions.AllClass) print("scala.runtime.Nothing$")
      else if (tsym == definitions.AllRefClass) print("scala.runtime.Null$")
      else typ match {
        case TypeRef(_, sym, List(elemtype)) if (sym == definitions.ArrayClass) =>
          printType(elemtype); print("[]")
        case _ => print(typ.toString)
      }
    }
  }

  private def javaName(sym: Symbol): String = {
    def suffix = if (sym.hasFlag(MODULE) && !sym.isMethod &&
                     !sym.isImplClass &&
                     !sym.hasFlag(JAVA)) "$" else "";

    if (sym == definitions.AllClass)
      return "scala.runtime.Nothing$"
    else if (sym == definitions.AllRefClass)
      return "scala.runtime.Null$"

//     if (sym.isClass && !sym.rawowner.isPackageClass)
//       innerClasses = innerClasses + sym;

    (if (sym.isClass || (sym.isModule && !sym.isMethod))
      sym.fullNameString('$')
     else
       sym.simpleName.toString().trim()) + suffix
    }

}

//         case ValDef(mods, name, tpt, rhs) => ;
//         case DefDef(mods, name, tparams, vparamss, tpt, rhs) => ;
//         case LabelDef(name, params, rhs) => ;
//         case Template(parents, body) => ;
//         case Block(stats, expr) => ;
//         case ArrayValue(elemtpt, trees) => ; //                              (introduced by uncurry)
//         case Assign(lhs, rhs) => ;
//         case If(cond, thenp, elsep) => ;
//         case Match(selector, cases) => ;
//         case Return(expr) => ;
//         case Try(block, catches, finalizer) => ;
//         case Throw(expr) => ;
//         case New(tpt) => ;
//         //case TypeApply(fun, args) => ;
//         case Apply(fun, args) => ;
//         case Super(qual, mix) => ;
//         case This(qual) => ;
//         case Select(qualifier, selector) => ;
//         case Ident(name) => ;
//         case Literal(value) => ;
//         case TypeTree() => ;
