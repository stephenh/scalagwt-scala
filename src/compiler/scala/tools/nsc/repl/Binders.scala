/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package repl

import util.returning

trait Binders {
  self: Interpreter =>
  
  import compiler.{ EmptyTree, NoSymbol, NoType }
  
  lazy val replValsBinder = returning(Binder("$replvals")) { binder =>
    binder.add("settings", settings)
    binder.add("repl", self)
    binder.add("global", "repl.compiler.type", compiler)
    binder.add("power", "repl.power.type", power)
    binder.add("reqs", "() => scala.collection.immutable.List[repl.Req]", () => reqHistory.toList)
  }
  
  abstract class BinderCategory[T](prefix: String, typeName: String) extends ((String, => Unit) => T) {  
    private val nameCreator = new NameCreator(prefix)
    private def createBinding(value: T) = bind(nameCreator(), typeName, value)

    def zero: T
    def creator(id: String): Option[T]
    
    def opt(id: String): Option[T] = returning(creator(id))(_ foreach createBinding)
    def apply(id: String, orElse: => Unit): T = opt(id) getOrElse { orElse ; zero }
  }
  object TreeBinder extends BinderCategory[compiler.Tree]("tree", "repl.compiler.Tree") {
    def zero = EmptyTree
    def creator(id: String) = stringToTreeOpt(id)
  }
  object TypeBinder extends BinderCategory[compiler.Type]("tpe", "repl.compiler.Type") {
    def zero = NoType
    def creator(id: String) = stringToTypeOpt(id)
  }
  object SymBinder extends BinderCategory[compiler.Symbol]("sym", "repl.compiler.Symbol") {
    def zero = NoSymbol
    def creator(id: String) = stringToSymbolOpt(id)
  }
  
  /** Bind a specified name to a specified value.  The name may
   *  later be used by expressions passed to interpret.
   *
   *  @param name      the variable name to bind
   *  @param boundType the type of the variable, as a string
   *  @param value     the object value to bind to it
   *  @return          an indication of whether the binding succeeded
   */
  def bind(name: String, boundType: String, value: Any): IR.Result = {    
    val slot  = BindSlot(boundType)
    val b     = Binder(slot)
    
    if (b.compile) {
      b.set(slot.slotName, value)
      interpret("val %s = %s".format(name, b.path(slot.slotName)))
    }
    else IR.Error
  }

  def bindT[T: OptManifest](name: String, value: T): Unit =
    bind(name, typeString(value), value)

  def quietlyBind(name: String, boundType: String, value: Any): IR.Result = 
    quietly { bind(name, boundType, value) }

  /** Creates a special $replvals object and binds various objects
   *  inside of it for easy access.
   */
  def injectReplVal[T: Manifest](name: String, value: T): Unit =
    replValsBinder.add[T](name, value)

  def injectReplVal(name: String, typeString: String, value: Any): Unit =
    replValsBinder.add(name, typeString, value)
  
  /** XXX kind of hacky, the import should work.
   */
  def compileReplVals(): Unit = {
    if (!replValsBinder.compile) DBG("Failed to compile $replvals")
    else replValsBinder.names foreach { name =>
      quietlyRun("val %s = %s.%s".format(name, replValsBinder.objectName, name))
    }
  }

  object BindSlot {
    val defaultSlotName = "value"
    def apply(tpe: String): BindSlot = new BindSlot(defaultSlotName, tpe)
    def apply(slotName: String, tpe: String): BindSlot = new BindSlot(slotName, tpe)
  }
  class BindSlot(val slotName: String, val tpe: String) {
    def setterName = "set_" + slotName
    
    private val template = """
      |var _@@NAME@@: @@TYPE@@ = _
      |def @@SETTERNAME@@(x: Any) = _@@NAME@@ = x.asInstanceOf[@@TYPE@@]
      |lazy val @@NAME@@: @@TYPE@@ = _@@NAME@@
    """.stripMargin

    private def saveDollars(s: String)  = s.replaceAll("""\$""", """\\\$""")
    private def dotDollars(s: String)   = s.replaceAll("""\Q$$\E""", """.\$""")
    private def interpolate(replacements: (String, String)*): String =
      replacements.foldLeft(template) {
        case (str, (olds, news)) =>
          str.replaceAll(olds, saveDollars(news))
      }
    
    def code = dotDollars(interpolate(
      "@@NAME@@" -> slotName,
      "@@SETTERNAME@@" -> setterName,
      "@@TYPE@@" -> tpe
    ))
  }
  
  object Binder {
    def apply(slots: BindSlot*): Binder =
      apply(getBinderName(), slots: _*)

    def apply(objectName: String, slots: BindSlot*): Binder = 
      returning(new Binder(objectName))(b => slots foreach (b add _))
  }

  class Binder(val objectName: String) {
    private val slots   = new ListBuffer[BindSlot]
    private val thunks  = new ListBuffer[() => Unit]
    
    /** We need the repl val import even here because commands like :type and :sym
     *  perform bindings directly and need handles on those objects.
     */
    def binderCode = """
      |object %s {
      |  %s
      |  %s
      |}""".stripMargin.format(objectName, replValsBinder.importCode, slotCode)
    
    def names                 = slots map (_.slotName)
    def find(name: String)    = slots find (_.slotName == name) getOrElse Predef.error("No slot defined for " + name)
    def setter(name: String)  = find(name).setterName
    def path(name: String)    = objectName + "." + name
    def slotCode              = slots map (_.code) mkString "\n"
    
    lazy val binderObject   = loadByName(objectName)
    lazy val compile        = compileString(binderCode) && { thunks foreach (f => f()) ; true }
    def importCode          = "import " + objectName + "._"
    def importAll           = quietlyRun(importCode)
    
    def add(slot: BindSlot) =
      slots += slot
    
    def add(name: String, typeString: String, value: Any): Unit = {
      slots += BindSlot(name, typeString)
      thunks += (() => set(name, value))
    }
    def add[T: Manifest](name: String, value: T): Unit =
      add(name, manifest[T].toString, value)
    
    private def methodByName(c: Class[_], name: String): JMethod =
      c.getMethod(name, classOf[Object])
    
    def set[T](slotName: String, value: T) = {
      val setterMethod = methodByName(binderObject, setter(slotName))
      setterMethod.invoke(null, value.asInstanceOf[AnyRef])
    }
  }
}