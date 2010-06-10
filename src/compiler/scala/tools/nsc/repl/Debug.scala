/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import scala.reflect._

/** Methods for using the repl as a debugger.
 */
object Debug {
  def breakIf(assertion: => Boolean, args: DebugParam[_]*): Unit =
    if (assertion) break(args.toList)

  // start a repl, binding supplied args
  def break(args: List[DebugParam[_]]): Unit = {
    val intLoop = new InterpreterLoop
    import intLoop._

    settings = ( args collect { case SettingsParam(s) => s } headOption ) getOrElse REPLSettings()
    createInterpreter()
    in = InteractiveReader.createDefault(repl)

    // rebind exit so people don't accidentally call System.exit by way of predef
    repl quietlyRun """def exit = println("Type :quit to resume program execution.")"""
    args foreach { p =>
      repl.quietlyBind(p.name, p.typeStr, p.param)
      println("%s: %s".format(p.name, p.typeStr))
    }
    loop()
    println("Exiting debug session.")
    closeInterpreter()
  }
}

object DebugParam {
  implicit def tuple2debugparam[T](x: (String, T))(implicit m: OptManifest[T]): DebugParam[T] =
    new DebugParam[T](x._1, x._2)(m)
  
  implicit def any2debugparam[T](x: T)(implicit m: OptManifest[T]): DebugParam[T] =
    new DebugParam[T]("p" + getCount(), x)(m)
  
  private var counter = 0 
  def getCount() = { counter += 1; counter }
  
  def apply[T](name: String, param: T)(implicit m: OptManifest[T]) = new DebugParam[T](name, param)(m)
  def unapply[T: OptManifest](x: Any): Option[(String, T, OptManifest[T])] = x match {
    case x: DebugParam[_] => Some((x.name, x.param.asInstanceOf[T], x.m.asInstanceOf[OptManifest[T]]))
    case _                => None
  }
}

class DebugParam[T](val name: String, val param: T)(implicit val m: OptManifest[T]) {
  val typeStr = typeString(param)
}

case class GlobalParam[T <: Global: OptManifest](global: T) extends DebugParam("global", global) { }
case class SettingsParam[T <: Settings: OptManifest](settings: T) extends DebugParam("settings", settings) { }

