/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package repl

import util.returning

abstract class Updater {
  def update[T: Manifest](lhs: String, rhs: T): Unit
}

trait Symbolize {
  def clazz: Class[_]
  def newUpdater(): Updater

  implicit def createUpdater[T](x: T)(implicit ev: Symbol.type <:< T): Updater = newUpdater()
}

trait SymbolizedREPL extends Symbolize {
  self: Interpreter =>

  class ReplUpdater extends Updater {
    def update[T: Manifest](lhs: String, rhs: T): Unit = {
      bind(lhs, manifest[T].toString, rhs)
    }
  }
  
  class SettingsUpdater extends Updater {
    def update[T: Manifest](lhs: String, rhs: T): Unit = {
      def isString  = manifest[T] == manifest[String]
      def asString  = if (isString) "\"" + rhs + "\"" else rhs.toString
      def isEmpty   = rhs.toString == ""
      
      if (isEmpty) {
        val code = "$settings.unsetFromRepl($settings.%s)".format(lhs)
        quietlyEval[Boolean](code)
        println("Reset " + lhs + " to default.")
      }
      else {
        val code = "$settings.setFromRepl($settings.%s, %s)".format(lhs, asString)
        val result = quietlyEval[Boolean](code) exists (_ == true)
        println(
          if (result) "Set " + lhs + " to " + rhs
          else "Failed to update setting."
        )
      }
    }
  }
  
  def clazz = SymbolizedREPL.this.getClass
  def newUpdater() = new SettingsUpdater
}
