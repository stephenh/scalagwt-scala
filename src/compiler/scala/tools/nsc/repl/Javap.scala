/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package repl

import sun.tools.javap._
import java.io.ByteArrayInputStream
import util.returning

object Javap {
  // private val fieldNames    = List("showDisassembled", "showLineAndLocal", "showVerbose", "showInternalSigs")
  private val fieldNames    = List("showDisassembled", "showVerbose", "showInternalSigs")
  
  def newEnv = {
    val env = new JavapEnvironment()
    fieldNames foreach { name =>
      val x = classOf[JavapEnvironment] getDeclaredField name
      x setAccessible true
      x.set(env, true)
    }
    env
  }
  
  def apply(name: String): Wrapper = apply(name, ScalaClassLoader.getSystemLoader())
  def apply(name: String, cl: ScalaClassLoader): Wrapper = new Wrapper(name, cl.getBytesForClass(name))
  
  class Wrapper(val className: String, val bytes: Array[Byte]) {
    val pw      = new PrintWriter(System.out, true)
    val env     = newEnv
    val printer = new JavapPrinter(new ByteArrayInputStream(bytes), pw, env)
    import printer._
    
    def showHeader() = printclassHeader()
    def showPool() = printcp()
    def showInnerClasses() = printInnerClasses()
    def showMethods() = printMethods()
    def showFields() = printfields()
    
    def show(): Unit = {
      showHeader()
      showPool()
      showMethods()
      showFields()
    }
    
    def show(filt: MethodData => Boolean): Unit = {
      val cdata   = new ClassData(new ByteArrayInputStream(bytes))
      val methods = cdata.getMethods.toList
      methods filter filt foreach (printer printMethodAttributes _)
    }
    override def toString = "Javap(%s / %s bytes)".format(className, bytes.size)
  }
}
import Javap._

trait Javap {
  self: Interpreter =>
  
  def isClassfileFor(x: AbstractFile, s: String) = x.name endsWith ("$" + s + ".class")
  
  def findVirtualClassFile(s: String) =
    virtualDirectory.toList filter (isClassfileFor(_, s)) sortBy (_.lastModified) lastOption

  def bytesForClassName(s: String): Array[Byte] = {
    val bytes = classLoader.getBytesForClass(s)
    if (bytes.nonEmpty) bytes
    else findVirtualClassFile(s) map (_.toByteArray) getOrElse Array()
  }
  def nonEmptyBytes(s: String) = {
    val bytes = bytesForClassName(s)
    if (bytes.nonEmpty) Some(bytes) else {
      println("Unable to find '%s'.".format(s))
      None
    }
  }

  def javap(className: String): Unit  =
    nonEmptyBytes(className) foreach (xs => new Wrapper(className, xs) show())

  def javap(className: String, methodName: String): Unit =
    javap(className, _.getName == methodName)    

  def javap(className: String, f: MethodData => Boolean): Unit =
    nonEmptyBytes(className) foreach (xs => new Wrapper(className, xs) show f)

}