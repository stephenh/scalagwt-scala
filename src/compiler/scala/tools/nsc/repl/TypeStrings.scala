/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import scala.reflect._
import java.lang.{ Class => JClass }

/** Logic for turning a type into a String.  The goal is to be
 *  able to take some arbitrary object 'x' and obtain the most precise
 *  String for which an injection of x.asInstanceOf[String] will
 *  be valid from both the JVM's and scala's perspectives.
 */

object TypeStrings {
  def fromManifest[T](implicit m: Manifest[T]): String = {
    val str = m.toString

    // I'm sure there are more to be discovered...
    val regexp1 = """(.*?)\[(.*)\]""".r
    val regexp2str = """.*\.type#"""
    val regexp2 = (regexp2str + """(.*)""").r
    
    val str2 = str.replaceAll("""\n""", "")

    str2 match { 
      case regexp1(clazz, typeArgs) => "%s[%s]".format(clazz, typeArgs.replaceAll(regexp2str, ""))
      case regexp2(clazz)           => clazz
      case x                        => x
    }
  }
  
  def fromOptManifest[T](implicit m: OptManifest[T]): String =
    if (m eq NoManifest) "Any"
    else if (m.isInstanceOf[Manifest[_]]) fromManifest[T](m.asInstanceOf[Manifest[T]])
    else fromClassManifest[T](m.asInstanceOf[ClassManifest[T]])
  
  def fromClassManifest[T](implicit m: ClassManifest[T]): String =
    m.toString
  
  def fromJavaClass(clazz: JClass[_]): String = {
    val typeParams = clazz.getTypeParameters
    val basename = clazz.getName
    val tpString = if (typeParams.isEmpty) "" else "[%s]".format(typeParams map (_ => "_") mkString ", ")

    basename + tpString
  }
  def applyJava(x: Any) = fromJavaClass(x.asInstanceOf[AnyRef].getClass)
  def apply[T](x: T)(implicit m: OptManifest[T]): String = fromOptManifest(m)
}  
