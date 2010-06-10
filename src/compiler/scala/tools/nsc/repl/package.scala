/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */
 
package scala.tools.nsc

import scala.collection.generic.CanBuildFrom

package object repl {
  val IR = Interpreter
  val NameTransformer = scala.reflect.NameTransformer
  val NoManifest = scala.reflect.NoManifest
  
  type PrintWriter = java.io.PrintWriter
  type BufferedReader = java.io.BufferedReader
  type BatchSourceFile = util.BatchSourceFile
  type URL = java.net.URL
  type JMethod = java.lang.reflect.Method
  type OptManifest[T] = scala.reflect.OptManifest[T]
  type AbstractFile = scala.tools.nsc.io.AbstractFile
  type ScalaClassLoader = scala.tools.nsc.util.ScalaClassLoader
  type ListBuffer[T] = scala.collection.mutable.ListBuffer[T]
  val ListBuffer = scala.collection.mutable.ListBuffer
  val ScalaClassLoader = scala.tools.nsc.util.ScalaClassLoader
  
  def optManifest[T](implicit m: OptManifest[T]): OptManifest[T] = m
  
  /** Tracing */
  def tracing[T](msg: String)(x: T): T = { println("(" + msg + ") " + x) ; x }
  
  /** Frequency counter */
  def freq[T](seq: Seq[T]) = seq groupBy identity mapValues (_.length)
  
  /** null becomes "", otherwise identity */
  def onull(s: String) = if (s == null) "" else s
  
  /** Reducing fully qualified noise for some common packages.
   */
  val typeTransforms = List(
    "java.lang." -> "",
    "scala.collection.immutable." -> "immutable.",
    "scala.collection.mutable." -> "mutable.",
    "scala.collection.generic." -> "generic."
  )
    
  def quietString(tp: String): String =
    typeTransforms.foldLeft(tp) {
      case (str, (prefix, replacement)) =>
        if (str startsWith prefix) replacement + (str stripPrefix prefix)
        else str
    }
  
  def typeString[T: OptManifest](x: T): String = TypeStrings(x)
  def typeStringJava(x: Any): String = TypeStrings.applyJava(x)
  
  /** collect + flatten */
  def flatCollect[A, B, CC[X] <: Traversable[X]]
    (coll: CC[A])
    (pf: PartialFunction[A, CC[B]])
    (implicit bf: CanBuildFrom[CC[A], B, CC[B]]) =
  {
    val b = bf(coll)
    for (x <- coll collect pf)
      b ++= x

    b.result
  }
}
