/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

import scala.collection.mutable.{ WrappedArray, ArrayBuilder }
import java.lang.{ Class => JClass }

/** A `ClassManifest[T]` is an opaque descriptor for type `T`.
 *  It is used by the compiler to preserve information necessary
 *  for instantiating `Arrays` in those cases where the element type
 *  is unknown at compile time.
 *
 *  The type-relation operators make an effort to present a more accurate
 *  picture than can be realized with erased types, but they should not be
 *  relied upon to give correct answers. In particular they are likely to
 *  be wrong when variance is involved or when a subtype has a different
 *  number of type arguments than a supertype.
 */
trait ClassManifest[T] extends OptManifest[T] with Equals with Serializable {
  /** A class representing the type `U` to which `T` would be erased. Note
    * that there is no subtyping relationship between `T` and `U`. */
  def erasure: JClass[_]

  private def subtype(sub: JClass[_], sup: JClass[_]): Boolean = {
    sys.error("GWT doesn't support getInterfaces")
  }

  private def subargs(args1: List[OptManifest[_]], args2: List[OptManifest[_]]) = (args1 corresponds args2) {
    // !!! [Martin] this is wrong, need to take variance into account
    case (x: ClassManifest[_], y: ClassManifest[_]) => x <:< y
    case (x, y)                                     => (x eq NoManifest) && (y eq NoManifest)
  }

  /** Tests whether the type represented by this manifest is a subtype 
    * of the type represented by `that` manifest, subject to the limitations
    * described in the header.
    */
  def <:<(that: ClassManifest[_]): Boolean = {    
    // All types which could conform to these types will override <:<.
    def cannotMatch = {
      import Manifest._
      that.isInstanceOf[AnyValManifest[_]] || (that eq AnyVal) || (that eq Nothing) || (that eq Null)
    }

    // This is wrong, and I don't know how it can be made right
    // without more development of Manifests, due to arity-defying
    // relationships like:
    //
    //   List[String] <: AnyRef
    //   Map[Int, Int] <: Iterable[(Int, Int)]
    // 
    // Given the manifest for Map[A, B] how do I determine that a
    // supertype has single type argument (A, B) ? I don't see how we
    // can say whether X <:< Y when type arguments are involved except
    // when the erasure is the same, even before considering variance.
    !cannotMatch && {
      // this part is wrong for not considering variance
      if (this.erasure == that.erasure)
        subargs(this.typeArguments, that.typeArguments)
      // this part is wrong for punting unless the rhs has no type
      // arguments, but it's better than a blindfolded pinata swing.
      else
        that.typeArguments.isEmpty && subtype(this.erasure, that.erasure)
    }
  }

  /** Tests whether the type represented by this manifest is a supertype 
    * of the type represented by `that` manifest, subject to the limitations
    * described in the header.
    */
  def >:>(that: ClassManifest[_]): Boolean =
    that <:< this

  def canEqual(other: Any) = other match {
    case _: ClassManifest[_] => true
    case _                   => false
  }

  /** Tests whether the type represented by this manifest is equal to
    * the type represented by `that` manifest, subject to the limitations
    * described in the header.
    */
  override def equals(that: Any): Boolean = that match {
    case m: ClassManifest[_] => (m canEqual this) && (this.erasure == m.erasure)
    case _                   => false
  }
  override def hashCode = this.erasure.##

  protected def arrayClass[T](tp: JClass[_]): JClass[Array[T]] = 
    sys.error("GWT doesn't support Array.newInstance")

  def arrayManifest: ClassManifest[Array[T]] = 
    ClassManifest.classType[Array[T]](arrayClass[T](erasure))

  def newArray(len: Int): Array[T] =
    sys.error("GWT doesn't support Array.newInstance")

  def newArray2(len: Int): Array[Array[T]] =
    sys.error("GWT doesn't support Array.newInstance")

  def newArray3(len: Int): Array[Array[Array[T]]] =
    sys.error("GWT doesn't support Array.newInstance")

  def newArray4(len: Int): Array[Array[Array[Array[T]]]] =
    sys.error("GWT doesn't support Array.newInstance")

  def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]] =
    sys.error("GWT doesn't support Array.newInstance")

  def newWrappedArray(len: Int): WrappedArray[T] =
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests 
    new WrappedArray.ofRef[T with AnyRef](newArray(len).asInstanceOf[Array[T with AnyRef]]).asInstanceOf[WrappedArray[T]]
  
  def newArrayBuilder(): ArrayBuilder[T] = 
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new ArrayBuilder.ofRef[T with AnyRef]()(this.asInstanceOf[ClassManifest[T with AnyRef]]).asInstanceOf[ArrayBuilder[T]]

  def typeArguments: List[OptManifest[_]] = List()

  protected def argString = 
    if (typeArguments.nonEmpty) typeArguments.mkString("[", ", ", "]")
    else if (erasure.isArray) "["+ClassManifest.fromClass(erasure.getComponentType)+"]"
    else ""
}

/** The object `ClassManifest` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 */
object ClassManifest {
  val Byte    = Manifest.Byte
  val Short   = Manifest.Short
  val Char    = Manifest.Char
  val Int     = Manifest.Int
  val Long    = Manifest.Long
  val Float   = Manifest.Float
  val Double  = Manifest.Double
  val Boolean = Manifest.Boolean
  val Unit    = Manifest.Unit
  val Any     = Manifest.Any
  val Object  = Manifest.Object
  val AnyVal  = Manifest.AnyVal
  val Nothing = Manifest.Nothing
  val Null    = Manifest.Null

  def fromClass[T](clazz: JClass[T]): ClassManifest[T] = sys.error("GWT doesn't support TYPE field")

  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] = Manifest.singleType(value)

  /** ClassManifest for the class type `clazz`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T <: AnyRef](clazz: JClass[_]): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class and `args` are its type arguments */
  def classType[T <: AnyRef](clazz: JClass[_], arg1: OptManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T <: AnyRef](prefix: OptManifest[_], clazz: JClass[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  def arrayType[T](arg: OptManifest[_]): ClassManifest[Array[T]] = arg match {
    case NoManifest => Object.asInstanceOf[ClassManifest[Array[T]]]
    case m: ClassManifest[_] => m.asInstanceOf[ClassManifest[T]].arrayManifest
  }

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: OptManifest[_], name: String, clazz: JClass[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassManifest[T] {
      def erasure = clazz
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection.
    * todo: remove after next boostrap
    */
  def abstractType[T](prefix: OptManifest[_], name: String, upperbound: ClassManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassManifest[T] {
      def erasure = upperbound.erasure
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }
}

/** Manifest for the class type `clazz[args]`, where `clazz` is
  * a top-level or static class. */
private class ClassTypeManifest[T <: AnyRef](
  prefix: Option[OptManifest[_]], 
  val erasure: JClass[_], 
  override val typeArguments: List[OptManifest[_]]) extends ClassManifest[T]
{
  override def toString = 
    (if (prefix.isEmpty) "" else prefix.get.toString+"#") + 
    (if (erasure.isArray) "Array" else erasure.getName) +
    argString
}
