package scala.lms
package internal

import scala.reflect.RefinedManifest

trait Typs {
  abstract class Typ[T] {
    def typeArguments: List[Typ[_]]
    def arrayTyp: Typ[Array[T]]
    def runtimeClass: java.lang.Class[_]
    def <:<(that: Typ[_]): Boolean
    def erasure: java.lang.Class[_]
  }

  // TODO: Is separate type for RefinedManifest necessary?
  // TODO: Change this out with Tiark's implementation later
  case class RefinedTyp[T](rm: RefinedManifest[T]) extends Typ[T] {
    def typeArguments: List[Typ[_]] = rm.typeArguments.map(ManifestTyp(_))
    def arrayTyp: Typ[Array[T]] = ManifestTyp(rm.arrayManifest)
    def fields: List[(String, Typ[_])] = rm.fields.map(e => (e._1, ManifestTyp(e._2)))
    def runtimeClass: java.lang.Class[_] = rm.runtimeClass
    def <:<(that: Typ[_]): Boolean = that match {
      case RefinedTyp(rm1) => rm <:< rm1
      case _ => false
    }
    def erasure: java.lang.Class[_] = rm.erasure
    override def toString = rm.toString
  }

  case class ManifestTyp[T](mf: Manifest[T]) extends Typ[T] {
    def typeArguments: List[Typ[_]]   = mf.typeArguments.map(ManifestTyp(_))
    def arrayTyp: Typ[Array[T]] = ManifestTyp(mf.arrayManifest)
    def runtimeClass: java.lang.Class[_] = mf.runtimeClass
    def <:<(that: Typ[_]): Boolean = that match {
      case ManifestTyp(mf1) => mf.<:<(mf1)
      case _ => false
    }
    def erasure: java.lang.Class[_] = mf.erasure
    //override def canEqual(that: Any): Boolean = mf.canEqual(that) // TEMP
    //override def equals(that: Any): Boolean = mf.equals(that) // TEMP
    //override def hashCode = mf.hashCode
    override def toString = mf.toString
  }

  def typ[T:Typ]: Typ[T] = implicitly[Typ[T]]

  protected def manifestTyp[T:Manifest]: Typ[T] = ManifestTyp(implicitly)
  implicit def unitTyp: Typ[Unit] = manifestTyp
  implicit def nullTyp: Typ[Null] = manifestTyp
  implicit def anyvalTyp: Typ[AnyVal] = manifestTyp

  // HACK: need to pass explicit type during things like mirroring
  def mtype[A,B](m:Typ[A]): Typ[B] = m.asInstanceOf[Typ[B]]
  def ntype[A,B](n:Numeric[A]): Numeric[B] = n.asInstanceOf[Numeric[B]]
  def otype[A,B](o:Ordering[A]): Ordering[B] = o.asInstanceOf[Ordering[B]]
  def frtype[A,B](f:Fractional[A]): Fractional[B] = f.asInstanceOf[Fractional[B]]

  def isPrimitiveType[T](m: Typ[T]) = m.toString match {
    case "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" | "Boolean" | "Unit" => true
    case _ => false
  }
  def isDataStructureType[T](tp: Typ[T]): Boolean = false
}
