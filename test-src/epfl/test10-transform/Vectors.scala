package scala.lms
package epfl
package test10

import common._
import internal.{NestedBlockTraversal}
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}
import test8._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

/*

  highest level: immutable Vectors 
  
  for loops, map and reduce
  
  mutable arrays, while loops
  
*/


// Vector API

trait VectorOps extends Base {
  trait Vector[T]
  implicit def vectorTyp[T:Typ]: Typ[Vector[T]]
  def vzeros(n: Rep[Int]): Rep[Vector[Double]]
  def vliteral[T:Typ](a: List[Rep[T]]): Rep[Vector[T]]
  def vapply[T:Typ](a: Rep[Vector[T]], x: Rep[Int]): Rep[T]
  def vupdate[T:Typ](a: Rep[Vector[T]], x: Rep[Int], y: Rep[T]): Rep[Unit]
  def vlength[T:Typ](a: Rep[Vector[T]]): Rep[Int]
  def vplus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]]
}

trait VectorExp extends VectorOps with EffectExp {
  implicit def vectorTyp[T:Typ]: Typ[Vector[T]] = { 
    implicit val ManifestTyp(m) = typ[T]
    ManifestTyp(implicitly)
  }
  implicit def intTyp: Typ[Int]
  implicit def doubleTyp: Typ[Double]

  case class VectorZeros(n: Rep[Int]) extends Def[Vector[Double]]
  case class VectorLiteral[T](a: List[Rep[T]]) extends Def[Vector[T]]
  case class VectorApply[T](a: Rep[Vector[T]], x: Rep[Int]) extends Def[T]
  case class VectorUpdate[T](a: Rep[Vector[T]], x: Rep[Int], y: Rep[T]) extends Def[Unit]
  case class VectorLength[T](a: Rep[Vector[T]]) extends Def[Int]
  case class VectorPlus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) extends Def[Vector[Double]]

  def vzeros(n: Rep[Int]): Rep[Vector[Double]] = VectorZeros(n)
  def vliteral[T:Typ](a: List[Rep[T]]): Rep[Vector[T]] = VectorLiteral(a)
  def vplus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = VectorPlus(a,b)
  def vapply[T:Typ](a: Rep[Vector[T]], x: Rep[Int]): Rep[T] = VectorApply(a,x)
  def vupdate[T:Typ](a: Rep[Vector[T]], x: Rep[Int], y: Rep[T]): Rep[Unit] = VectorUpdate(a,x,y)
  def vlength[T:Typ](a: Rep[Vector[T]]): Rep[Int] = VectorLength(a)

  // FIXME: wrong manifests -- need to take from Def
  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case VectorZeros(n) => vzeros(f(n))
    case VectorLiteral(a) => vliteral(f(a))(mtyp1[A])
    case VectorApply(a,x) => vapply(f(a),f(x))(mtyp1[A])
    case VectorUpdate(a,x,y) => vupdate(f(a),f(x),f(y))(mtyp1[A])
    case VectorLength(a) => vlength(f(a))(mtyp1[A])
    case VectorPlus(a, b) => vplus(f(a),f(b))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case VectorZeros(n) => VectorZeros(f(n))
    case VectorLiteral(a) => VectorLiteral(f(a))
    case VectorApply(a,x) => VectorApply(f(a),f(x))
    case VectorUpdate(a,x,y) => VectorUpdate(f(a),f(x),f(y))
    case VectorLength(a) => VectorLength(f(a))
    case VectorPlus(a, b) => VectorPlus(f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case VectorLiteral(as) => syms(as)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case VectorApply(a,x) => syms(a)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case _ => super.copySyms(e)
  }
}

trait VectorExpOpt extends VectorExp {

  override def vplus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = (a,b) match {
    case (Def(VectorZeros(n)), b) => b
    case (a, Def(VectorZeros(n))) => a
    case _ => super.vplus(a,b)
  }
  
}

trait ScalaGenVector extends ScalaGenBase {
  val IR: VectorExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    if (rhs.toString.startsWith("Vector"))
      emitValDef(sym, rhs.toString)
    else
      super.emitNode(sym,rhs)
  }
}