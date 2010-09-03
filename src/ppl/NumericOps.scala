package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter


trait NumericOps extends Base with OverloadHack {
  implicit def repNumericToNumericOps[T](x: Rep[T])(implicit n: Numeric[T]) = new NumericOpsCls(x,n)
  implicit def numericToNumericOps[T](x: T)(implicit n: Numeric[T]) = new NumericOpsCls(x,n)

  class NumericOpsCls[T](lhs: Rep[T], implicit val n: Numeric[T]) {
    def +(rhs: Rep[T]) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs,rhs)
    def toFloat() = numeric_toFloat(lhs)
  }

  def numeric_plus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T]): Rep[T]
  def numeric_minus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T]): Rep[T]
  def numeric_times[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T]): Rep[T]
  //def numeric_negate[T](x: T)(implicit n: Numeric[T]): Rep[T]
  //def numeric_abs[T](x: T)(implicit n: Numeric[T]): Rep[T]
  //def numeric_signum[T](x: T)(implicit n: Numeric[T]): Rep[Int]
  def numeric_toFloat[T](lhs: Rep[T])(implicit n: Numeric[T]): Rep[Float]
}

trait NumericOpsExp extends NumericOps with BaseExp {
  case class NumericPlus[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]
  case class NumericMinus[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]
  case class NumericTimes[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]
  case class NumericToFloat[T](lhs: Exp[T], implicit val n: Numeric[T]) extends Def[Float]

  def numeric_plus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericPlus(lhs, rhs, n)
  def numeric_minus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericMinus(lhs, rhs, n)
  def numeric_times[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericTimes(lhs, rhs, n)
  def numeric_toFloat[T](lhs: Rep[T])(implicit n: Numeric[T]): Rep[Float] = NumericToFloat(lhs,n)
}

trait ScalaGenNumeric extends ScalaGenBase  { this: NumericOpsExp =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case NumericPlus(a,b,n) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b,n) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b,n) => emitValDef(sym, quote(a) + " * " + quote(b))
    case NumericToFloat(a,n) => emitValDef(sym, quote(a) + ".toFloat()")
    case _ => super.emitNode(sym, rhs)
  }
}
