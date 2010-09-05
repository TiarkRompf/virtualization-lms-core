package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter


trait NumericOps extends Base with Variables with OverloadHack {
  implicit def varNumericToNumericOps[T](x: Var[T])(implicit n: Numeric[T]) : NumericOpsCls[T]
  implicit def repNumericToNumericOps[T](x: Rep[T])(implicit n: Numeric[T]) = new NumericOpsCls(x,n)
  implicit def numericToNumericOps[T](x: T)(implicit n: Numeric[T]) = new NumericOpsCls(x,n)

  class NumericOpsCls[T](lhs: Rep[T], implicit val n: Numeric[T]) {
    def +(rhs: Rep[T]) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs,rhs)
  }

  def numeric_plus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T]): Rep[T]
  def numeric_minus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T]): Rep[T]
  def numeric_times[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T]): Rep[T]
  //def numeric_negate[T](x: T)(implicit n: Numeric[T]): Rep[T]
  //def numeric_abs[T](x: T)(implicit n: Numeric[T]): Rep[T]
  //def numeric_signum[T](x: T)(implicit n: Numeric[T]): Rep[Int]
}

trait NumericOpsExp extends NumericOps with VariablesExp {
  implicit def varNumericToNumericOps[T](x: Var[T])(implicit n: Numeric[T]) = new NumericOpsCls(varToRep(x), n)

  case class NumericPlus[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]
  case class NumericMinus[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]
  case class NumericTimes[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]

  def numeric_plus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericPlus(lhs, rhs, n)
  def numeric_minus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericMinus(lhs, rhs, n)
  def numeric_times[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericTimes(lhs, rhs, n)
}

trait ScalaGenNumeric extends ScalaGenBase with NumericOpsExp { 

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case NumericPlus(a,b,n) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b,n) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b,n) => emitValDef(sym, quote(a) + " * " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
