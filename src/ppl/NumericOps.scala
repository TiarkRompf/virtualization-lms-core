package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter


trait NumericOps extends Base with Variables with OverloadHack {
  // TODO: fix or investigate -- this causes a compiler bug
  //implicit val repDoubleToNumericOps : Rep[Double] => NumericOpsCls[Double] = x => new NumericOpsCls[Double](x)
  implicit def varNumericToNumericOps[T:Numeric](x: Var[T]) : NumericOpsCls[T]
  implicit def repNumericToNumericOps[T:Numeric](x: Rep[T]) = new NumericOpsCls(x)
  //implicit def numericToNumericOps[T:Numeric](x: T) = new NumericOpsCls(x)

  def __ext__*[T:Numeric](lhs: Rep[T], rhs: Rep[T]) = numeric_times(lhs,rhs)
  def __ext__+(lhs: Rep[Double], rhs: Rep[Double]) = numeric_plus(lhs,rhs)
  //def __ext__+[T:Numeric](lhs: Rep[T], rhs: Rep[T]) = numeric_plus(lhs,rhs)
  def __ext__-[T:Numeric](lhs: Rep[T], rhs: Rep[T]) = numeric_minus(lhs,rhs)

  class NumericOpsCls[T:Numeric](lhs: Rep[T]) {
    def +(rhs: Rep[T]) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs,rhs)
    def toFloat() = numeric_toFloat(lhs)
  }

  def numeric_plus[T:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_minus[T:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_times[T:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
  def numeric_toFloat[T](lhs: Rep[T])(implicit n: Numeric[T]): Rep[Float]
}

trait NumericOpsExp extends NumericOps with VariablesExp {
  implicit def varNumericToNumericOps[T:Numeric](x: Var[T]) = new NumericOpsCls(readVar(x))

  case class NumericPlus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class NumericMinus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class NumericTimes[T:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class NumericToFloat[T:Numeric](lhs: Exp[T]) extends Def[Float]

  def numeric_plus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericTimes(lhs, rhs)
  //def numeric_negate[T](x: T)(implicit n: Numeric[T]): Rep[T]
  //def numeric_abs[T](x: T)(implicit n: Numeric[T]): Rep[T]
  //def numeric_signum[T](x: T)(implicit n: Numeric[T]): Rep[Int]
  def numeric_toFloat[T:Numeric](lhs: Rep[T]): Rep[Float] = NumericToFloat(lhs)
}

trait ScalaGenNumeric extends ScalaGenBase with NumericOpsExp { 

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
   case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case NumericToFloat(a) => emitValDef(sym, quote(a) + ".toFloat()")
    case _ => super.emitNode(sym, rhs)
  }
}
