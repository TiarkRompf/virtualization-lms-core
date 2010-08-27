package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter


trait NumericOps extends Base with OverloadHack {
//  /**
//   Int operations
//   **/
//  implicit def repIntToRepIntOps(i: Rep[Int]) = new RepIntOpsCls(i)
//  implicit def intToRepIntOps(i: Int) = new RepIntOpsCls(i)
//
//  class RepIntOpsCls(a: Rep[Int]) {
//    def until(b: Rep[Int]): Rep[Range] = int_until(a,b)
//    def < (b: Rep[Int]): Rep[Boolean]  = int_lt(a,b)
//  }
//  // abstract methods for int operations
//  def int_until(a: Rep[Int], b: Rep[Int]): Rep[Range]
//  def int_lt   (a: Rep[Int], b: Rep[Int]): Rep[Boolean]
//
//  /**
//   Double Operations
//   **/
//  implicit def repDoubleToRepDoubleOps(d: Rep[Double]) = new RepDoubleOpsCls(d)
//  implicit def doubleToRepDoubleOps(d: Double) = new RepDoubleOpsCls(d)
//
//  object Double {
//    def parseDouble(s: Rep[String]) = obj_parsedouble(s)
//  }
//
//  class RepDoubleOpsCls(a: Rep[Double]) {
//    def + (b: Rep[Double]) = double_plus(a,b)
//    def * (b: Rep[Double]) = double_mul(a,b)
//    def / (b: Rep[Double]) = double_div(a,b)
//    def / (b: Rep[Int])(implicit o: Overloaded1)  = double_div(a,implicit_convert(b))
//    def <= (b: Rep[Double]) = double_lte(a,b)
//  }
//
//  def obj_parsedouble(s: Rep[String]) : Rep[Double]
//  def double_plus(a: Rep[Double], b: Rep[Double]): Rep[Double]
//  def double_mul(a: Rep[Double], b: Rep[Double]): Rep[Double]
//  def double_div(a: Rep[Double], b: Rep[Double]): Rep[Double]
//  def double_lte(a: Rep[Double], b: Rep[Double]): Rep[Boolean]

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

trait NumericOpsExp extends NumericOps with BaseExp {
  case class NumericPlus[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]
  case class NumericMinus[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]
  case class NumericTimes[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends Def[T]

  def numeric_plus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericPlus(lhs, rhs, n)
  def numeric_minus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericMinus(lhs, rhs, n)
  def numeric_times[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T]) : Rep[T] = NumericTimes(lhs, rhs, n)
}

trait ScalaGenNumeric extends ScalaGenBase  { this: NumericOpsExp =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case NumericPlus(a,b,n) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b,n) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b,n) => emitValDef(sym, quote(a) + " * " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
