package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaGenBase

trait NumericOps extends Variables {
  // workaround for infix not working with manifests
  implicit def numericToRepNumericCls[T:Numeric:Manifest](n: T) = new NumericOpsCls(n)
  implicit def repNumericToRepNumericCls[T:Numeric:Manifest](n: Rep[T]) = new NumericOpsCls(n)
  implicit def varNumericToNumericOps[T:Numeric:Manifest](n: Var[T]) = new NumericOpsCls(readVar(n))
  
  class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T]){
    def +[A](rhs: A)(implicit c: A => T) = numeric_plus(lhs,c(rhs))
    def +(rhs: Rep[T]) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs,rhs)
  }

  //def infix_+[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_plus(lhs,rhs)
  //def infix_-[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_minus(lhs,rhs)
  //def infix_*[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_times(lhs,rhs)

  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait NumericOpsExp extends NumericOps with VariablesExp {
  case class NumericPlus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class NumericPlusEquals[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Unit]
  case class NumericMinus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class NumericTimes[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = NumericTimes(lhs, rhs)
}

trait ScalaGenNumericOps extends ScalaGenBase {
  val IR: NumericOpsExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
