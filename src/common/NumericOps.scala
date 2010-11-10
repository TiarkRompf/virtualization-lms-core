package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaGenBase

trait NumericOps extends Variables {
  def infix_+[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_plus(lhs,rhs)
  def infix_-[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_minus(lhs,rhs)
  def infix_*[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_times(lhs,rhs)
    
  class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T]) {
    def +(rhs: Rep[T]) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs,rhs)
  }

  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait NumericOpsExp extends NumericOps with VariablesExp {
  implicit def varNumericToNumericOps[T:Numeric:Manifest](x: Var[T]) = new NumericOpsCls(readVar(x))

  case class NumericPlus[T](lhs: Exp[T], rhs: Exp[T])(implicit val n:Numeric[T], val mT: Manifest[T]) extends Def[T]
  case class NumericMinus[T](lhs: Exp[T], rhs: Exp[T])(implicit val n:Numeric[T], val mT: Manifest[T]) extends Def[T]
  case class NumericTimes[T](lhs: Exp[T], rhs: Exp[T])(implicit val n:Numeric[T], val mT: Manifest[T]) extends Def[T]

  def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericTimes(lhs, rhs)
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
