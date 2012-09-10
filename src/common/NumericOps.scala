package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftNumeric {
  this: Base =>

  implicit def numericToNumericRep[T:Numeric:Manifest](x: T) = unit(x)
}

trait NumericOps extends Variables { this: ImplicitOps =>

  // workaround for infix not working with manifests
  implicit def numericToNumericOps[T:Numeric:Manifest](n: T) = new NumericOpsCls(unit(n))
  implicit def repNumericToNumericOps[T:Numeric:Manifest](n: Rep[T]) = new NumericOpsCls(n)
  implicit def varNumericToNumericOps[T:Numeric:Manifest](n: Var[T]) = new NumericOpsCls(readVar(n))
  
  class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T]){
    def +[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,unit(c(rhs)))
    def +(rhs: Rep[T])(implicit pos: SourceContext) = numeric_plus(lhs,rhs)
    def +[A:Manifest](rhs: Rep[A])(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,implicit_convert[A, T](rhs))
    def -[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_minus(lhs,unit(c(rhs)))
    def -(rhs: Rep[T])(implicit pos: SourceContext) = numeric_minus(lhs,rhs)
    def -[A:Manifest](rhs: Rep[A])(implicit c: A => T, pos: SourceContext) = numeric_minus(lhs,implicit_convert[A, T](rhs))
    def *[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_times(lhs,unit(c(rhs)))
    def *(rhs: Rep[T])(implicit pos: SourceContext) = numeric_times(lhs,rhs)
    def *[A:Manifest](rhs: Rep[A])(implicit c: A => T, pos: SourceContext) = numeric_times(lhs,implicit_convert[A, T](rhs))
    def /[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_divide(lhs,unit(c(rhs)))
    def /(rhs: Rep[T])(implicit pos: SourceContext) = numeric_divide(lhs,rhs)
    def /[A:Manifest](rhs: Rep[A])(implicit c: A => T, pos: SourceContext) = numeric_divide(lhs,implicit_convert[A, T](rhs))
  }

  //def infix_+[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_plus(lhs,rhs)
  //def infix_-[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_minus(lhs,rhs)
  //def infix_*[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_times(lhs,rhs)

  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_divide[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait NumericOpsExp extends NumericOps with VariablesExp with BaseFatExp {
  abstract class DefMN[A:Manifest:Numeric] extends Def[A] {
    def mev = manifest[A]
    def aev = implicitly[Numeric[A]]
  }

  case class NumericPlus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericMinus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericTimes[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericDivide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]

  def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericTimes(lhs, rhs)
  def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericDivide(lhs, rhs)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@NumericPlus(l,r) => numeric_plus(f(l), f(r))(e.aev, e.mev, pos)
    case e@NumericMinus(l,r) => numeric_minus(f(l), f(r))(e.aev, e.mev, pos)
    case e@NumericTimes(l,r) => numeric_times(f(l), f(r))(e.aev, e.mev, pos)
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(e.aev, e.mev, pos)
    case _ => super.mirror(e,f)
  }

}


trait NumericOpsExpOpt extends NumericOpsExp {
  
  override def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].plus(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => x
    case _ => super.numeric_plus(lhs,rhs)
  }
  override def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = NumericMinus(lhs, rhs)
  override def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].times(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => Const(x)
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => Const(y)
    case (Const(x), y) if x == implicitly[Numeric[T]].one => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].one => x
    case _ => super.numeric_times(lhs,rhs)
  }
  override def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = NumericDivide(lhs, rhs)

}


trait ScalaGenNumericOps extends ScalaGenFat {
  val IR: NumericOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case NumericDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenNumericOps extends CLikeGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case NumericPlus(a,b) =>
          emitValDef(sym, quote(a) + " + " + quote(b))
        case NumericMinus(a,b) =>
          emitValDef(sym, quote(a) + " - " + quote(b))
        case NumericTimes(a,b) =>
          emitValDef(sym, quote(a) + " * " + quote(b))
        case NumericDivide(a,b) =>
          emitValDef(sym, quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenNumericOps extends CudaGenBase with CLikeGenNumericOps
trait OpenCLGenNumericOps extends OpenCLGenBase with CLikeGenNumericOps
trait CGenNumericOps extends CGenBase with CLikeGenNumericOps

