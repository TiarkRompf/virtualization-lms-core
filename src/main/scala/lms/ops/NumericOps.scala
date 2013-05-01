package scala.lms
package ops

import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftNumeric {
  this: Base =>

  implicit def numericToNumericRep[T:Numeric:TypeRep](x: T) = unit(x)
}

trait NumericOps extends Variables {

  // workaround for infix not working with manifests
  implicit def numericToNumericOps[T:Numeric:TypeRep](n: T) = new NumericOpsCls(unit(n))
  implicit def repNumericToNumericOps[T:Numeric:TypeRep](n: Rep[T]) = new NumericOpsCls(n)
  implicit def varNumericToNumericOps[T:Numeric:TypeRep](n: Var[T]) = new NumericOpsCls(readVar(n))

  class NumericOpsCls[T:Numeric:TypeRep](lhs: Rep[T]){
    def +[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,unit(c(rhs)))
    def +(rhs: Rep[T])(implicit pos: SourceContext) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T])(implicit pos: SourceContext) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T])(implicit pos: SourceContext) = numeric_times(lhs,rhs)
    def /(rhs: Rep[T])(implicit pos: SourceContext) = numeric_divide(lhs,rhs)
  }

  //def infix_+[T:Numeric:TypeRep](lhs: Rep[T], rhs: Rep[T]) = numeric_plus(lhs,rhs)
  //def infix_-[T:Numeric:TypeRep](lhs: Rep[T], rhs: Rep[T]) = numeric_minus(lhs,rhs)
  //def infix_*[T:Numeric:TypeRep](lhs: Rep[T], rhs: Rep[T]) = numeric_times(lhs,rhs)

  def numeric_plus[T:Numeric:TypeRep](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_minus[T:Numeric:TypeRep](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_times[T:Numeric:TypeRep](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_divide[T:Numeric:TypeRep](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait NumericOpsExp extends NumericOps with VariablesExp with BaseFatExp {
  abstract class DefMN[A:TypeRep:Numeric] extends Def[A] {
    def mev = typeRep[A]
    def aev = implicitly[Numeric[A]]
  }

  case class NumericPlus[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericMinus[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericTimes[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericDivide[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]

  def numeric_plus[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericTimes(lhs, rhs)
  def numeric_divide[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericDivide(lhs, rhs)

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@NumericPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}


trait NumericOpsExpOpt extends NumericOpsExp {

  override def numeric_plus[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].plus(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => x
    case _ => super.numeric_plus(lhs,rhs)
  }
  override def numeric_minus[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].minus(x,y))
    case _ => super.numeric_minus(lhs,rhs)
  }
  override def numeric_times[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].times(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => Const(x)
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => Const(y)
    case (Const(x), y) if x == implicitly[Numeric[T]].one => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].one => x
    case _ => super.numeric_times(lhs,rhs)
  }
  override def numeric_divide[T:Numeric:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    // CAVEAT: Numeric doesn't have .div, Fractional has
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].asInstanceOf[Fractional[T]].div(x,y))
    case _ => super.numeric_divide(lhs,rhs)
  }
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
