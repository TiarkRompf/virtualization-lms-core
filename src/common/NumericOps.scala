package scala.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.lms.internal._

trait LiftNumeric { this: Base =>
  implicit def numericToNumericRep[T:Numeric:Typ](x: T) = unit(x)
}

trait NumericOps extends Variables { this: PrimitiveOps =>

  // workaround for infix not working with manifests
  implicit def numericToNumericOps[T:Numeric:Typ](n: T) = new NumericOpsCls(unit(n))
  implicit def repNumericToNumericOps[T:Numeric:Typ](n: Rep[T]) = new NumericOpsCls(n)
  implicit def varNumericToNumericOps[T:Numeric:Typ](n: Var[T])(implicit pos: SourceContext) = new NumericOpsCls(readVar(n))

  class NumericOpsCls[T:Numeric:Typ](lhs: Rep[T]){
    def +[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,unit(c(rhs)))
    def +(rhs: Rep[T])(implicit pos: SourceContext) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T])(implicit pos: SourceContext) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T])(implicit pos: SourceContext) = numeric_times(lhs,rhs)
    def /(rhs: Rep[T])(implicit pos: SourceContext) = numeric_divide(lhs,rhs)
  }

  def numeric_plus[T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_minus[T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_times[T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_divide[T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait NumericOpsExp extends NumericOps with VariablesExp with BaseExp { this: PrimitiveOpsExp =>

  case class NumericPlus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDef1[T]
  case class NumericMinus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDef1[T]
  case class NumericTimes[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDef1[T]
  case class NumericDivide[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDef1[T]

  def numeric_plus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = toAtom(NumericPlus(lhs, rhs))
  def numeric_minus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = toAtom(NumericMinus(lhs, rhs))
  def numeric_times[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = toAtom(NumericTimes(lhs, rhs))
  def numeric_divide[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = toAtom(NumericDivide(lhs, rhs))

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@NumericPlus(l,r) => numeric_plus(f(l), f(r))(ntype(e.nR), mtype(e.mR), pos)
    case e@NumericMinus(l,r) => numeric_minus(f(l), f(r))(ntype(e.nR), mtype(e.mR), pos)
    case e@NumericTimes(l,r) => numeric_times(f(l), f(r))(ntype(e.nR), mtype(e.mR), pos)
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(ntype(e.nR), mtype(e.mR), pos)
    case _ => super.mirror(e,f)
  }
}


trait NumericOpsExpOpt extends NumericOpsExp { this: PrimitiveOpsExp =>

  override def numeric_plus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].plus(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => x
    case _ => super.numeric_plus(lhs,rhs)
  }
  override def numeric_minus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].minus(x,y))
    case _ => super.numeric_minus(lhs,rhs)
  }
  override def numeric_times[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].times(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => Const(x)
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => Const(y)
    case (Const(x), y) if x == implicitly[Numeric[T]].one => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].one => x
    case _ => super.numeric_times(lhs,rhs)
  }
  override def numeric_divide[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    // CAVEAT: Numeric doesn't have .div, Fractional has
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].asInstanceOf[Fractional[T]].div(x,y))
    case _ => super.numeric_divide(lhs,rhs)
  }
}


trait ScalaGenNumericOps extends ScalaGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, src"$a + $b")
    case NumericMinus(a,b) => emitValDef(sym, src"$a - $b")
    case NumericTimes(a,b) => emitValDef(sym, src"$a * $b")
    case NumericDivide(a,b) => emitValDef(sym, src"$a / $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenNumericOps extends CLikeGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case NumericPlus(a,b) =>
          emitValDef(sym, src"$a + $b")
        case NumericMinus(a,b) =>
          emitValDef(sym, src"$a - $b")
        case NumericTimes(a,b) =>
          emitValDef(sym, src"$a * $b")
        case NumericDivide(a,b) =>
          emitValDef(sym, src"$a / $b")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenNumericOps extends CudaGenBase with CLikeGenNumericOps
trait OpenCLGenNumericOps extends OpenCLGenBase with CLikeGenNumericOps
trait CGenNumericOps extends CGenBase with CLikeGenNumericOps

