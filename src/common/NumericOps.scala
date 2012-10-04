package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftNumeric {
  this: Base with NumericOps =>

  // HACK The Numeric context bound is not *required* but it is useful to reduce the applicability of this implicit conversion
  implicit def numericToNumericRep[T:Numeric:Manifest](x: T) = unit(x)
  // Explicit `1 + unit(1)` support because it needs two implicit conversions (FIXME Doesn’t work)
  implicit def anyToNumericOps[A](a: A)(implicit lift: A => Rep[A]) = new NumericOpsCls(lift(a))
  // implicit def numericToNumericOps[A : Numeric : Manifest](a: A) = new NumericOpsCls(unit(a))
}

trait NumericOps extends Variables {

  // Type constraints allowing an eventual type promotion (e.g. Int to Float) before performing the numeric operation
  object NumericOpsTypes {
    trait Args { type Lhs; type Rhs }
    trait ~[A, B] extends Args { type Lhs = A; type Rhs = B }
    class :=[A <: Args, B](val lhs: Rep[A#Lhs] => Rep[B], val rhs: Rep[A#Rhs] => Rep[B])(implicit val Numeric: Numeric[B])
  }
  import NumericOpsTypes._
  implicit def numericSameArgs[A : Numeric] = new (A ~ A := A) (identity, identity)

  /* FIXME
   * I’d like to define numeric operators as follows:
   * def infix_+[A, B](lhs: A, rhs: B)(implicit someAdditionnalConstraints...)
   * But this signature leads to an ambiguous reference to overloaded definition with an infix_+(s: String, a: Any) method defined in EmbeddedControls (?)
   */
  implicit class NumericOpsCls[A](lhs: Rep[A]) {
    def + [B, C](rhs: Rep[B])(implicit op: (A ~ B := C), mC: Manifest[C], sc: SourceContext) = numeric_plus(op.lhs(lhs), op.rhs(rhs))(op.Numeric, mC, sc)
    def - [B, C](rhs: Rep[B])(implicit op: (A ~ B := C), mC: Manifest[C], sc: SourceContext) = numeric_minus(op.lhs(lhs), op.rhs(rhs))(op.Numeric, mC, sc)
    def * [B, C](rhs: Rep[B])(implicit op: (A ~ B := C), mC: Manifest[C], sc: SourceContext) = numeric_times(op.lhs(lhs), op.rhs(rhs))(op.Numeric, mC, sc)
    def / [B, C](rhs: Rep[B])(implicit op: (A ~ B := C), mC: Manifest[C], sc: SourceContext) = numeric_divide(op.lhs(lhs), op.rhs(rhs))(op.Numeric, mC, sc)
  }
  implicit def varNumericToNumericOps[T : Numeric : Manifest](n: Var[T]) = new NumericOpsCls(readVar(n))

  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_divide[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

/*
 * Enable promotion of arguments involved in a numeric operation provided there exists an implicit conversion to perform the promotion.
 * For instance, it allows to mix Int values and Double values in a numeric operation.
 */
trait NumericPromotions { this: ImplicitOps with NumericOps =>
  import NumericOpsTypes._
  implicit def numericPromoteLhs[A : Manifest, B : Numeric : Manifest](implicit aToB: A => B) = new (A ~ B := B) (lhs = implicit_convert[A, B](_), rhs = identity)
  implicit def numericPromoteRhs[A : Manifest, B : Numeric : Manifest](implicit aToB: A => B) = new (B ~ A := B) (lhs = identity, rhs = implicit_convert[A, B](_))
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
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@NumericPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

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

