package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext

trait OrderingOps extends Base with Variables with OverloadHack {
  // workaround for infix not working with implicits in PrimitiveOps
  implicit def orderingToOrderingOps[T:Ordering:Manifest](n: T) = new OrderingOpsCls(unit(n))
  implicit def repOrderingToOrderingOps[T:Ordering:Manifest](n: Rep[T]) = new OrderingOpsCls(n)
  implicit def varOrderingToOrderingOps[T:Ordering:Manifest](n: Var[T]) = new OrderingOpsCls(readVar(n))

  class OrderingOpsCls[T:Ordering:Manifest](lhs: Rep[T]){
    def <(rhs: Rep[T])(implicit ctx: SourceContext) = ordering_lt(lhs, rhs)
    def <=(rhs: Rep[T])(implicit ctx: SourceContext) = ordering_lteq(lhs, rhs)
    def >(rhs: Rep[T])(implicit ctx: SourceContext) = ordering_gt(lhs, rhs)
    def >=(rhs: Rep[T])(implicit ctx: SourceContext) = ordering_gteq(lhs, rhs)
    def equiv(rhs: Rep[T])(implicit ctx: SourceContext) = ordering_equiv(lhs, rhs)
    def max(rhs: Rep[T])(implicit ctx: SourceContext) = ordering_max(lhs, rhs)
    def min(rhs: Rep[T])(implicit ctx: SourceContext) = ordering_min(lhs, rhs)

    def <[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext) = ordering_lt(lhs, c(rhs))
    def <=[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext) = ordering_lteq(lhs, c(rhs))
    def >[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext) = ordering_gt(lhs, c(rhs))
    def >=[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext) = ordering_gteq(lhs, c(rhs))
    def equiv[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext) = ordering_equiv(lhs, c(rhs))
    def max[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext) = ordering_max(lhs, c(rhs))
    def min[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext) = ordering_min(lhs, c(rhs))
  }

//  def infix_<[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: Manifest[T]) = ordering_lt(lhs,c(rhs))
//  def infix_<=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: Manifest[T]) = ordering_lteq(lhs,c(rhs))
//  def infix_>[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: Manifest[T]) = ordering_gt(lhs,c(rhs))
//  def infix_>=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: Manifest[T]) = ordering_gteq(lhs,c(rhs))
//  def infix_equiv[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: Manifest[T]) = ordering_equiv(lhs,c(rhs))
//  def infix_max[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: Manifest[T]) = ordering_max(lhs,c(rhs))
//  def infix_min[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: Manifest[T]) = ordering_min(lhs,c(rhs))

  def ordering_lt[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Boolean]
  def ordering_lteq[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Boolean]
  def ordering_gt[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Boolean]
  def ordering_gteq[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Boolean]
  def ordering_equiv[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Boolean]
  def ordering_max[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def ordering_min[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
}


trait OrderingOpsExp extends OrderingOps with VariablesExp {

  case class OrderingLT[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingLTEQ[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingGT[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingGTEQ[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingEquiv[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingMax[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class OrderingMin[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def ordering_lt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Rep[Boolean] = OrderingLT(lhs,rhs)
  def ordering_lteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Rep[Boolean] = OrderingLTEQ(lhs,rhs)
  def ordering_gt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Rep[Boolean] = OrderingGT(lhs,rhs)
  def ordering_gteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Rep[Boolean] = OrderingGTEQ(lhs,rhs)
  def ordering_equiv[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Rep[Boolean] = OrderingEquiv(lhs,rhs)
  def ordering_max[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Rep[T] = OrderingMax(lhs,rhs)
  def ordering_min[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Rep[T] = OrderingMin(lhs,rhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    implicit val z1: Ordering[Any] = null // hack!! need to store it in Def instances??
    implicit val z2: Ordering[A] = null // hack!! need to store it in Def instances??
    (e match {
    case OrderingLT(a,b) => ordering_lt(f(a),f(b))
    case OrderingLTEQ(a,b) => ordering_lteq(f(a),f(b))
    case OrderingGT(a,b) => ordering_gt(f(a),f(b))
    case OrderingGTEQ(a,b) => ordering_gteq(f(a),f(b))
    case OrderingEquiv(a,b) => ordering_equiv(f(a),f(b))
    case OrderingMax(a,b) => ordering_max(f(a),f(b))
    case OrderingMin(a,b) => ordering_min(f(a),f(b))
    case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]]
  }
}

trait ScalaGenOrderingOps extends ScalaGenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case OrderingLT(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
    case OrderingLTEQ(a,b) => emitValDef(sym, quote(a) + " <= " + quote(b))
    case OrderingGT(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
    case OrderingGTEQ(a,b) => emitValDef(sym, quote(a) + " >= " + quote(b))
    case OrderingEquiv(a,b) => emitValDef(sym, quote(a) + " equiv " + quote(b))
    case OrderingMax(a,b) => emitValDef(sym, quote(a) + " max " + quote(b))
    case OrderingMin(a,b) => emitValDef(sym, quote(a) + " min " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenOrderingOps extends CLikeGenBase {
  val IR: OrderingOpsExp
  import IR._
  
  // TODO: Add MIN/MAX macro needs to C-like header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case OrderingLT(a,b) =>
          emitValDef(sym, quote(a) + " < " + quote(b))
        case OrderingLTEQ(a,b) =>
          emitValDef(sym, quote(a) + " <= " + quote(b))
        case OrderingGT(a,b) =>
          emitValDef(sym, quote(a) + " > " + quote(b))
        case OrderingGTEQ(a,b) =>
          emitValDef(sym, quote(a) + " >= " + quote(b))
        case OrderingEquiv(a,b) =>
          emitValDef(sym, quote(a) + " == " + quote(b))
        case OrderingMax(a,b) =>
          emitValDef(sym, "MAX(" + quote(a) + ", " + quote(b) + ")")
        case OrderingMin(a,b) =>
          emitValDef(sym, "MIN(" + quote(a) + ", " + quote(b) + ")")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenOrderingOps extends CudaGenBase with CLikeGenOrderingOps
trait OpenCLGenOrderingOps extends OpenCLGenBase with CLikeGenOrderingOps
trait CGenOrderingOps extends CGenBase with CLikeGenOrderingOps

