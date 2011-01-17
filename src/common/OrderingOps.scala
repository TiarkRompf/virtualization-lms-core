package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.util.OverloadHack

trait OrderingOps extends Base with Variables with OverloadHack {  
  // workaround for infix not working with manifests
  implicit def orderingToRepOrderingCls[T:Ordering:Manifest](n: T) = new OrderingOpsCls(n)
  implicit def repOrderingToRepOrderingCls[T:Ordering:Manifest](n: Rep[T]) = new OrderingOpsCls(n)
  implicit def varOrderingToOrderingOps[T:Ordering:Manifest](n: Var[T]) = new OrderingOpsCls(readVar(n))
  
  class OrderingOpsCls[T:Ordering:Manifest](lhs: Rep[T]){
    def <[B](rhs: B)(implicit c: B => Rep[T]) = ordering_lt(lhs, c(rhs))
    def <=[B](rhs: B)(implicit c: B => Rep[T]) = ordering_lteq(lhs, c(rhs))
    def >[B](rhs: B)(implicit c: B => Rep[T]) = ordering_gt(lhs, c(rhs))
    def >=[B](rhs: B)(implicit c: B => Rep[T]) = ordering_gteq(lhs, c(rhs))
    def equiv[B](rhs: B)(implicit c: B => Rep[T]) = ordering_equiv(lhs, c(rhs))
    def max[B](rhs: B)(implicit c: B => Rep[T]) = ordering_max(lhs, c(rhs))
    def min[B](rhs: B)(implicit c: B => Rep[T]) = ordering_min(lhs, c(rhs))
  }
  
  /*
  def infix_<[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T, mT: Manifest[T]) = ordering_lt(lhs,c(rhs))
  def infix_<=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T, mT: Manifest[T]) = ordering_lteq(lhs,c(rhs))
  def infix_>[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T, mT: Manifest[T]) = ordering_gt(lhs,c(rhs))
  def infix_>=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T, mT: Manifest[T]) = ordering_gteq(lhs,c(rhs))
  def infix_equiv[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T, mT: Manifest[T]) = ordering_equiv(lhs,c(rhs))
  def infix_max[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T, mT: Manifest[T]) = ordering_max(lhs,c(rhs))
  def infix_min[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T, mT: Manifest[T]) = ordering_min(lhs,c(rhs))
  */

  def ordering_lt[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_lteq[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gt[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gteq[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_equiv[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_max[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def ordering_min[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
}


trait OrderingOpsExp extends OrderingOps with BaseExp {

  case class OrderingLT[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingLTEQ[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingGT[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingGTEQ[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingEquiv[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingMax[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class OrderingMin[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def ordering_lt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingLT(lhs,rhs)
  def ordering_lteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingLTEQ(lhs,rhs)
  def ordering_gt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingGT(lhs,rhs)
  def ordering_gteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingGTEQ(lhs,rhs)
  def ordering_equiv[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingEquiv(lhs,rhs)
  def ordering_max[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[T] = OrderingMax(lhs,rhs)
  def ordering_min[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[T] = OrderingMin(lhs,rhs)
}

trait ScalaGenOrderingOps extends ScalaGenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
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

trait CudaGenOrderingOps extends CudaGenBase {
  val IR: OrderingOpsExp
  import IR._
  
  // TODO: Add MIN/MAX macro needs to C-like header file
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
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
