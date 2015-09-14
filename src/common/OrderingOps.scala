package scala.lms
package common

import java.io.PrintWriter
import scala.lms.util.OverloadHack
import scala.reflect.SourceContext
import scala.lms.internal._

trait OrderingOps extends Base with Variables with BooleanOps with PrimitiveOps with OverloadHack {
  // workaround for infix not working with implicits in PrimitiveOps
  implicit def orderingToOrderingOps[T:Ordering:Typ](n: T) = new OrderingOpsCls(unit(n))
  implicit def repOrderingToOrderingOps[T:Ordering:Typ](n: Rep[T]) = new OrderingOpsCls(n)
  implicit def varOrderingToOrderingOps[T:Ordering:Typ](n: Var[T])(implicit pos: SourceContext) = new OrderingOpsCls(readVar(n))

  class OrderingOpsCls[T:Ordering:Typ](lhs: Rep[T]){
    def <       (rhs: Rep[T])(implicit pos: SourceContext) = ordering_lt(lhs, rhs)
    def <=      (rhs: Rep[T])(implicit pos: SourceContext) = ordering_lteq(lhs, rhs)
    def >       (rhs: Rep[T])(implicit pos: SourceContext) = ordering_gt(lhs, rhs)
    def >=      (rhs: Rep[T])(implicit pos: SourceContext) = ordering_gteq(lhs, rhs)
    def equiv   (rhs: Rep[T])(implicit pos: SourceContext) = ordering_equiv(lhs, rhs)
    def max     (rhs: Rep[T])(implicit pos: SourceContext) = ordering_max(lhs, rhs)
    def min     (rhs: Rep[T])(implicit pos: SourceContext) = ordering_min(lhs, rhs)
    def compare (rhs: Rep[T])(implicit pos: SourceContext) = ordering_compare(lhs, rhs)

    def <       [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_lt(lhs, c(rhs))
    def <=      [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_lteq(lhs, c(rhs))
    def >       [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_gt(lhs, c(rhs))
    def >=      [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_gteq(lhs, c(rhs))
    def equiv   [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_equiv(lhs, c(rhs))
    def max     [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_max(lhs, c(rhs))
    def min     [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_min(lhs, c(rhs))
    def compare [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_compare(lhs, c(rhs))
  }

  def ordering_lt      [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_lteq    [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_gt      [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_gteq    [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_equiv   [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_max     [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def ordering_min     [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def ordering_compare [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Int]
}


trait OrderingOpsExp extends OrderingOps with VariablesExp {

  abstract class OrderingDef2[A:Ordering:Typ,R:Typ] extends Def2[A,R] {
    val oA = implicitly[Ordering[A]]
  }

  case class OrderingLT      [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,Boolean]
  case class OrderingLTEQ    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,Boolean]
  case class OrderingGT      [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,Boolean]
  case class OrderingGTEQ    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,Boolean]
  case class OrderingEquiv   [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,Boolean]
  case class OrderingMax     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,T]
  case class OrderingMin     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,T]
  case class OrderingCompare [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDef2[T,Int]

  def ordering_lt     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = toAtom(OrderingLT(lhs,rhs))
  def ordering_lteq   [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = toAtom(OrderingLTEQ(lhs,rhs))
  def ordering_gt     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = toAtom(OrderingGT(lhs,rhs))
  def ordering_gteq   [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = toAtom(OrderingGTEQ(lhs,rhs))
  def ordering_equiv  [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = toAtom(OrderingEquiv(lhs,rhs))
  def ordering_max    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T]       = toAtom(OrderingMax(lhs,rhs))
  def ordering_min    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T]       = toAtom(OrderingMin(lhs,rhs))
  def ordering_compare[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Int]     = toAtom(OrderingCompare(lhs,rhs))

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@OrderingLT(a,b) => ordering_lt(f(a),f(b))(e.oA,e.mA,pos)
    case e@OrderingLTEQ(a,b) => ordering_lteq(f(a),f(b))(e.oA,e.mA,pos)
    case e@OrderingGT(a,b) => ordering_gt(f(a),f(b))(e.oA,e.mA,pos)
    case e@OrderingGTEQ(a,b) => ordering_gteq(f(a),f(b))(e.oA,e.mA,pos)
    case e@OrderingEquiv(a,b) => ordering_equiv(f(a),f(b))(e.oA,e.mA,pos)
    case e@OrderingMax(a,b) => ordering_max(f(a),f(b))(otype(e.oA),mtype(e.mA),pos)
    case e@OrderingMin(a,b) => ordering_min(f(a),f(b))(otype(e.oA),mtype(e.mA),pos)
    case e@OrderingCompare(a,b) => ordering_compare(f(a),f(b))(e.oA,e.mA,pos)
    case _ => super.mirror(e, f)
  }
}

/**
 * @author  Alen Stojanov (astojanov@inf.ethz.ch)
 */
trait OrderingOpsExpOpt extends OrderingOpsExp {

  override def ordering_lt[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].lt(a, b))
    case (a, b) if a.equals(b) => Const(false)
    case _ => super.ordering_lt(lhs, rhs)
  }

  override def ordering_lteq[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].lteq(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_lteq(lhs, rhs)
  }

  override def ordering_gt[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].gt(a, b))
    case (a, b) if a.equals(b) => Const(false)
    case _ => super.ordering_gt(lhs, rhs)
  }

  override def ordering_gteq[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].gteq(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_gteq(lhs, rhs)
  }

  override def ordering_equiv[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].equiv(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_equiv(lhs, rhs)
  }

  override def ordering_max[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].max(a, b))
    case (a, b) if a.equals(b) => a
    case _ => super.ordering_max(lhs, rhs)
  }

  override def ordering_min[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].min(a, b))
    case (a, b) if a.equals(b) => a
    case _ => super.ordering_min(lhs, rhs)
  }

  override def ordering_compare[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Int] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].compare(a, b))
    case (a, b) if a.equals(b) => Const[Int](0)
    case _ => super.ordering_compare(lhs, rhs)
  }

}


trait ScalaGenOrderingOps extends ScalaGenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingLT(a,b) => emitValDef(sym, src"$a < $b")
    case OrderingLTEQ(a,b) => emitValDef(sym, src"$a <= $b")
    case OrderingGT(a,b) => emitValDef(sym, src"$a > $b")
    case OrderingGTEQ(a,b) => emitValDef(sym, src"$a >= $b")
    case OrderingEquiv(a,b) => emitValDef(sym, src"$a equiv $b")
    case OrderingMax(a,b) => emitValDef(sym, src"$a max $b")
    case OrderingMin(a,b) => emitValDef(sym, src"$a min $b")
    case c@OrderingCompare(a,b) => c.mA match {
      case m if m == typ[Int] => emitValDef(sym, "java.lang.Integer.compare("+quote(a)+","+quote(b)+")")
      case m if m == typ[Long] => emitValDef(sym, "java.lang.Long.compare("+quote(a)+","+quote(b)+")")
      case m if m == typ[Double] => emitValDef(sym, "java.lang.Double.compare("+quote(a)+","+quote(b)+")")
      case m if m == typ[Float] => emitValDef(sym, "java.lang.Float.compare("+quote(a)+","+quote(b)+")")
      case m if m == typ[Boolean] => emitValDef(sym, "java.lang.Boolean.compare("+quote(a)+","+quote(b)+")")
      case m if m == typ[Byte] => emitValDef(sym, "java.lang.Byte.compare("+quote(a)+","+quote(b)+")")
      case m if m == typ[Char] => emitValDef(sym, "java.lang.Character.compare("+quote(a)+","+quote(b)+")")
      case m if m == typ[Short] => emitValDef(sym, "java.lang.Short.compare("+quote(a)+","+quote(b)+")")
      case _ => emitValDef(sym, quote(a) + " compare " + quote(b))
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenOrderingOps extends CLikeGenBase {
  val IR: OrderingOpsExp
  import IR._

  // TODO: Add MIN/MAX macro needs to C-like header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case OrderingLT(a,b) =>
          emitValDef(sym, src"$a < $b")
        case OrderingLTEQ(a,b) =>
          emitValDef(sym, src"$a <= $b")
        case OrderingGT(a,b) =>
          emitValDef(sym, src"$a > $b")
        case OrderingGTEQ(a,b) =>
          emitValDef(sym, src"$a >= $b")
        case OrderingEquiv(a,b) =>
          emitValDef(sym, src"$a == $b")
        case OrderingMax(a,b) =>
          //emitValDef(sym, quote(a) + ">" + quote(b) + "?" + quote(a) + ":" + quote(b))
          emitValDef(sym, src"MAX($a, $b)")
        case OrderingMin(a,b) =>
          //emitValDef(sym, quote(a) + "<" + quote(b) + "?" + quote(a) + ":" + quote(b))
          emitValDef(sym, src"MIN($a, $b)")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenOrderingOps extends CudaGenBase with CLikeGenOrderingOps
trait OpenCLGenOrderingOps extends OpenCLGenBase with CLikeGenOrderingOps
trait CGenOrderingOps extends CGenBase with CLikeGenOrderingOps

