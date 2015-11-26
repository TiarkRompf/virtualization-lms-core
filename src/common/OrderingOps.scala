package scala.lms
package common

import java.io.PrintWriter
import scala.lms.util.OverloadHack
import scala.reflect.SourceContext

trait OrderingOps extends Base with Variables with OverloadHack {
  // workaround for infix not working with implicits in PrimitiveOps
  implicit def orderingToOrderingOps[T:Ordering:Manifest](n: T) = new OrderingOpsCls(unit(n))
  implicit def repOrderingToOrderingOps[T:Ordering:Manifest](n: Rep[T]) = new OrderingOpsCls(n)
  implicit def varOrderingToOrderingOps[T:Ordering:Manifest](n: Var[T]) = new OrderingOpsCls(readVar(n))

  class OrderingOpsCls[T:Ordering:Manifest](lhs: Rep[T]){
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

  def ordering_lt      [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_lteq    [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_gt      [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_gteq    [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_equiv   [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
  def ordering_max     [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def ordering_min     [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def ordering_compare [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Int]
}


trait OrderingOpsExp extends OrderingOps with VariablesExp {
  abstract class DefMN[T:Ordering:Manifest,A] extends Def[A] {
    def mev = manifest[T]
    def aev = implicitly[Ordering[T]]
  }
  case class OrderingLT      [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingLTEQ    [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingGT      [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingGTEQ    [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingEquiv   [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingMax     [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]
  case class OrderingMin     [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]
  case class OrderingCompare [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Int]

  def ordering_lt     [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingLT(lhs,rhs)
  def ordering_lteq   [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingLTEQ(lhs,rhs)
  def ordering_gt     [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingGT(lhs,rhs)
  def ordering_gteq   [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingGTEQ(lhs,rhs)
  def ordering_equiv  [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingEquiv(lhs,rhs)
  def ordering_max    [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T]       = OrderingMax(lhs,rhs)
  def ordering_min    [T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T]       = OrderingMin(lhs,rhs)
  def ordering_compare[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Int]     = OrderingCompare(lhs,rhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@OrderingLT(a,b)                      => ordering_lt(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingLTEQ(a,b)                    => ordering_lteq(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingGT(a,b)                      => ordering_gt(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingGTEQ(a,b)                    => ordering_gteq(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingEquiv(a,b)                   => ordering_equiv(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingMax(a,b)                     => ordering_max(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev),pos)
    case e@OrderingMin(a,b)                     => ordering_min(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev),pos)
    case e@OrderingCompare(a,b)                 => ordering_compare(f(a),f(b))(e.aev,e.mev,pos)
    case Reflect(e@OrderingLT(a,b), u, es)      => reflectMirrored(Reflect(OrderingLT(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@OrderingLTEQ(a,b), u, es)    => reflectMirrored(Reflect(OrderingLTEQ(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@OrderingGT(a,b), u, es)      => reflectMirrored(Reflect(OrderingGT(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@OrderingGTEQ(a,b), u, es)    => reflectMirrored(Reflect(OrderingGTEQ(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@OrderingEquiv(a,b), u, es)   => reflectMirrored(Reflect(OrderingEquiv(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@OrderingMax(a,b), u, es)     => reflectMirrored(Reflect(OrderingMax(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@OrderingMin(a,b), u, es)     => reflectMirrored(Reflect(OrderingMin(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@OrderingCompare(a,b), u, es) => reflectMirrored(Reflect(OrderingCompare(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

/**
 * @author  Alen Stojanov (astojanov@inf.ethz.ch)
 */
trait OrderingOpsExpOpt extends OrderingOpsExp {

  override def ordering_lt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].lt(a, b))
    case (a, b) if a.equals(b) => Const(false)
    case _ => super.ordering_lt(lhs, rhs)
  }

  override def ordering_lteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].lteq(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_lteq(lhs, rhs)
  }

  override def ordering_gt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].gt(a, b))
    case (a, b) if a.equals(b) => Const(false)
    case _ => super.ordering_gt(lhs, rhs)
  }

  override def ordering_gteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].gteq(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_gteq(lhs, rhs)
  }

  override def ordering_equiv[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].equiv(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_equiv(lhs, rhs)
  }

  override def ordering_max[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].max(a, b))
    case (a, b) if a.equals(b) => a
    case _ => super.ordering_max(lhs, rhs)
  }

  override def ordering_min[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].min(a, b))
    case (a, b) if a.equals(b) => a
    case _ => super.ordering_min(lhs, rhs)
  }

  override def ordering_compare[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Int] = (lhs, rhs) match {
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
    // "$a max $b" is wrong for Strings because it tries to use `StringLike.max(Ordering)`
    case c@OrderingMax(a,b) =>
      val rhs = if (c.mev == manifest[String])
        src"scala.math.Ordering.String.max($a, $b)"
      else
        src"$a max $b"
      emitValDef(sym, rhs)
    case c@OrderingMin(a,b) =>
      val rhs = if (c.mev == manifest[String])
        src"scala.math.Ordering.String.min($a, $b)"
      else
        src"$a min $b"
      emitValDef(sym, rhs)
    case c@OrderingCompare(a,b) => c.mev match {
      case m if m == Manifest.Int => emitValDef(sym, "java.lang.Integer.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Long => emitValDef(sym, "java.lang.Long.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Double => emitValDef(sym, "java.lang.Double.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Float => emitValDef(sym, "java.lang.Float.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Boolean => emitValDef(sym, "java.lang.Boolean.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Byte => emitValDef(sym, "java.lang.Byte.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Char => emitValDef(sym, "java.lang.Character.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Short => emitValDef(sym, "java.lang.Short.compare("+quote(a)+","+quote(b)+")")
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

