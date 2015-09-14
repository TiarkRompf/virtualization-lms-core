package scala.lms
package common

import scala.reflect.SourceContext
import scala.lms.internal._

import java.io.PrintWriter

trait FractionalOps extends ImplicitOps {
  def infix_/[A,T](lhs: Rep[T], rhs: Rep[A])(implicit c: A => T, f: Fractional[T], mA: Typ[A], mT: Typ[T], pos: SourceContext) = fractional_divide(lhs,implicit_convert[A,T](rhs))

  def fractional_divide[T:Fractional:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
}

trait FractionalOpsExp extends FractionalOps with ImplicitOpsExp {

  case class FractionalDivide[T](lhs: Exp[T], rhs: Exp[T])(implicit val f: Fractional[T], val mT: Typ[T]) extends Def[T]

  def fractional_divide[T:Fractional:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Rep[T] = toAtom(FractionalDivide(lhs, rhs))

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@FractionalDivide(a,b) => fractional_divide(f(a),f(b))(e.f.asInstanceOf[Fractional[A]],mtype(e.mT),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenFractionalOps extends ScalaGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FractionalDivide(a,b) => emitValDef(sym, src"$a / $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenFractionalOps extends CLikeGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case FractionalDivide(a,b) =>
          emitValDef(sym, src"$a / $b")
        case _ => super.emitNode(sym, rhs)
     }
    }
}

trait CudaGenFractionalOps extends CudaGenBase with CLikeGenFractionalOps
trait OpenCLGenFractionalOps extends OpenCLGenBase with CLikeGenFractionalOps
trait CGenFractionalOps extends CGenBase with CLikeGenFractionalOps
