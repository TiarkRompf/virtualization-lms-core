package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftBoolean {
  this: Base =>

  implicit def boolToBoolRep(b: Boolean) = unit(b)
}

trait BooleanOps extends Variables {
  def infix_unary_!(x: Rep[Boolean])(implicit pos: SourceContext) = boolean_negate(x)
  def infix_&&(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext) = boolean_and(lhs,rhs)
  def infix_||(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext) = boolean_or(lhs,rhs)

  def boolean_negate(lhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with BaseExp {
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanOr(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]

  def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanAnd(lhs,rhs)
  def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanOr(lhs,rhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case BooleanNegate(x) => boolean_negate(f(x))
    case BooleanAnd(x,y) => boolean_and(f(x),f(y))
    case BooleanOr(x,y) => boolean_or(f(x),f(y))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenBooleanOps extends ScalaGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
    case BooleanOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " || " + quote(rhs))
    case _ => super.emitNode(sym,rhs)
  }
}

trait CLikeGenBooleanOps extends CLikeGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case BooleanNegate(b) =>
        emitValDef(sym, "!" + quote(b))
      case _ => super.emitNode(sym,rhs)
    }
  }
}

trait CudaGenBooleanOps extends CudaGenBase with CLikeGenBooleanOps
trait OpenCLGenBooleanOps extends OpenCLGenBase with CLikeGenBooleanOps
trait CGenBooleanOps extends CGenBase with CLikeGenBooleanOps
