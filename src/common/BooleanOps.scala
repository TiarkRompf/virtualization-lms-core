package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.internal._

trait LiftBoolean {
  this: Base =>

  implicit def boolToBoolRep(b: Boolean) = unit(b)
}

trait BooleanOps extends Variables with Expressions {
  def infix_unary_!(x: Rep[Boolean])(implicit pos: SourceContext) = boolean_negate(x)
  def infix_&&(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext) = boolean_and(lhs,rhs)
  def infix_&&(lhs: Boolean, rhs: => Rep[Boolean])(implicit pos: SourceContext): Exp[Boolean] = {
    if (lhs == true) rhs.asInstanceOf[Exp[Boolean]]
    else Const(false)
  }
  def infix_||(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext) = boolean_or(lhs,rhs)
  def infix_||(lhs: Boolean, rhs: Rep[Boolean])(implicit pos: SourceContext): Exp[Boolean] = {
    if (lhs == true) Const(true)
    else rhs.asInstanceOf[Exp[Boolean]]
  }

  def boolean_negate(lhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with BaseExp with EffectExp {
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Block[Boolean]) extends Def[Boolean]
  case class BooleanOr(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]

  def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], frhs: => Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = {
    lhs match {
        case x@Const(true) => x
        case _ => {
            val rhs = reifyEffects(frhs)
            BooleanAnd(lhs,rhs)
        }
    }
  }
  def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = {
    lhs match {
        case x@Const(false) => rhs
        case _ => BooleanOr(lhs,rhs)
    }
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case BooleanNegate(x) => boolean_negate(f(x))
    case BooleanAnd(x,y) => BooleanAnd(f(x),y)
    case BooleanOr(x,y) => boolean_or(f(x),f(y))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case BooleanAnd(x,y) => effectSyms(x) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }

}

trait BooleanOpsExpOpt extends BooleanOpsExp {
  override def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) = lhs match {
    case Def(BooleanNegate(x)) => x
    case _ => super.boolean_negate(lhs)
  }
}

trait ScalaGenBooleanOps extends ScalaGenBase with GenericNestedCodegen {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => 
        emitValDef(sym, "if (" + quote(lhs) + " == true) {")
        emitBlock(rhs)
        stream.println(quote(getBlockResult(rhs)))
        stream.println("} else false")
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
