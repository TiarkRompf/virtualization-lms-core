package scala.virtualization.lms
package common

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext

trait LiftBoolean {
  this: Base =>

  implicit def boolToBoolRep(b: Boolean) = unit(b)
}

trait BooleanOps extends Variables {
  implicit def var2BooleanOps(x: Var[Boolean]) = new BooleanOps(readVar(x))
  implicit class BooleanOps(x: Rep[Boolean]) {
    def unary_!(implicit pos: SourceContext) = boolean_negate(x)
    def &&(rhs: =>Rep[Boolean])(implicit pos: SourceContext) = boolean_and(x,rhs)
    def ||(rhs: =>Rep[Boolean])(implicit pos: SourceContext) = boolean_or(x,rhs)
  }
  //def infix_unary_!(x: Rep[Boolean])(implicit pos: SourceContext) = boolean_negate(x)
  //def infix_&&(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext) = boolean_and(lhs,rhs)
  //def infix_||(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext) = boolean_or(lhs,rhs)

  def boolean_negate(lhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with EffectExp {
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

    case Reflect(BooleanNegate(x), u, es) => reflectMirrored(Reflect(BooleanNegate(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(BooleanAnd(x,y), u, es) => reflectMirrored(Reflect(BooleanAnd(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(BooleanOr(x,y), u, es) => reflectMirrored(Reflect(BooleanOr(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait BooleanOpsExpOpt extends BooleanOpsExp {
  override def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) = lhs match {
    case Def(BooleanNegate(x)) => x
    case _ => super.boolean_negate(lhs)
  }
}

trait ScalaGenBooleanOps extends ScalaGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, src"!$b")
    case BooleanAnd(lhs,rhs) => emitValDef(sym, src"$lhs && $rhs")
    case BooleanOr(lhs,rhs) => emitValDef(sym, src"$lhs || $rhs")
    case _ => super.emitNode(sym,rhs)
  }
}

trait CLikeGenBooleanOps extends CLikeGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, src"!$b")
    case BooleanAnd(lhs,rhs) => emitValDef(sym, src"$lhs && $rhs")
    case BooleanOr(lhs,rhs) => emitValDef(sym, src"$lhs || $rhs")
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenBooleanOps extends CudaGenBase with CLikeGenBooleanOps
trait OpenCLGenBooleanOps extends OpenCLGenBase with CLikeGenBooleanOps
trait CGenBooleanOps extends CGenBase with CLikeGenBooleanOps
