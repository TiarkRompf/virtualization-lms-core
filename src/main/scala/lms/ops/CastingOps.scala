package scala.lms
package ops

import util.OverloadHack

import java.io.PrintWriter
import scala.reflect.SourceContext

trait CastingOps extends Variables with OverloadHack {
  this: ImplicitOps =>

  //implicit def anyToCastingOps[A:TypeRep](lhs: A) = new CastingOpsCls(lhs)
  implicit def repAnyToCastingOps[A:TypeRep](lhs: Rep[A]) = new CastingOpsCls(lhs)
  implicit def varAnyToCastingOps[A:TypeRep](lhs: Var[A]) = new CastingOpsCls(readVar(lhs))

  class CastingOpsCls[A:TypeRep](lhs: Rep[A]){
    def IsInstanceOf[B:TypeRep](implicit pos: SourceContext): Rep[Boolean] = rep_isinstanceof(lhs, typeRep[A], typeRep[B])
    def AsInstanceOf[B:TypeRep](implicit pos: SourceContext): Rep[B] = rep_asinstanceof(lhs, typeRep[A], typeRep[B])
  }

  def rep_isinstanceof[A,B](lhs: Rep[A], mA:TypeRep[A], mB:TypeRep[B])(implicit pos: SourceContext) : Rep[Boolean]
  def rep_asinstanceof[A,B:TypeRep](lhs: Rep[A], mA:TypeRep[A], mB:TypeRep[B])(implicit pos: SourceContext) : Rep[B]
}

trait CastingOpsExp extends CastingOps with BaseExp with EffectExp {
  this: ImplicitOps =>

  case class RepIsInstanceOf[A,B](lhs: Exp[A], mA:TypeRep[A], mB:TypeRep[B]) extends Def[Boolean]
  case class RepAsInstanceOf[A,B:TypeRep](lhs: Exp[A], mA:TypeRep[A], mB:TypeRep[B]) extends Def[B]

  def rep_isinstanceof[A,B](lhs: Exp[A], mA:TypeRep[A], mB:TypeRep[B])(implicit pos: SourceContext) = RepIsInstanceOf(lhs,mA,mB)
  def rep_asinstanceof[A,B:TypeRep](lhs: Exp[A], mA:TypeRep[A], mB:TypeRep[B])(implicit pos: SourceContext) : Exp[B] = toAtom(RepAsInstanceOf(lhs,mA,mB))(mB,pos)

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case RepAsInstanceOf(lhs, mA, mB) => rep_asinstanceof(f(lhs),mA,mB)(mtype(mB),pos)
    case Reflect(e@RepAsInstanceOf(lhs, mA, mB), u, es) => reflectMirrored(Reflect(RepAsInstanceOf(f(lhs),mA,mB)(mtype(mB)), mapOver(f,u), f(es)))(mtype(typeRep[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenCastingOps extends ScalaGenBase {
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case RepIsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".isInstanceOf[" + remap(mB) + "]")
    case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".asInstanceOf[" + remap(mB) + "]")
    case _ => super.emitNode(sym, rhs)
  }
}
