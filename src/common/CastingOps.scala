package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import org.scala_lang.virtualized.SourceContext

trait CastingOps extends Variables with OverloadHack {
  this: ImplicitOps =>

  //implicit def anyToCastingOps[A:Manifest](lhs: A) = new CastingOpsCls(lhs)
  implicit def repAnyToCastingOps[A:Manifest](lhs: Rep[A]) = new CastingOpsCls(lhs)
  implicit def varAnyToCastingOps[A:Manifest](lhs: Var[A]) = new CastingOpsCls(readVar(lhs))
    
  class CastingOpsCls[A:Manifest](lhs: Rep[A]){
    def IsInstanceOf[B:Manifest](implicit pos: SourceContext): Rep[Boolean] = rep_isinstanceof(lhs, manifest[A], manifest[B])
    def AsInstanceOf[B:Manifest](implicit pos: SourceContext): Rep[B] = rep_asinstanceof(lhs, manifest[A], manifest[B])
  }

  def rep_isinstanceof[A,B](lhs: Rep[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext) : Rep[Boolean]
  def rep_asinstanceof[A,B:Manifest](lhs: Rep[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext) : Rep[B]
}

trait CastingOpsExp extends CastingOps with BaseExp with EffectExp {
  this: ImplicitOps =>

  case class RepIsInstanceOf[A,B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B]) extends Def[Boolean]
  case class RepAsInstanceOf[A,B:Manifest](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B]) extends Def[B]

  def rep_isinstanceof[A,B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext) = RepIsInstanceOf(lhs,mA,mB)
  def rep_asinstanceof[A,B:Manifest](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext) : Exp[B] = toAtom(RepAsInstanceOf(lhs,mA,mB))(mB,pos)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case RepAsInstanceOf(lhs, mA, mB) => rep_asinstanceof(f(lhs),mA,mB)(mtype(mB),pos)
    case Reflect(e@RepAsInstanceOf(lhs, mA, mB), u, es) => reflectMirrored(Reflect(RepAsInstanceOf(f(lhs),mA,mB)(mtype(mB)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenCastingOps extends ScalaGenBase {
  val IR: CastingOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case RepIsInstanceOf(x,mA,mB) => emitValDef(sym, src"$x.isInstanceOf[$mB]")
    case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, src"$x.asInstanceOf[$mB]")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenCastingOps extends CLikeGenBase { 
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        //case RepIsInstanceOf(x,mA,mB) => //TODO: How?
        case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, "(%s) %s".format(remapWithRef(mB),quote(x)))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenCastingOps extends CudaGenBase with CLikeGenCastingOps 
trait OpenCLGenCastingOps extends OpenCLGenBase with CLikeGenCastingOps 
trait CGenCastingOps extends CGenBase with CLikeGenCastingOps 
