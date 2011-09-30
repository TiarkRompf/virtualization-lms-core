package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext

trait CastingOps extends Variables with OverloadHack {
  this: ImplicitOps =>

  //implicit def anyToCastingOps[A:Manifest](lhs: A) = new CastingOpsCls(lhs)
  implicit def repAnyToCastingOps[A:Manifest](lhs: Rep[A]) = new CastingOpsCls(lhs)
  implicit def varAnyToCastingOps[A:Manifest](lhs: Var[A]) = new CastingOpsCls(readVar(lhs))
    
  class CastingOpsCls[A:Manifest](lhs: Rep[A]){
    def isInstanceOfL[B:Manifest]: Rep[Boolean] = rep_isinstanceof(lhs, manifest[A], manifest[B])
    def asInstanceOfL[B:Manifest]: Rep[B] = rep_asinstanceof(lhs, manifest[A], manifest[B])
  }

  def rep_isinstanceof[A,B](lhs: Rep[A], mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean]
  def rep_asinstanceof[A,B:Manifest](lhs: Rep[A], mA: Manifest[A], mB: Manifest[B]) : Rep[B]
}

trait CastingOpsExp extends CastingOps with BaseExp {
  this: ImplicitOps =>

  case class RepIsInstanceOf[A,B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B]) extends Def[Boolean]
  case class RepAsInstanceOf[A,B:Manifest](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B]) extends Def[B]

  def rep_isinstanceof[A,B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B]) = RepIsInstanceOf(lhs,mA,mB)
  def rep_asinstanceof[A,B:Manifest](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B]) : Exp[B] = RepAsInstanceOf(lhs,mA,mB)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case RepAsInstanceOf(lhs, mA, mB) => rep_asinstanceof(f(lhs), mA,mB)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenCastingOps extends ScalaGenBase {
  val IR: CastingOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case RepIsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".isInstanceOf[" + remap(mB) + "]")
    case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".asInstanceOf[" + remap(mB) + "]")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenCastingOps extends CudaGenBase {
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        //TODO: How about carrying the dynamic type information in C datastructure?
        case RepIsInstanceOf(x,mA,mB) => throw new RuntimeException("CudaGen: Cannot check runtime type")
        case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, "(%s) %s".format(remap(mB),quote(x)))
        case _ => super.emitNode(sym, rhs)
      }
    }
}
