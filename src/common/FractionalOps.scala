package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext

trait FractionalOps extends ImplicitOps {
  def infix_/[A,T](lhs: Rep[T], rhs: Rep[A])(implicit c: A => T, f: Fractional[T], mA: Manifest[A], mT: Manifest[T], pos: SourceContext) = fractional_divide(lhs,implicit_convert[A,T](rhs))

  def fractional_divide[T:Fractional:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
}

trait FractionalOpsExp extends FractionalOps with ImplicitOpsExp with EffectExp {
  
  case class FractionalDivide[T](lhs: Exp[T], rhs: Exp[T])(implicit val f: Fractional[T], val mT: Manifest[T]) extends Def[T]

  def fractional_divide[T:Fractional:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Rep[T] = FractionalDivide(lhs, rhs)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@FractionalDivide(a,b) => fractional_divide(f(a),f(b))(e.f.asInstanceOf[Fractional[A]],mtype(e.mT),pos)
    case Reflect(e@FractionalDivide(a,b), u, es) => reflectMirrored(Reflect(FractionalDivide(f(a),f(b))(e.f.asInstanceOf[Fractional[A]],mtype(e.mT)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)        
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]    
}

trait ScalaGenFractionalOps extends ScalaGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FractionalDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenFractionalOps extends CLikeGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case FractionalDivide(a,b) =>
          emitValDef(sym, quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
     }
    }
}

trait CudaGenFractionalOps extends CudaGenBase with CLikeGenFractionalOps
trait OpenCLGenFractionalOps extends OpenCLGenBase with CLikeGenFractionalOps
trait CGenFractionalOps extends CGenBase with CLikeGenFractionalOps
