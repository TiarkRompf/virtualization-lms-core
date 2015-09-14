package scala.lms
package common

import scala.reflect.SourceContext
import scala.lms.internal._

import java.io.PrintWriter

trait ImplicitOps extends Base {
  /**
   *  Implicit conversion from Rep[X] to Rep[Y]
   *
   *  As long as a conversion is in scope, it will be invoked in the generated scala code.
   *  Code-gen for other platforms should implement the conversions.
   **/
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y, mX: Typ[X], mY: Typ[Y], pos: SourceContext) : Rep[Y] // = x.asInstanceOf[Rep[Y]
}

trait ImplicitOpsExp extends ImplicitOps with BaseExp {
  case class ImplicitConvert[X,Y](x: Exp[X])(implicit val mX: Typ[X], val mY: Typ[Y]) extends Def[Y]

  def implicit_convert[X,Y](x: Exp[X])(implicit c: X => Y, mX: Typ[X], mY: Typ[Y], pos: SourceContext) : Rep[Y] = {
    if (mX == mY) x.asInstanceOf[Rep[Y]] else toAtom(ImplicitConvert[X,Y](x))
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case im@ImplicitConvert(x) => toAtom(ImplicitConvert(f(x))(im.mX,im.mY))(mtype(typ[A]),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenImplicitOps extends ScalaGenBase {
  val IR: ImplicitOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // TODO: this valDef is redundant; we really just want the conversion to be a no-op in the generated code.
    // TODO: but we still need to link the defs together
    case ImplicitConvert(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenImplicitOps extends CLikeGenBase {
  val IR: ImplicitOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case im@ImplicitConvert(x) => gen"${im.mY} $sym = (${im.mY})$x;"
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenImplicitOps extends CudaGenBase with CLikeGenImplicitOps
trait OpenCLGenImplicitOps extends OpenCLGenBase with CLikeGenImplicitOps
trait CGenImplicitOps extends CGenBase with CLikeGenImplicitOps
