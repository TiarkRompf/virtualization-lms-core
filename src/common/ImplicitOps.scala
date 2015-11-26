package scala.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext

trait ImplicitOps extends Base {
  /**
   *  Implicit conversion from Rep[X] to Rep[Y]
   *
   *  As long as a conversion is in scope, it will be invoked in the generated scala code.
   *  Code-gen for other platforms should implement the conversions.
   **/
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y], pos: SourceContext) : Rep[Y] // = x.asInstanceOf[Rep[Y]
}

trait ImplicitOpsExp extends ImplicitOps with BaseExp {
  case class ImplicitConvert[X,Y](x: Exp[X], mY: Manifest[Y])(implicit val mX: Manifest[X]) extends Def[Y]

  def implicit_convert[X,Y](x: Exp[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y], pos: SourceContext) : Rep[Y] = {
    if (mX == mY) x.asInstanceOf[Rep[Y]] else ImplicitConvert[X,Y](x, mY)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case im@ImplicitConvert(x, mY) => toAtom(ImplicitConvert(f(x), mY)(im.mX))(mtype(manifest[A]),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenImplicitOps extends ScalaGenBase {
  val IR: ImplicitOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // Make sure it's typed to trigger the implicit conversion
    // Otherwise we can get type mismatch in generated code
    case ImplicitConvert(x, mY) => emitTypedValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenImplicitOps extends CLikeGenBase {
  val IR: ImplicitOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ImplicitConvert(x, mY) =>
          gen"$mY $sym = ($mY)$x;"
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenImplicitOps extends CudaGenBase with CLikeGenImplicitOps
trait OpenCLGenImplicitOps extends OpenCLGenBase with CLikeGenImplicitOps
trait CGenImplicitOps extends CGenBase with CLikeGenImplicitOps
