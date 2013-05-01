package scala.lms
package ops

import java.io.PrintWriter
import scala.reflect.SourceContext

trait ImplicitOps extends Base {
  /**
   *  Implicit conversion from Rep[X] to Rep[Y]
   *
   *  As long as a conversion is in scope, it will be invoked in the generated scala code.
   *  Code-gen for other platforms should implement the conversions.
   **/
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y, mX: TypeRep[X], mY: TypeRep[Y], pos: SourceContext) : Rep[Y] // = x.asInstanceOf[Rep[Y]
}

trait ImplicitOpsExp extends ImplicitOps with BaseExp {
  case class ImplicitConvert[X,Y](x: Exp[X])(implicit val mX: TypeRep[X], val mY: TypeRep[Y]) extends Def[Y]

  def implicit_convert[X,Y](x: Exp[X])(implicit c: X => Y, mX: TypeRep[X], mY: TypeRep[Y], pos: SourceContext) : Rep[Y] = {
    if (mX == mY) x.asInstanceOf[Rep[Y]] else ImplicitConvert[X,Y](x)
  }

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case im@ImplicitConvert(x) => toAtom(ImplicitConvert(f(x))(im.mX,im.mY))(mtype(typeRep[A]),pos)
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
