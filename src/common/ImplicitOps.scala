package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenBase, ScalaGenBase}

trait ImplicitOps extends Base {
  /**
   *  Implicit conversion from Rep[X] to Rep[Y]
   *
   *  As long as a conversion is in scope, it will be invoked in the generated scala code.
   *  Code-gen for other platforms should implement the conversions.
   **/
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y]) : Rep[Y] // = x.asInstanceOf[Rep[Y]
}

trait ImplicitOpsExp extends ImplicitOps with BaseExp {
  case class ImplicitConvert[X,Y](x: Exp[X])(implicit val mX: Manifest[X], val mY: Manifest[Y]) extends Def[Y]

  def implicit_convert[X,Y](x: Exp[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y]) : Rep[Y] = ImplicitConvert[X,Y](x)

}

trait ScalaGenImplicitOps extends ScalaGenBase {
  val IR: ImplicitOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // TODO: this valDef is redundant; we really just want the conversion to be a no-op in the generated code.
    // TODO: but we still need to link the defs together
    case ImplicitConvert(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenImplicitOps extends CudaGenBase {
  val IR: ImplicitOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        // TODO: this valDef is redundant; we really just want the conversion to be a no-op in the generated code.
        // TODO: but we still need to link the defs together
        case im@ImplicitConvert(x) =>
          if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
          else stream.println(addTab()+"%s %s = (%s)%s;".format(CudaType(im.mY.toString), quote(sym), CudaType(im.mY.toString), quote(x)))
        case _ => super.emitNode(sym, rhs)
      }
    }
}
