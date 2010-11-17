package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import java.io.PrintWriter


trait ImplicitOps extends Base {
  /**
   *  Implicit conversion from Rep[X] to Rep[Y]
   *
   *  As long as a conversion is in scope, it will be invoked in the generated scala code.
   *  Code-gen for other platforms should implement the conversions.
   **/
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y) : Rep[Y] // = x.asInstanceOf[Rep[Y]
}

trait ImplicitOpsExp extends ImplicitOps with BaseExp {
  case class ImplicitConvert[X,Y](x: Exp[X]) extends Def[Y]

  def implicit_convert[X,Y](x: Exp[X])(implicit c: X => Y) : Rep[Y] = ImplicitConvert[X,Y](x)
}

trait ScalaGenImplicit extends ScalaGenBase {
  val IR: ImplicitOpsExp
  import IR._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // TODO: this valDef is redundant; we really just want the conversion to be a no-op in the generated code.
    // TODO: but we still need to link the defs together
    case ImplicitConvert(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
}
