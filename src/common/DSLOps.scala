package scala.virtualization.lms
package common

import scala.virtualization.lms.internal.ScalaGenEffect
import java.io.PrintWriter

trait DSLOpsExp extends EffectExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).
  class DSLOp[A](val representation: Exp[A]) extends Def[A]
}

trait ScalaGenDSLOps extends ScalaGenEffect {
  val IR: DSLOpsExp
  import IR._
  
  // TODO: think about whether this should override syms for DSLOps or not

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case op: DSLOp[_] => getFreeVarBlock(op.representation,Nil)
    case _ => super.getFreeVarNode(rhs)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case op: DSLOp[_] =>
      val b = op.representation
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }

}
