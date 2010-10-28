package scala.virtualization.lms
package common.embedded.scala

import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaGenEffect
import scala.virtualization.lms.common.{EffectExp}

trait DSLOpsExp extends EffectExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).
  case class DSLOp[A](val representation: Exp[A]) extends Def[A]
}

trait ScalaGenDSLOps extends ScalaGenEffect {
  val IR: DSLOpsExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case op@DSLOp(b) =>
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
