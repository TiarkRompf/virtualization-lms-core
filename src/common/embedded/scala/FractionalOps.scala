package scala.virtualization.lms
package common.embedded.scala

import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaGenBase

trait FractionalOps extends ImplicitOps {
  def infix_/[A,T](lhs: Rep[T], rhs: Rep[A])(implicit c: A => T, f: Fractional[T]) = fractional_divide(lhs,implicit_convert[A,T](rhs))

  def fractional_divide[T:Fractional](lhs: Rep[T], rhs: Rep[T]): Rep[T]
}

trait FractionalOpsExp extends FractionalOps with ImplicitOpsExp {
  case class FractionalDivide[T:Fractional](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  
  def fractional_divide[T:Fractional](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = FractionalDivide(lhs, rhs)
}

trait ScalaGenFractionalOps extends ScalaGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case FractionalDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
