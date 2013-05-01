package scala.lms
package ops

import java.io.PrintWriter
import scala.reflect.SourceContext

trait FractionalOps extends ImplicitOps {
  def infix_/[A,T](lhs: Rep[T], rhs: Rep[A])(implicit c: A => T, f: Fractional[T], mA: TypeRep[A], mT: TypeRep[T], pos: SourceContext) = fractional_divide(lhs,implicit_convert[A,T](rhs))

  def fractional_divide[T:Fractional:TypeRep](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
}

trait FractionalOpsExp extends FractionalOps with ImplicitOpsExp {

  case class FractionalDivide[T](lhs: Exp[T], rhs: Exp[T])(implicit f: Fractional[T], val mT: TypeRep[T]) extends Def[T]

  def fractional_divide[T:Fractional:TypeRep](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Rep[T] = FractionalDivide(lhs, rhs)
}

trait ScalaGenFractionalOps extends ScalaGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FractionalDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}


