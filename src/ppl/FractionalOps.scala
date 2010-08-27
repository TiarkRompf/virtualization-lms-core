package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter


trait FractionalOps extends Base with OverloadHack with ImplicitOps {

  implicit def repFractionalToFractionalOps[T](x: Rep[T])(implicit f: Fractional[T]) = new FractionalOpsCls(x,f)
  implicit def fractionalToFractionalOps[T](x: T)(implicit f: Fractional[T]) = new FractionalOpsCls(x,f)

  class FractionalOpsCls[T](lhs: Rep[T], implicit val f: Fractional[T]) {
    def /(rhs: Rep[T]) : Rep[T] = fractional_divide(lhs,rhs)
    // TODO: why does this not work, but the uncommented one does?
    //def /[A](rhs: Rep[A])(implicit c: A => T) : Rep[T] = fractional_divide(lhs,implicit_convert[A,T](rhs))
    def /(rhs: Rep[Int])(implicit c: Int => T) : Rep[T] = fractional_divide(lhs,implicit_convert[Int,T](rhs))
  }

  def fractional_divide[T](lhs: Rep[T], rhs: Rep[T])(implicit f: Fractional[T]): Rep[T]
}

trait FractionalOpsExp extends FractionalOps with BaseExp {
  case class FractionalDivide[T](lhs: Exp[T], rhs: Exp[T], implicit val f: Fractional[T]) extends Def[T]
  
  def fractional_divide[T](lhs: Exp[T], rhs: Exp[T])(implicit f: Fractional[T]) : Rep[T] = FractionalDivide(lhs, rhs, f)
}

trait ScalaGenFractional extends ScalaGenBase  { this: FractionalOpsExp =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case FractionalDivide(a,b,f) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
