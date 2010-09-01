package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import java.io.PrintWriter


trait BooleanOps extends Base {

  implicit def repBooleanToBooleanOps(x: Rep[Boolean]) = new BooleanOpsCls(x)
  implicit def booleanToBooleanOps(x: Boolean) = new BooleanOpsCls(x)

  class BooleanOpsCls(lhs: Rep[Boolean]) {
    def unary_! = boolean_negate(lhs)
  }

  def boolean_negate(lhs: Rep[Boolean]): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with BaseExp {
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  
  def boolean_negate(lhs: Exp[Boolean]) : Rep[Boolean] = BooleanNegate(lhs)
}

trait ScalaGenBoolean extends ScalaGenBase with BooleanOpsExp {

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
