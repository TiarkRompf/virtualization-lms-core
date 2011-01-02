package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CGenBase, CLikeCodegen, CudaGenBase, ScalaGenBase}

trait BooleanOps extends Variables {
  def infix_unary_!(x: Rep[Boolean]) = boolean_negate(x)
  def infix_&&(lhs: Rep[Boolean], rhs: Rep[Boolean]) = boolean_and(lhs,rhs)

  /*
  implicit def boolToRepBoolCls(b: Boolean) = new BooleanOpsCls(b)
  implicit def repBoolToRepBoolCls(b: Rep[Boolean]) = new BooleanOpsCls(b)
  implicit def varBoolToRepBoolCls(b: Var[Boolean]) = new BooleanOpsCls(readVar(b))

  class BooleanOpsCls(lhs: Rep[Boolean]){
    def &&(rhs: Rep[Boolean]) = boolean_and(lhs,rhs)
  }
  */

  def boolean_negate(lhs: Rep[Boolean]): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean]): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with BaseExp {
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]

  def boolean_negate(lhs: Exp[Boolean]) : Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean]) : Exp[Boolean] = BooleanAnd(lhs,rhs)
}

trait ScalaGenBooleanOps extends ScalaGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
    case _ => super.emitNode(sym,rhs)
  }
}

trait CLikeGenBooleanOps extends CLikeCodegen {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case BooleanNegate(b) =>
        emitValDef(sym, "!" + quote(b))
      case _ => super.emitNode(sym,rhs)
    }
  }
}

trait CudaGenBooleanOps extends CudaGenBase with CLikeGenBooleanOps
trait CGenBooleanOps extends CGenBase with CLikeGenBooleanOps
