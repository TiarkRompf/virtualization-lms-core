package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal.{GenericNestedCodegen, CudaGenEffect, ScalaGenEffect}

trait MathOps extends Base {

  object Math {
    def ceil(x: Rep[Double]) = math_ceil(x)
    def exp(x: Rep[Double]) = math_exp(x)
    def log(x: Rep[Double]) = math_log(x)
    def abs(x: Rep[Double]) = math_abs(x)
    def sqrt(x: Rep[Double]) = math_sqrt(x)
  }

  def math_ceil(x: Rep[Double]) : Rep[Double]
  def math_exp(x: Rep[Double]) : Rep[Double]
  def math_log(x: Rep[Double]) : Rep[Double]
  def math_abs(x: Rep[Double]) : Rep[Double]
  def math_sqrt(x: Rep[Double]) : Rep[Double]
}

trait MathOpsExp extends MathOps with EffectExp {
  case class MathCeil(x: Exp[Double]) extends Def[Double]
  case class MathExp(x: Exp[Double]) extends Def[Double]
  case class MathLog(x: Exp[Double]) extends Def[Double]
  case class MathAbs(x: Exp[Double]) extends Def[Double]
  case class MathSqrt(x: Exp[Double]) extends Def[Double]

  def math_ceil(x: Exp[Double]) = MathCeil(x)
  def math_exp(x: Exp[Double]) = MathExp(x)
  def math_log(x: Exp[Double]) = MathLog(x)
  def math_abs(x: Exp[Double]) = MathAbs(x)
  def math_sqrt(x: Exp[Double]) = MathSqrt(x)
}

trait BaseGenMathOps extends GenericNestedCodegen {
  val IR: MathOpsExp
  import IR._

  //override def syms(e: Any): Math[Sym[Any]] = e match {
  //  case _ => super.syms(e)
  //}

}

trait ScalaGenMathOps extends BaseGenMathOps with ScalaGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case MathCeil(x) => emitValDef(sym, "Math.ceil(" + quote(x) + ")")
    case MathExp(x) => emitValDef(sym, "Math.exp(" + quote(x) + ")")
    case MathLog(x) => emitValDef(sym, "Math.log(" + quote(x) + ")")
    case MathAbs(x) => emitValDef(sym, "Math.abs(" + quote(x) + ")")
    case MathSqrt(x) => emitValDef(sym, "Math.sqrt(" + quote(x) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMathOps extends BaseGenMathOps with CudaGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}
