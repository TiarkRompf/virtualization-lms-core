package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal.{GenericNestedCodegen}

trait MathOps extends Base {

  object Math {
    def ceil(x: Rep[Double]) = math_ceil(x)
    def floor(x: Rep[Double]) = math_floor(x)
    def exp(x: Rep[Double]) = math_exp(x)
    def log(x: Rep[Double]) = math_log(x)
    def sqrt(x: Rep[Double]) = math_sqrt(x)
    def abs[A:Manifest:Numeric](x: Rep[A]) = math_abs(x)
    def max[A:Manifest:Numeric](x: Rep[A], y: Rep[A]) = math_max(x,y)
    def min[A:Manifest:Numeric](x: Rep[A], y: Rep[A]) = math_min(x,y)
  }

  def math_ceil(x: Rep[Double]) : Rep[Double]
  def math_floor(x: Rep[Double]) : Rep[Double]
  def math_exp(x: Rep[Double]) : Rep[Double]
  def math_log(x: Rep[Double]) : Rep[Double]
  def math_sqrt(x: Rep[Double]) : Rep[Double]
  def math_abs[A:Manifest:Numeric](x: Rep[A]) : Rep[A]
  def math_max[A:Manifest:Numeric](x: Rep[A], y: Rep[A]): Rep[A]
  def math_min[A:Manifest:Numeric](x: Rep[A], y: Rep[A]): Rep[A]
}

trait MathOpsExp extends MathOps with EffectExp {
  case class MathCeil(x: Exp[Double]) extends Def[Double]
  case class MathFloor(x: Exp[Double]) extends Def[Double]
  case class MathExp(x: Exp[Double]) extends Def[Double]
  case class MathLog(x: Exp[Double]) extends Def[Double]
  case class MathSqrt(x: Exp[Double]) extends Def[Double]
  case class MathAbs[A:Manifest:Numeric](x: Exp[A]) extends Def[A]
  case class MathMax[A:Manifest:Numeric](x: Exp[A], y: Exp[A]) extends Def[A]
  case class MathMin[A:Manifest:Numeric](x: Exp[A], y: Exp[A]) extends Def[A]

  def math_ceil(x: Exp[Double]) = MathCeil(x)
  def math_floor(x: Exp[Double]) = MathFloor(x)
  def math_exp(x: Exp[Double]) = MathExp(x)
  def math_log(x: Exp[Double]) = MathLog(x)
  def math_sqrt(x: Exp[Double]) = MathSqrt(x)
  def math_abs[A:Manifest:Numeric](x: Exp[A]) = MathAbs(x)
  def math_max[A:Manifest:Numeric](x: Exp[A], y: Exp[A]) = MathMax(x, y)
  def math_min[A:Manifest:Numeric](x: Exp[A], y: Exp[A]) = MathMin(x, y)
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case MathCeil(x) => emitValDef(sym, "Math.ceil(" + quote(x) + ")")
    case MathFloor(x) => emitValDef(sym, "Math.floor(" + quote(x) + ")")
    case MathExp(x) => emitValDef(sym, "Math.exp(" + quote(x) + ")")
    case MathLog(x) => emitValDef(sym, "Math.log(" + quote(x) + ")")
    case MathSqrt(x) => emitValDef(sym, "Math.sqrt(" + quote(x) + ")")
    case MathAbs(x) => emitValDef(sym, "Math.abs(" + quote(x) + ")")
    case MathMax(x,y) => emitValDef(sym, "Math.max(" + quote(x) + ", " + quote(y) + ")")
    case MathMin(x,y) => emitValDef(sym, "Math.min(" + quote(x) + ", " + quote(y) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMathOps extends BaseGenMathOps with CudaGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case MathCeil(x) => emitValDef(sym, "ceil(" + quote(x) + ")")
    case MathFloor(x) => emitValDef(sym, "floor(" + quote(x) + ")")
    case MathExp(x) => emitValDef(sym, "exp(" + quote(x) + ")")
    case MathLog(x) => emitValDef(sym, "log(" + quote(x) + ")")
    case MathSqrt(x) => emitValDef(sym, "sqrt(" + quote(x) + ")")
    case MathAbs(x) => emitValDef(sym, "abs(" + quote(x) + ")")
    case MathMax(x,y) => emitValDef(sym, "max(" + quote(x) + ", " + quote(y) + ")")
    case MathMin(x,y) => emitValDef(sym, "min(" + quote(x) + ", " + quote(y) + ")")
    case _ => super.emitNode(sym, rhs)
  }        
}
