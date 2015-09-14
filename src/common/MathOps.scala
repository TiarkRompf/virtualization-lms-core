package scala.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.lms.codegen.GenericCodegen
import scala.lms.internal._

trait MathOps extends Base with PrimitiveOps {

  object Math {
    def ceil(x: Rep[Double])(implicit pos: SourceContext) = math_ceil(x)
    def floor(x: Rep[Double])(implicit pos: SourceContext) = math_floor(x)
    def exp(x: Rep[Double])(implicit pos: SourceContext) = math_exp(x)
    def log(x: Rep[Double])(implicit pos: SourceContext) = math_log(x)
    def log10(x: Rep[Double])(implicit ctx: SourceContext) = math_log10(x)
    def sqrt(x: Rep[Double])(implicit pos: SourceContext) = math_sqrt(x)
    def sin(x: Rep[Double])(implicit pos: SourceContext) = math_sin(x)
    def sinh(x: Rep[Double])(implicit ctx: SourceContext) = math_sinh(x)
    def asin(x: Rep[Double])(implicit ctx: SourceContext) = math_asin(x)
    def cos(x: Rep[Double])(implicit pos: SourceContext) = math_cos(x)
    def cosh(x: Rep[Double])(implicit ctx: SourceContext) = math_cosh(x)
    def acos(x: Rep[Double])(implicit pos: SourceContext) = math_acos(x)
    def tan(x: Rep[Double])(implicit ctx: SourceContext) = math_tan(x)
    def tanh(x: Rep[Double])(implicit ctx: SourceContext) = math_tanh(x)
    def atan(x: Rep[Double])(implicit pos: SourceContext) = math_atan(x)
    def atan2(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = math_atan2(x,y)
    def pow(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = math_pow(x,y)
    def abs[A:Typ:Numeric](x: Rep[A])(implicit pos: SourceContext) = math_abs(x)
    def max[A:Typ:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_max(x,y)
    def min[A:Typ:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_min(x,y)
    def Pi(implicit pos: SourceContext) = 3.141592653589793238462643383279502884197169
    def E(implicit pos: SourceContext) = math_e
  }

  def math_ceil(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_floor(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_exp(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_log(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_log10(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def math_sqrt(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_sin(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_sinh(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def math_asin(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def math_cos(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_cosh(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def math_acos(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_tan(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def math_tanh(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def math_atan(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_atan2(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_pow(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def math_abs[A:Typ:Numeric](x: Rep[A])(implicit pos: SourceContext) : Rep[A]
  def math_max[A:Typ:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext): Rep[A]
  def math_min[A:Typ:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext): Rep[A]
  def math_pi(implicit pos: SourceContext): Rep[Double]
  def math_e(implicit pos: SourceContext): Rep[Double]
}

trait MathOpsExp extends MathOps with BaseExp {

  case class MathCeil(x: Exp[Double]) extends Def[Double]
  case class MathFloor(x: Exp[Double]) extends Def[Double]
  case class MathExp(x: Exp[Double]) extends Def[Double]
  case class MathLog(x: Exp[Double]) extends Def[Double]
  case class MathLog10(x: Exp[Double]) extends Def[Double]
  case class MathSqrt(x: Exp[Double]) extends Def[Double]
  case class MathSin(x: Exp[Double]) extends Def[Double]
  case class MathSinh(x: Exp[Double]) extends Def[Double]
  case class MathAsin(x: Exp[Double]) extends Def[Double]
  case class MathCos(x: Exp[Double]) extends Def[Double]
  case class MathCosh(x: Exp[Double]) extends Def[Double]
  case class MathAcos(x: Exp[Double]) extends Def[Double]
  case class MathTan(x: Exp[Double]) extends Def[Double]
  case class MathTanh(x: Exp[Double]) extends Def[Double]
  case class MathAtan(x: Exp[Double]) extends Def[Double]
  case class MathAtan2(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class MathPow(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class MathAbs[A:Typ:Numeric](x: Exp[A]) extends NumericDef1[A]
  case class MathMax[A:Typ:Numeric](x: Exp[A], y: Exp[A]) extends NumericDef1[A]
  case class MathMin[A:Typ:Numeric](x: Exp[A], y: Exp[A]) extends NumericDef1[A]
  case class MathPi() extends Def[Double]
  case class MathE() extends Def[Double]

  def math_ceil(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathCeil(x))
  def math_floor(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathFloor(x))
  def math_exp(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathExp(x))
  def math_log(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathLog(x))
  def math_log10(x: Exp[Double])(implicit ctx: SourceContext) = toAtom(MathLog10(x))
  def math_sqrt(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathSqrt(x))
  def math_sin(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathSin(x))
  def math_sinh(x: Exp[Double])(implicit ctx: SourceContext) = toAtom(MathSinh(x))
  def math_asin(x: Exp[Double])(implicit ctx: SourceContext) = toAtom(MathAsin(x))
  def math_cos(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathCos(x))
  def math_cosh(x: Exp[Double])(implicit ctx: SourceContext) = toAtom(MathCosh(x))
  def math_acos(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathAcos(x))
  def math_tan(x: Exp[Double])(implicit ctx: SourceContext) = toAtom(MathTan(x))
  def math_tanh(x: Exp[Double])(implicit ctx: SourceContext) = toAtom(MathTanh(x))
  def math_atan(x: Exp[Double])(implicit pos: SourceContext) = toAtom(MathAtan(x))
  def math_atan2(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = toAtom(MathAtan2(x,y))
  def math_pow(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = toAtom(MathPow(x,y))
  def math_abs[A:Typ:Numeric](x: Exp[A])(implicit pos: SourceContext) = toAtom(MathAbs(x))
  def math_max[A:Typ:Numeric](x: Exp[A], y: Exp[A])(implicit pos: SourceContext) = toAtom(MathMax(x, y))
  def math_min[A:Typ:Numeric](x: Exp[A], y: Exp[A])(implicit pos: SourceContext) = toAtom(MathMin(x, y))
  def math_pi(implicit pos: SourceContext) = toAtom(MathPi())
  def math_e(implicit pos: SourceContext) = toAtom(MathE())

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case MathCeil(x) => math_ceil(f(x))
    case MathFloor(x) => math_floor(f(x))
    case MathExp(x) => math_exp(f(x))
    case MathPow(x,y) => math_pow(f(x),f(y))
    case e@MathAbs(x) => math_abs(f(x))(mtype(e.mR),ntype(e.nR),pos)
    case MathSin(x) => math_sin(f(x))
    case MathCos(x) => math_cos(f(x))
    case MathAcos(x) => math_acos(f(x))
    case MathLog(x) => math_log(f(x))
    case MathSqrt(x) => math_sqrt(f(x))
    case MathAtan2(x,y) => math_atan2(f(x),f(y))
    case e@MathMin(x,y) => math_min(f(x),f(y))(mtype(e.mR),ntype(e.nR),pos)
    case e@MathMax(x,y) => math_max(f(x),f(y))(mtype(e.mR),ntype(e.nR),pos)
    case MathLog10(x) => math_log10(f(x))
    case MathSinh(x) => math_sinh(f(x))
    case MathAsin(x) => math_asin(f(x))
    case MathCosh(x) => math_cosh(f(x))
    case MathTan(x) => math_tan(f(x))
    case MathTanh(x) => math_tanh(f(x))
    case _ => super.mirror(e,f)
  }
}

trait BaseGenMathOps extends GenericCodegen {
  val IR: MathOpsExp
  import IR._

}

trait ScalaGenMathOps extends BaseGenMathOps with ScalaGenBase {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathCeil(x) => emitValDef(sym, src"java.lang.Math.ceil($x)")
    case MathFloor(x) => emitValDef(sym, src"java.lang.Math.floor($x)")
    case MathExp(x) => emitValDef(sym, src"java.lang.Math.exp($x)")
    case MathLog(x) => emitValDef(sym, src"java.lang.Math.log($x)")
    case MathLog10(x) => emitValDef(sym, src"java.lang.Math.log10($x)")
    case MathSqrt(x) => emitValDef(sym, src"java.lang.Math.sqrt($x)")
    case MathSin(x) => emitValDef(sym, src"java.lang.Math.sin($x)")
    case MathSinh(x) => emitValDef(sym, "java.lang.Math.sinh($x)")
    case MathAsin(x) => emitValDef(sym, "java.lang.Math.asin($x)")
    case MathCos(x) => emitValDef(sym, src"java.lang.Math.cos($x)")
    case MathCosh(x) => emitValDef(sym, "java.lang.Math.cosh($x)")
    case MathAcos(x) => emitValDef(sym, src"java.lang.Math.acos($x)")
    case MathTan(x) => emitValDef(sym, "java.lang.Math.tan($x)")
    case MathTanh(x) => emitValDef(sym, "java.lang.Math.tanh($x)")
    case MathAtan(x) => emitValDef(sym, src"java.lang.Math.atan($x)")
    case MathAtan2(x,y) => emitValDef(sym, src"java.lang.Math.atan2($x, $y)")
    case MathPow(x,y) => emitValDef(sym, src"java.lang.Math.pow($x,$y)")
    case MathAbs(x) => emitValDef(sym, src"java.lang.Math.abs($x)")
    case MathMax(x,y) => emitValDef(sym, src"java.lang.Math.max($x, $y)")
    case MathMin(x,y) => emitValDef(sym, src"java.lang.Math.min($x, $y)")
    case MathPi() => emitValDef(sym, "java.lang.Math.PI")
    case MathE() => emitValDef(sym, "java.lang.Math.E")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenMathOpsApacheCommons extends ScalaGenMathOps {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match { // TODO: use java.lang.Math etc...
    case MathCeil(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.ceil($x)")
    case MathFloor(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.floor($x)")
    case MathExp(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.exp($x)")
    case MathLog(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.log($x)")
    case MathSqrt(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.sqrt($x)")
    case MathSin(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.sin($x)")
    case MathCos(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.cos($x)")
    case MathAcos(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.acos($x)")
    case MathAtan(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.atan($x)")
    case MathAtan2(x,y) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.atan2($x, $y)")
    case MathPow(x,y) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.pow($x,$y)")
    case MathAbs(x) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.abs($x)")
    case MathMax(x,y) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.max($x, $y)")
    case MathMin(x,y) => emitValDef(sym, src"org.apache.commons.math.util.FastMath.min($x, $y)")
    case MathPi() => emitValDef(sym, "org.apache.commons.math.util.FastMath.PI")
    case MathE() => emitValDef(sym, "org.apache.commons.math.util.FastMath.E")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CLikeGenMathOps extends BaseGenMathOps with CLikeGenBase {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathCeil(x) => emitValDef(sym, src"ceil($x)")
    case MathFloor(x) => emitValDef(sym, src"floor($x)")
    case MathExp(x) => emitValDef(sym, src"exp($x)")
    case MathLog(x) => emitValDef(sym, src"log($x)")
    case MathSqrt(x) => emitValDef(sym, src"sqrt($x)")
    case MathSin(x) => emitValDef(sym, src"sin($x)")
    case MathCos(x) => emitValDef(sym, src"cos($x)")
    case MathAcos(x) => emitValDef(sym, src"acos($x)")
    case MathAtan(x) => emitValDef(sym, src"atan($x)")
    case MathAtan2(x,y) => emitValDef(sym, src"atan2($x, $y)")
    case MathPow(x,y) => emitValDef(sym, src"pow($x,$y)")
    case MathAbs(x) => emitValDef(sym, src"fabs($x)")
    case MathMax(x,y) if(remap(sym.tp))=="float" => emitValDef(sym, src"fmax($x, $y)")
    case MathMax(x,y) if(remap(sym.tp))=="double" => emitValDef(sym, src"fmax($x, $y)")
    case MathMax(x,y) if(remap(sym.tp))=="int" => emitValDef(sym, src"max($x, $y)")
    case MathMin(x,y) if(remap(sym.tp))=="float" => emitValDef(sym, src"fmin($x, $y)")
    case MathMin(x,y) if(remap(sym.tp))=="double" => emitValDef(sym, src"fmin($x, $y)")
    case MathMin(x,y) if(remap(sym.tp))=="int" => emitValDef(sym, src"min($x, $y)")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenMathOps extends CLikeGenMathOps with CudaGenBase {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "CUDART_PI_F")
    case MathE() => emitValDef(sym, "2.7182818284f")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenMathOps extends CLikeGenMathOps with OpenCLGenBase {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "M_PI")
    case MathE() => emitValDef(sym, "M_E")
    case MathMax(x,y) => emitValDef(sym, src"max($x, $y)")
    case MathMin(x,y) => emitValDef(sym, src"min($x, $y)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMathOps extends CLikeGenMathOps with CGenBase {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "M_PI")
    case MathE() => emitValDef(sym, "M_E")
    case _ => super.emitNode(sym, rhs)
  }

}
