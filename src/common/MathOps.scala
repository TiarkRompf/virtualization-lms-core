package scala.lms
package common

import java.io.PrintWriter
import internal.{GenericNestedCodegen}
import scala.reflect.SourceContext

trait MathOps extends Base {

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
    def abs[A:Manifest:Numeric](x: Rep[A])(implicit pos: SourceContext) = math_abs(x)
    def max[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_max(x,y)
    def min[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_min(x,y)
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
  def math_abs[A:Manifest:Numeric](x: Rep[A])(implicit pos: SourceContext) : Rep[A]
  def math_max[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext): Rep[A]
  def math_min[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext): Rep[A]
  def math_pi(implicit pos: SourceContext): Rep[Double]
  def math_e(implicit pos: SourceContext): Rep[Double]
}

trait MathOpsExp extends MathOps with EffectExp {
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
  case class MathAbs[A:Manifest:Numeric](x: Exp[A]) extends Def[A]
  case class MathMax[A:Manifest:Numeric](x: Exp[A], y: Exp[A]) extends Def[A]
  case class MathMin[A:Manifest:Numeric](x: Exp[A], y: Exp[A]) extends Def[A]
  case class MathPi() extends Def[Double]
  case class MathE() extends Def[Double]

  def math_ceil(x: Exp[Double])(implicit pos: SourceContext) = MathCeil(x)
  def math_floor(x: Exp[Double])(implicit pos: SourceContext) = MathFloor(x)
  def math_exp(x: Exp[Double])(implicit pos: SourceContext) = MathExp(x)
  def math_log(x: Exp[Double])(implicit pos: SourceContext) = MathLog(x)
  def math_log10(x: Exp[Double])(implicit ctx: SourceContext) = MathLog10(x)
  def math_sqrt(x: Exp[Double])(implicit pos: SourceContext) = MathSqrt(x)
  def math_sin(x: Exp[Double])(implicit pos: SourceContext) = MathSin(x)
  def math_sinh(x: Exp[Double])(implicit ctx: SourceContext) = MathSinh(x)
  def math_asin(x: Exp[Double])(implicit ctx: SourceContext) = MathAsin(x)
  def math_cos(x: Exp[Double])(implicit pos: SourceContext) = MathCos(x)
  def math_cosh(x: Exp[Double])(implicit ctx: SourceContext) = MathCosh(x)
  def math_acos(x: Exp[Double])(implicit pos: SourceContext) = MathAcos(x)
  def math_tan(x: Exp[Double])(implicit ctx: SourceContext) = MathTan(x)
  def math_tanh(x: Exp[Double])(implicit ctx: SourceContext) = MathTanh(x)    
  def math_atan(x: Exp[Double])(implicit pos: SourceContext) = MathAtan(x)
  def math_atan2(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = MathAtan2(x,y)
  def math_pow(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = MathPow(x,y)
  def math_abs[A:Manifest:Numeric](x: Exp[A])(implicit pos: SourceContext) = MathAbs(x)
  def math_max[A:Manifest:Numeric](x: Exp[A], y: Exp[A])(implicit pos: SourceContext) = MathMax(x, y)
  def math_min[A:Manifest:Numeric](x: Exp[A], y: Exp[A])(implicit pos: SourceContext) = MathMin(x, y)
  def math_pi(implicit pos: SourceContext) = MathPi()
  def math_e(implicit pos: SourceContext) = MathE()  

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = ({
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    e match {
      case MathCeil(x) => math_ceil(f(x))
      case MathFloor(x) => math_floor(f(x))
      case MathExp(x) => math_exp(f(x))
      case MathPow(x,y) => math_pow(f(x),f(y))
      case MathAbs(x) => math_abs(f(x))
      case MathSin(x) => math_sin(f(x))
      case MathCos(x) => math_cos(f(x))
      case MathAcos(x) => math_acos(f(x))
      case MathLog(x) => math_log(f(x))
      case MathSqrt(x) => math_sqrt(f(x))
      case MathAtan2(x,y) => math_atan2(f(x),f(y))
      case MathMin(x,y) => math_min(f(x),f(y))
      case MathMax(x,y) => math_max(f(x),f(y))
      case MathLog10(x) => math_log10(f(x))
      case MathSinh(x) => math_sinh(f(x))
      case MathAsin(x) => math_asin(f(x))
      case MathCosh(x) => math_cosh(f(x))
      case MathTan(x) => math_tan(f(x))
      case MathTanh(x) => math_tanh(f(x))

      case Reflect(MathCeil(x), u, es) => reflectMirrored(Reflect(MathCeil(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathFloor(x), u, es) => reflectMirrored(Reflect(MathFloor(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathExp(x), u, es) => reflectMirrored(Reflect(MathExp(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathPow(x,y), u, es) => reflectMirrored(Reflect(MathPow(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathAbs(x), u, es) => reflectMirrored(Reflect(MathAbs(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathSin(x), u, es) => reflectMirrored(Reflect(MathSin(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathCos(x), u, es) => reflectMirrored(Reflect(MathCos(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathAcos(x), u, es) => reflectMirrored(Reflect(MathAcos(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathLog(x), u, es) => reflectMirrored(Reflect(MathLog(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathSqrt(x), u, es) => reflectMirrored(Reflect(MathSqrt(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathAtan2(x,y), u, es) => reflectMirrored(Reflect(MathAtan2(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathMin(x,y), u, es) => reflectMirrored(Reflect(MathMin(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathMax(x,y), u, es) => reflectMirrored(Reflect(MathMax(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathLog10(x), u, es) => reflectMirrored(Reflect(MathLog10(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathSinh(x), u, es) => reflectMirrored(Reflect(MathSinh(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathAsin(x), u, es) => reflectMirrored(Reflect(MathAsin(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathCosh(x), u, es) => reflectMirrored(Reflect(MathCosh(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathTan(x), u, es) => reflectMirrored(Reflect(MathTan(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(MathTanh(x), u, es) => reflectMirrored(Reflect(MathTanh(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)      

      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]

}

trait BaseGenMathOps extends GenericNestedCodegen {
  val IR: MathOpsExp
  import IR._

}

trait ScalaGenMathOps extends BaseGenMathOps with ScalaGenEffect {
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


trait CLikeGenMathOps extends BaseGenMathOps with CLikeGenEffect {
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


trait CudaGenMathOps extends CLikeGenMathOps with CudaGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "CUDART_PI_F")
    case MathE() => emitValDef(sym, "2.7182818284f")
    case _ => super.emitNode(sym, rhs)
  }        
}

trait OpenCLGenMathOps extends CLikeGenMathOps with OpenCLGenEffect {
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

trait CGenMathOps extends CLikeGenMathOps with CGenEffect {
  val IR: MathOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "M_PI")
    case MathE() => emitValDef(sym, "M_E")
    case _ => super.emitNode(sym, rhs)
  }

}
