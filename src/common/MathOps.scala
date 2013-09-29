package scala.virtualization.lms
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
    def sqrt(x: Rep[Double])(implicit pos: SourceContext) = math_sqrt(x)
    def sin(x: Rep[Double])(implicit pos: SourceContext) = math_sin(x)
    def cos(x: Rep[Double])(implicit pos: SourceContext) = math_cos(x)
    def acos(x: Rep[Double])(implicit pos: SourceContext) = math_acos(x)
    def atan(x: Rep[Double])(implicit pos: SourceContext) = math_atan(x)
    def atan2(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = math_atan2(x,y)
    def pow(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = math_pow(x,y)
    def abs[A:Manifest:Numeric](x: Rep[A])(implicit pos: SourceContext) = math_abs(x)
    def max[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_max(x,y)
    def min[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_min(x,y)
    def Pi(implicit pos: SourceContext) = math_pi
    def E(implicit pos: SourceContext) = math_e
  }

  def math_ceil(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_floor(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_exp(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_log(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_sqrt(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_sin(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_cos(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
  def math_acos(x: Rep[Double])(implicit pos: SourceContext) : Rep[Double]
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
  case class MathSqrt(x: Exp[Double]) extends Def[Double]
  case class MathSin(x: Exp[Double]) extends Def[Double]
  case class MathCos(x: Exp[Double]) extends Def[Double]
  case class MathAcos(x: Exp[Double]) extends Def[Double]
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
  def math_sqrt(x: Exp[Double])(implicit pos: SourceContext) = MathSqrt(x)
  def math_sin(x: Exp[Double])(implicit pos: SourceContext) = MathSin(x)
  def math_cos(x: Exp[Double])(implicit pos: SourceContext) = MathCos(x)
  def math_acos(x: Exp[Double])(implicit pos: SourceContext) = MathAcos(x)
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

      case Reflect(MathCeil(x), u, es) => reflectMirrored(Reflect(MathCeil(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathFloor(x), u, es) => reflectMirrored(Reflect(MathFloor(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathExp(x), u, es) => reflectMirrored(Reflect(MathExp(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathPow(x,y), u, es) => reflectMirrored(Reflect(MathPow(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathAbs(x), u, es) => reflectMirrored(Reflect(MathAbs(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathSin(x), u, es) => reflectMirrored(Reflect(MathSin(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathCos(x), u, es) => reflectMirrored(Reflect(MathCos(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathAcos(x), u, es) => reflectMirrored(Reflect(MathAcos(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathLog(x), u, es) => reflectMirrored(Reflect(MathLog(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathSqrt(x), u, es) => reflectMirrored(Reflect(MathSqrt(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathAtan2(x,y), u, es) => reflectMirrored(Reflect(MathAtan2(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathMin(x,y), u, es) => reflectMirrored(Reflect(MathMin(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MathMax(x,y), u, es) => reflectMirrored(Reflect(MathMax(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))

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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match { // TODO: use java.lang.Math etc...
    case MathCeil(x) => emitValDef(sym, raw"java.lang.Math.ceil(${quote(x)})")
    case MathFloor(x) => emitValDef(sym, raw"java.lang.Math.floor(${quote(x)})")
    case MathExp(x) => emitValDef(sym, raw"java.lang.Math.exp(${quote(x)})")
    case MathLog(x) => emitValDef(sym, raw"java.lang.Math.log(${quote(x)})")
    case MathSqrt(x) => emitValDef(sym, raw"java.lang.Math.sqrt(${quote(x)})")
    case MathSin(x) => emitValDef(sym, raw"java.lang.Math.sin(${quote(x)})")
    case MathCos(x) => emitValDef(sym, raw"java.lang.Math.cos(${quote(x)})")
    case MathAcos(x) => emitValDef(sym, raw"java.lang.Math.acos(${quote(x)})")
    case MathAtan(x) => emitValDef(sym, raw"java.lang.Math.atan(${quote(x)})")
    case MathAtan2(x,y) => emitValDef(sym, raw"java.lang.Math.atan2(${quote(x)}, ${quote(y)})")
    case MathPow(x,y) => emitValDef(sym, raw"java.lang.Math.pow(${quote(x)},${quote(y)})")
    case MathAbs(x) => emitValDef(sym, raw"java.lang.Math.abs(${quote(x)})")
    case MathMax(x,y) => emitValDef(sym, raw"java.lang.Math.max(${quote(x)}, ${quote(y)})")
    case MathMin(x,y) => emitValDef(sym, raw"java.lang.Math.min(${quote(x)}, ${quote(y)})")
    case MathPi() => emitValDef(sym, "java.lang.Math.PI")
    case MathE() => emitValDef(sym, "java.lang.Math.E")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenMathOpsApacheCommons extends ScalaGenMathOps {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match { // TODO: use java.lang.Math etc...
    case MathCeil(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.ceil(${quote(x)})")
    case MathFloor(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.floor(${quote(x)})")
    case MathExp(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.exp(${quote(x)})")
    case MathLog(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.log(${quote(x)})")
    case MathSqrt(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.sqrt(${quote(x)})")
    case MathSin(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.sin(${quote(x)})")
    case MathCos(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.cos(${quote(x)})")
    case MathAcos(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.acos(${quote(x)})")
    case MathAtan(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.atan(${quote(x)})")
    case MathAtan2(x,y) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.atan2(${quote(x)}, ${quote(y)})")
    case MathPow(x,y) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.pow(${quote(x)},${quote(y)})")
    case MathAbs(x) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.abs(${quote(x)})")
    case MathMax(x,y) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.max(${quote(x)}, ${quote(y)})")
    case MathMin(x,y) => emitValDef(sym, raw"org.apache.commons.math.util.FastMath.min(${quote(x)}, ${quote(y)})")
    case MathPi() => emitValDef(sym, "org.apache.commons.math.util.FastMath.PI")
    case MathE() => emitValDef(sym, "org.apache.commons.math.util.FastMath.E")
    case _ => super.emitNode(sym, rhs)
  }
}




trait CudaGenMathOps extends BaseGenMathOps with CudaGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathCeil(x) => emitValDef(sym, raw"ceil(${quote(x)})")
    case MathFloor(x) => emitValDef(sym, raw"floor(${quote(x)})")
    case MathExp(x) => emitValDef(sym, raw"exp(${quote(x)})")
    case MathLog(x) => emitValDef(sym, raw"log(${quote(x)})")
    case MathSqrt(x) => emitValDef(sym, raw"sqrt(${quote(x)})")
    case MathSin(x) => emitValDef(sym, raw"sin(${quote(x)})")
    case MathCos(x) => emitValDef(sym, raw"cos(${quote(x)})")
    case MathAcos(x) => emitValDef(sym, raw"acos(${quote(x)})")
    case MathAtan(x) => emitValDef(sym, raw"atan(${quote(x)})")
    case MathAtan2(x,y) => emitValDef(sym, raw"atan2(${quote(x)}, ${quote(y)})")
    case MathPow(x,y) => emitValDef(sym, raw"pow(${quote(x)},${quote(y)})")
    case MathAbs(x) => emitValDef(sym, raw"fabs(${quote(x)})")
    case MathMax(x,y) if(remap(sym.tp))=="float" => emitValDef(sym, raw"fmax(${quote(x)}, ${quote(y)})")
    case MathMax(x,y) if(remap(sym.tp))=="double" => emitValDef(sym, raw"fmax(${quote(x)}, ${quote(y)})")
    case MathMax(x,y) if(remap(sym.tp))=="int" => emitValDef(sym, raw"max(${quote(x)}, ${quote(y)})")
    case MathMin(x,y) if(remap(sym.tp))=="float" => emitValDef(sym, raw"fmin(${quote(x)}, ${quote(y)})")
    case MathMin(x,y) if(remap(sym.tp))=="double" => emitValDef(sym, raw"fmin(${quote(x)}, ${quote(y)})")
    case MathMin(x,y) if(remap(sym.tp))=="int" => emitValDef(sym, raw"min(${quote(x)}, ${quote(y)})")
    case MathPi() => emitValDef(sym, "CUDART_PI_F")
    case MathE() => emitValDef(sym, "2.7182818284f")
    case _ => super.emitNode(sym, rhs)
  }        
}

trait OpenCLGenMathOps extends BaseGenMathOps with OpenCLGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathCeil(x) => emitValDef(sym, raw"ceil(${quote(x)})")
    case MathFloor(x) => emitValDef(sym, raw"floor(${quote(x)})")
    case MathExp(x) => emitValDef(sym, raw"exp(${quote(x)})")
    case MathLog(x) => emitValDef(sym, raw"log(${quote(x)})")
    case MathSqrt(x) => emitValDef(sym, raw"sqrt(${quote(x)})")
    case MathAbs(x) => emitValDef(sym, raw"abs(${quote(x)})")
    case MathMax(x,y) => emitValDef(sym, raw"max(${quote(x)}, ${quote(y)})")
    case MathMin(x,y) => emitValDef(sym, raw"min(${quote(x)}, ${quote(y)})")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMathOps extends BaseGenMathOps with CGenEffect {
  val IR: MathOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathSin(x) if(remap(sym.tp)=="double") => emitValDef(sym, raw"sin(${quote(x)})")
    case MathPi() if(remap(sym.tp)=="double") => emitValDef(sym, "M_PI")
    case _ => super.emitNode(sym, rhs)
  }

}
