package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext

trait LiftPrimitives {
  this: PrimitiveOps =>

  implicit def intToRepInt(x: Int) = unit(x)  
  implicit def floatToRepFloat(x: Float) = unit(x)
  implicit def doubleToRepDouble(x: Double) = unit(x)  
  
  // precision-widening promotions
  implicit def chainIntToRepFloat[A:Manifest](x: A)(implicit c: A => Rep[Int]): Rep[Float] = repIntToRepFloat(c(x))
  implicit def chainFloatToRepDouble[A:Manifest](x: A)(implicit c: A => Rep[Float]): Rep[Double] = repFloatToRepDouble(c(x))
}


/**
 * This file is extremely boilerplate and redundant and does not take advantage of any of
 * Scala's type hierarchy to reduce the amount of IR nodes or code generation require.
 * It is in semi-desperate need of a refactor.
 */
trait PrimitiveOps extends Variables with OverloadHack { 
  this: ImplicitOps =>

  /**
   * Primitive conversions
   */
  implicit def repIntToRepDouble(x: Rep[Int]): Rep[Double] = implicit_convert[Int,Double](x)
  implicit def repIntToRepFloat(x: Rep[Int]): Rep[Float] = implicit_convert[Int,Float](x)
  implicit def repFloatToRepDouble(x: Rep[Float]): Rep[Double] = implicit_convert[Float,Double](x)  

  /**
   *  Double
   */
  implicit def doubleToDoubleOps(n: Double) = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]) = new DoubleOpsCls(n)
  implicit def varDoubleToDoubleOps(n: Var[Double]) = new DoubleOpsCls(readVar(n))
  
  object Double {
    def parseDouble(s: Rep[String])(implicit ctx: SourceContext) = obj_double_parse_double(s)
    def PositiveInfinity(implicit ctx: SourceContext) = obj_double_positive_infinity
    def NegativeInfinity(implicit ctx: SourceContext) = obj_double_negative_infinity
    def MinValue(implicit ctx: SourceContext) = obj_double_min_value
    def MaxValue(implicit ctx: SourceContext) = obj_double_max_value
  }

  class DoubleOpsCls(lhs: Rep[Double]){
    def floatValue()(implicit ctx: SourceContext) = double_float_value(lhs)
  }

  def obj_double_parse_double(s: Rep[String])(implicit ctx: SourceContext): Rep[Double]
  def obj_double_positive_infinity(implicit ctx: SourceContext): Rep[Double]
  def obj_double_negative_infinity(implicit ctx: SourceContext): Rep[Double]
  def obj_double_min_value(implicit ctx: SourceContext): Rep[Double]
  def obj_double_max_value(implicit ctx: SourceContext): Rep[Double]
  def double_float_value(lhs: Rep[Double])(implicit ctx: SourceContext): Rep[Float]

  /**
   * Int
   */

  object Integer {
    def parseInt(s: Rep[String])(implicit ctx: SourceContext) = obj_integer_parse_int(s)
  }

  object Int {
    def MaxValue(implicit ctx: SourceContext) = obj_int_max_value
    def MinValue(implicit ctx: SourceContext) = obj_int_min_value
  }

  implicit def intToIntOps(n: Int) = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]) = new IntOpsCls(n)
  implicit def varIntToIntOps(n: Var[Int]) = new IntOpsCls(readVar(n))
    
  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash        
    //def /[A](rhs: Rep[A])(implicit mA: Manifest[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
    // TODO Something is wrong if we just use floatValue. implicits get confused
    def floatValueL()(implicit ctx: SourceContext) = int_float_value(lhs)
    def doubleValue()(implicit ctx: SourceContext) = int_double_value(lhs)
    def unary_~()(implicit ctx: SourceContext) = int_bitwise_not(lhs)
    def toLong(implicit ctx: SourceContext) = int_tolong(lhs)
  }

  def infix_/(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext) = int_divide(lhs, rhs)
  def infix_%(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext) = int_mod(lhs, rhs)
  def infix_&(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext) = int_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext) = int_binaryor(lhs, rhs)
  def infix_^(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext) = int_binaryxor(lhs, rhs)

  def obj_integer_parse_int(s: Rep[String])(implicit ctx: SourceContext): Rep[Int]
  def obj_int_max_value(implicit ctx: SourceContext): Rep[Int]
  def obj_int_min_value(implicit ctx: SourceContext): Rep[Int]
  def int_divide_frac[A:Manifest:Fractional](lhs: Rep[Int], rhs: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  def int_mod(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  def int_float_value(lhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float]
  def int_double_value(lhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double]
  def int_bitwise_not(lhs: Rep[Int])(implicit ctx: SourceContext) : Rep[Int]
  def int_tolong(lhs: Rep[Int])(implicit ctx: SourceContext) : Rep[Long]
  
  /**
   * Long
   */
  def infix_&(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded1, ctx: SourceContext) = long_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded1, ctx: SourceContext) = long_binaryor(lhs, rhs)
  def infix_<<(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext) = long_shiftleft(lhs, rhs)
  def infix_>>>(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext) = long_shiftright_unsigned(lhs, rhs)
  def infix_toInt(lhs: Rep[Long])(implicit o: Overloaded1, ctx: SourceContext) = long_toint(lhs)
    
  def long_binaryand(lhs: Rep[Long], rhs: Rep[Long])(implicit ctx: SourceContext): Rep[Long]
  def long_binaryor(lhs: Rep[Long], rhs: Rep[Long])(implicit ctx: SourceContext): Rep[Long]
  def long_shiftleft(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long]
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long]
  def long_toint(lhs: Rep[Long])(implicit ctx: SourceContext): Rep[Int]
}

trait PrimitiveOpsExp extends PrimitiveOps with BaseExp {
  this: ImplicitOps =>

  /**
   * Double
   */
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]
  case class ObjDoublePositiveInfinity() extends Def[Double]
  case class ObjDoubleNegativeInfinity() extends Def[Double]
  case class ObjDoubleMinValue() extends Def[Double]
  case class ObjDoubleMaxValue() extends Def[Double]
  case class DoubleFloatValue(lhs: Exp[Double]) extends Def[Float]

  def obj_double_parse_double(s: Exp[String])(implicit ctx: SourceContext) = ObjDoubleParseDouble(s)
  def obj_double_positive_infinity(implicit ctx: SourceContext) = ObjDoublePositiveInfinity()
  def obj_double_negative_infinity(implicit ctx: SourceContext) = ObjDoubleNegativeInfinity()
  def obj_double_min_value(implicit ctx: SourceContext) = ObjDoubleMinValue()
  def obj_double_max_value(implicit ctx: SourceContext) = ObjDoubleMaxValue()
  def double_float_value(lhs: Exp[Double])(implicit ctx: SourceContext) = DoubleFloatValue(lhs)

  /**
   * Int
   */
  case class ObjIntegerParseInt(s: Exp[String]) extends Def[Int]
  case class ObjIntMaxValue() extends Def[Int]
  case class ObjIntMinValue() extends Def[Int]
  case class IntDivideFrac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]
  case class IntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMod(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryOr(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryAnd(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryXor(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntDoubleValue(lhs: Exp[Int]) extends Def[Double]
  case class IntFloatValue(lhs: Exp[Int]) extends Def[Float]
  case class IntBitwiseNot(lhs: Exp[Int]) extends Def[Int]
  case class IntToLong(lhs: Exp[Int]) extends Def[Long]

  def obj_integer_parse_int(s: Rep[String])(implicit ctx: SourceContext) = ObjIntegerParseInt(s)
  def obj_int_max_value(implicit ctx: SourceContext) = ObjIntMaxValue()
  def obj_int_min_value(implicit ctx: SourceContext) = ObjIntMinValue()
  def int_divide_frac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A])(implicit ctx: SourceContext) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int])(implicit ctx: SourceContext) : Exp[Int] = IntDivide(lhs, rhs)
  def int_mod(lhs: Exp[Int], rhs: Exp[Int])(implicit ctx: SourceContext) = IntMod(lhs, rhs)
  def int_binaryor(lhs: Exp[Int], rhs: Exp[Int])(implicit ctx: SourceContext) = IntBinaryOr(lhs, rhs)
  def int_binaryand(lhs: Exp[Int], rhs: Exp[Int])(implicit ctx: SourceContext) = IntBinaryAnd(lhs, rhs)
  def int_binaryxor(lhs: Exp[Int], rhs: Exp[Int])(implicit ctx: SourceContext) = IntBinaryXor(lhs, rhs)
  def int_double_value(lhs: Exp[Int])(implicit ctx: SourceContext) = IntDoubleValue(lhs)
  def int_float_value(lhs: Exp[Int])(implicit ctx: SourceContext) = IntFloatValue(lhs)
  def int_bitwise_not(lhs: Exp[Int])(implicit ctx: SourceContext) = IntBitwiseNot(lhs)
  def int_tolong(lhs: Exp[Int])(implicit ctx: SourceContext) = IntToLong(lhs)
  
  /**
   * Long
   */
  case class LongBinaryOr(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongShiftLeft(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightUnsigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongToInt(lhs: Exp[Long]) extends Def[Int]

  def long_binaryor(lhs: Exp[Long], rhs: Exp[Long])(implicit ctx: SourceContext) = LongBinaryOr(lhs,rhs)
  def long_binaryand(lhs: Exp[Long], rhs: Exp[Long])(implicit ctx: SourceContext) = LongBinaryAnd(lhs,rhs)  
  def long_shiftleft(lhs: Exp[Long], rhs: Exp[Int])(implicit ctx: SourceContext) = LongShiftLeft(lhs,rhs)
  def long_shiftright_unsigned(lhs: Exp[Long], rhs: Exp[Int])(implicit ctx: SourceContext) = LongShiftRightUnsigned(lhs,rhs)
  def long_toint(lhs: Exp[Long])(implicit ctx: SourceContext) = LongToInt(lhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = ({
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    e match {
      case DoubleFloatValue(x) => double_float_value(f(x))
      case IntDoubleValue(x) => int_double_value(f(x))
      case IntFloatValue(x) => int_float_value(f(x))
      case IntBitwiseNot(x) => int_bitwise_not(f(x))
      case IntDivide(x,y) => int_divide(f(x),f(y))
      case IntMod(x,y) => int_mod(f(x),f(y))
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "scala.Double.PositiveInfinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "scala.Double.NegativeInfinity")
    case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
    case ObjDoubleMaxValue() => emitValDef(sym, "scala.Double.MaxValue")
    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
    case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
    case ObjIntMinValue() => emitValDef(sym, "scala.Int.MinValue")
    case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case IntDoubleValue(lhs) => emitValDef(sym, quote(lhs) + ".doubleValue()")
    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case IntToLong(lhs) => emitValDef(sym, quote(lhs) + ".toLong")
    case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))    
    case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))    
    case LongToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenPrimitiveOps extends CLikeGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  //TODO: stdlib.h needs to be included in the common header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      //case ObjDoubleParseDouble(s) => emitValDef(sym, "atof(" + quote(s) + ")")
    	case ObjDoublePositiveInfinity() => emitValDef(sym, "DBL_MAX")
      //case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
      case DoubleFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
      //case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
      //case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
      case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
      case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
      case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
      case IntDoubleValue(lhs) => emitValDef(sym, "(double)"+quote(lhs))
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase with CLikeGenPrimitiveOps
trait OpenCLGenPrimitiveOps extends OpenCLGenBase with CLikeGenPrimitiveOps
trait CGenPrimitiveOps extends CGenBase with CLikeGenPrimitiveOps

