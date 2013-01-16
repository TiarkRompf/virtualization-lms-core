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
    def parseDouble(s: Rep[String])(implicit pos: SourceContext) = obj_double_parse_double(s)
    def PositiveInfinity(implicit pos: SourceContext) = obj_double_positive_infinity
    def NegativeInfinity(implicit pos: SourceContext) = obj_double_negative_infinity
    def MinValue(implicit pos: SourceContext) = obj_double_min_value
    def MaxValue(implicit pos: SourceContext) = obj_double_max_value
  }

  class DoubleOpsCls(lhs: Rep[Double]){
    def floatValue()(implicit pos: SourceContext) = double_float_value(lhs)
  }

  def obj_double_parse_double(s: Rep[String])(implicit pos: SourceContext): Rep[Double]
  def obj_double_positive_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_negative_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_min_value(implicit pos: SourceContext): Rep[Double]
  def obj_double_max_value(implicit pos: SourceContext): Rep[Double]
  def double_float_value(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float]

  /**
   * Int
   */

  object Integer {
    def parseInt(s: Rep[String])(implicit pos: SourceContext) = obj_integer_parse_int(s)
  }

  object Int {
    def MaxValue(implicit pos: SourceContext) = obj_int_max_value
    def MinValue(implicit pos: SourceContext) = obj_int_min_value
  }

  implicit def intToIntOps(n: Int) = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]) = new IntOpsCls(n)
  implicit def varIntToIntOps(n: Var[Int]) = new IntOpsCls(readVar(n))
    
  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash        
    //def /[A](rhs: Rep[A])(implicit mA: Manifest[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
    // TODO Something is wrong if we just use floatValue. implicits get confused
    def floatValueL()(implicit pos: SourceContext) = int_float_value(lhs)
    def doubleValue()(implicit pos: SourceContext) = int_double_value(lhs)
    def unary_~()(implicit pos: SourceContext) = int_bitwise_not(lhs)
    def toLong(implicit pos: SourceContext) = int_tolong(lhs)
  }

  def infix_+(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_plus(lhs, rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_minus(lhs, rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_times(lhs, rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_divide(lhs, rhs)
  def infix_%(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_mod(lhs, rhs)
  def infix_&(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_binaryor(lhs, rhs)
  def infix_^(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_binaryxor(lhs, rhs)
  def infix_<<(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_leftshift(lhs, rhs)
  def infix_>>(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_rightshiftarith(lhs, rhs)
  def infix_>>>(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext) = int_rightshiftlogical(lhs, rhs)

  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
  def obj_int_max_value(implicit pos: SourceContext): Rep[Int]
  def obj_int_min_value(implicit pos: SourceContext): Rep[Int]
  def int_plus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_minus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_times(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_divide_frac[A:Manifest:Fractional](lhs: Rep[Int], rhs: Rep[A])(implicit pos: SourceContext): Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_mod(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_float_value(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Float]
  def int_double_value(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Double]
  def int_bitwise_not(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Int]
  def int_tolong(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Long]
  def int_leftshift(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftarith(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftlogical(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]

  /**
   * Long
   */
  def infix_&(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded1, pos: SourceContext) = long_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded1, pos: SourceContext) = long_binaryor(lhs, rhs)
  def infix_<<(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = long_shiftleft(lhs, rhs)
  def infix_>>>(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = long_shiftright_unsigned(lhs, rhs)
  def infix_toInt(lhs: Rep[Long])(implicit o: Overloaded1, pos: SourceContext) = long_toint(lhs)
    
  def long_binaryand(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_shiftleft(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_toint(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Int]
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

  def obj_double_parse_double(s: Exp[String])(implicit pos: SourceContext) = ObjDoubleParseDouble(s)
  def obj_double_positive_infinity(implicit pos: SourceContext) = ObjDoublePositiveInfinity()
  def obj_double_negative_infinity(implicit pos: SourceContext) = ObjDoubleNegativeInfinity()
  def obj_double_min_value(implicit pos: SourceContext) = ObjDoubleMinValue()
  def obj_double_max_value(implicit pos: SourceContext) = ObjDoubleMaxValue()
  def double_float_value(lhs: Exp[Double])(implicit pos: SourceContext) = DoubleFloatValue(lhs)

  /**
   * Int
   */
  case class ObjIntegerParseInt(s: Exp[String]) extends Def[Int]
  case class ObjIntMaxValue() extends Def[Int]
  case class ObjIntMinValue() extends Def[Int]
  case class IntPlus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMinus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntTimes(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntDivideFrac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]
  case class IntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMod(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryOr(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryAnd(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryXor(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftLeft(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightArith(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightLogical(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntDoubleValue(lhs: Exp[Int]) extends Def[Double]
  case class IntFloatValue(lhs: Exp[Int]) extends Def[Float]
  case class IntBitwiseNot(lhs: Exp[Int]) extends Def[Int]
  case class IntToLong(lhs: Exp[Int]) extends Def[Long]

  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext) = ObjIntegerParseInt(s)
  def obj_int_max_value(implicit pos: SourceContext) = ObjIntMaxValue()
  def obj_int_min_value(implicit pos: SourceContext) = ObjIntMinValue()
  def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = IntPlus(lhs, rhs)
  def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = IntMinus(lhs, rhs)
  def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = IntTimes(lhs, rhs)
  def int_divide_frac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A])(implicit pos: SourceContext) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = IntDivide(lhs, rhs)
  def int_mod(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntMod(lhs, rhs)
  def int_binaryor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryOr(lhs, rhs)
  def int_binaryand(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryAnd(lhs, rhs)
  def int_binaryxor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryXor(lhs, rhs)
  def int_double_value(lhs: Exp[Int])(implicit pos: SourceContext) = IntDoubleValue(lhs)
  def int_float_value(lhs: Exp[Int])(implicit pos: SourceContext) = IntFloatValue(lhs)
  def int_bitwise_not(lhs: Exp[Int])(implicit pos: SourceContext) = IntBitwiseNot(lhs)
  def int_tolong(lhs: Exp[Int])(implicit pos: SourceContext) = IntToLong(lhs)
  def int_leftshift(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftLeft(lhs, rhs)
  def int_rightshiftarith(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftRightArith(lhs, rhs)
  def int_rightshiftlogical(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftRightLogical(lhs, rhs)


  /**
   * Long
   */
  case class LongBinaryOr(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongShiftLeft(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightUnsigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongToInt(lhs: Exp[Long]) extends Def[Int]

  def long_binaryor(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = LongBinaryOr(lhs,rhs)
  def long_binaryand(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = LongBinaryAnd(lhs,rhs)  
  def long_shiftleft(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = LongShiftLeft(lhs,rhs)
  def long_shiftright_unsigned(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = LongShiftRightUnsigned(lhs,rhs)
  def long_toint(lhs: Exp[Long])(implicit pos: SourceContext) = LongToInt(lhs)
    
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = ({
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    e match {
      case ObjDoubleParseDouble(x) => obj_double_parse_double(f(x))
      case ObjDoublePositiveInfinity() => obj_double_positive_infinity
      case ObjDoubleMaxValue() => obj_double_max_value
      case DoubleFloatValue(x) => double_float_value(f(x))
      case ObjIntegerParseInt(x) => obj_integer_parse_int(f(x))
      case IntDoubleValue(x) => int_double_value(f(x))
      case IntFloatValue(x) => int_float_value(f(x))
      case IntBitwiseNot(x) => int_bitwise_not(f(x))
      case IntPlus(x,y) => int_plus(f(x),f(y))
      case IntMinus(x,y) => int_minus(f(x),f(y))
      case IntTimes(x,y) => int_times(f(x),f(y))
      case IntDivide(x,y) => int_divide(f(x),f(y))
      case IntMod(x,y) => int_mod(f(x),f(y))
      case IntToLong(x) => int_tolong(f(x))
      case IntShiftLeft(x,y) => int_leftshift(f(x),f(y))
      case IntShiftRightLogical(x,y) => int_rightshiftlogical(f(x),f(y))
      case IntShiftRightArith(x,y) => int_rightshiftarith(f(x),f(y))
      case LongShiftLeft(x,y) => long_shiftleft(f(x),f(y))
      case LongBinaryAnd(x,y) => long_binaryand(f(x),f(y))
      case LongToInt(x) => long_toint(f(x))
      case LongShiftRightUnsigned(x,y) => long_shiftright_unsigned(f(x),f(y))
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "scala.Double.PositiveInfinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "scala.Double.NegativeInfinity")
    case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
    case ObjDoubleMaxValue() => emitValDef(sym, "scala.Double.MaxValue")
    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
    case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
    case ObjIntMinValue() => emitValDef(sym, "scala.Int.MinValue")
    case IntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case IntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case IntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case IntShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case IntShiftRightArith(lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case IntShiftRightLogical(lhs, rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
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
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      //case ObjDoubleParseDouble(s) => emitValDef(sym, "atof(" + quote(s) + ")")
      case ObjDoublePositiveInfinity() => emitValDef(sym, "DBL_MAX")
      //case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
      case DoubleFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
      //case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
      //case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
      case IntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
      case IntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
      case IntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
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

