package scala.lms
package common

import scala.reflect.SourceContext
import scala.lms.util.OverloadHack
import scala.lms.internal._

import java.io.PrintWriter

trait LiftPrimitives { this: PrimitiveOps =>
  implicit def intToRepInt(x: Int) = unit(x)
  implicit def floatToRepFloat(x: Float) = unit(x)
  implicit def doubleToRepDouble(x: Double) = unit(x)
  implicit def longToRepLong(x: Long) = unit(x)

  // precision-widening promotions
  implicit def chainIntToRepFloat[A:Typ](x: A)(implicit c: A => Rep[Int], pos: SourceContext): Rep[Float] = repIntToRepFloat(c(x))
  implicit def chainFloatToRepDouble[A:Typ](x: A)(implicit c: A => Rep[Float], pos: SourceContext): Rep[Double] = repFloatToRepDouble(c(x))
}

/**
 * This file is extremely boilerplate and redundant and does not take advantage of any of
 * Scala's type hierarchy to reduce the amount of IR nodes or code generation required.
 * It is in semi-desperate need of a refactor.
 */
trait PrimitiveOps extends Variables with OverloadHack { this: ImplicitOps =>

  implicit def byteTyp: Typ[Byte]
  implicit def charTyp: Typ[Char]
  implicit def shortTyp: Typ[Short]
  //implicit def intTyp: Typ[Int]
  implicit def longTyp: Typ[Long]
  //implicit def floatTyp: Typ[Float]
  //implicit def doubleTyp: Typ[Double]

  /**
   * Primitive implicit conversions
   */
  implicit def repIntToRepDouble(x: Rep[Int])(implicit pos: SourceContext): Rep[Double] = int_to_double(x)
  implicit def repIntToRepFloat(x: Rep[Int])(implicit pos: SourceContext): Rep[Float] = int_to_float(x)
  implicit def repIntToRepLong(x: Rep[Int])(implicit pos: SourceContext): Rep[Long] = int_to_long(x)
  implicit def repFloatToRepDouble(x: Rep[Float])(implicit pos: SourceContext): Rep[Double] = float_to_double(x)
  implicit def repLongToRepFloat(x: Rep[Long])(implicit pos: SourceContext): Rep[Float] = long_to_float(x)
  implicit def repLongToRepDouble(x: Rep[Long])(implicit pos: SourceContext): Rep[Double] = long_to_double(x)


  /**
   *  Double
   */
  implicit def doubleToDoubleOps(n: Double): DoubleOpsCls = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]): DoubleOpsCls = new DoubleOpsCls(n)
  implicit def varDoubleToDoubleOps(n: Var[Double])(implicit pos: SourceContext): DoubleOpsCls = new DoubleOpsCls(readVar(n))

  object Double {
    def parseDouble(s: Rep[String])(implicit pos: SourceContext) = obj_double_parse_double(s)
    def PositiveInfinity(implicit pos: SourceContext) = obj_double_positive_infinity
    def NegativeInfinity(implicit pos: SourceContext) = obj_double_negative_infinity
    def MinValue(implicit pos: SourceContext) = obj_double_min_value
    def MaxValue(implicit pos: SourceContext) = obj_double_max_value
  }

  class DoubleOpsCls(lhs: Rep[Double]){
    def floatValue()(implicit pos: SourceContext) = double_float_value(lhs)
    def toInt(implicit pos: SourceContext) = double_to_int(lhs)
    def toFloat(implicit pos: SourceContext) = double_to_float(lhs)
  }

  // --- Double object operations
  def obj_double_parse_double(s: Rep[String])(implicit pos: SourceContext): Rep[Double]
  def obj_double_positive_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_negative_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_min_value(implicit pos: SourceContext): Rep[Double]
  def obj_double_max_value(implicit pos: SourceContext): Rep[Double]
  // --- Double primitive math
  def double_plus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_minus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_times(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_divide(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  // --- Double casting
  def double_float_value(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float]
  def double_to_int(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Int]
  def double_to_float(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float]
  // --- Double infix operations
  def infix_+(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded25, ctx: SourceContext): Rep[Double] = double_plus(lhs,rhs)
  def infix_-(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded11, ctx: SourceContext): Rep[Double] = double_minus(lhs,rhs)
  def infix_*(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded11, ctx: SourceContext): Rep[Double] = double_times(lhs,rhs)
  def infix_/(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded11, ctx: SourceContext): Rep[Double] = double_divide(lhs,rhs)


  /**
   * Float
   */

  implicit def floatToFloatOps(n: Float): FloatOpsCls = new FloatOpsCls(unit(n))
  implicit def repFloatToFloatOps(n: Rep[Float]): FloatOpsCls = new FloatOpsCls(n)
  implicit def varFloatToFloatOps(n: Var[Float])(implicit pos: SourceContext): FloatOpsCls = new FloatOpsCls(readVar(n))

  object Float {
    def parseFloat(s: Rep[String])(implicit pos: SourceContext) = obj_float_parse_float(s)
    // TODO: MaxValue
    // TODO: MinValue
    // TODO: PositiveInfinity
    // TODO: NegativeInfinity
  }

  class FloatOpsCls(lhs: Rep[Float]) {
    def toInt(implicit pos: SourceContext): Rep[Int] = float_to_int(lhs)
    def toDouble(implicit pos: SourceContext): Rep[Double] = float_to_double(lhs)
  }

  // --- Float object operations
  def obj_float_parse_float(s: Rep[String])(implicit pos: SourceContext): Rep[Float]
  // TODO: MaxValue
  // TODO: MinValue
  // TODO: PositiveInfinity
  // TODO: NegativeInfinity
  // --- Float primitive math
  def float_plus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_minus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_times(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_divide(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  // --- Float casting
  def float_to_int(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Int]
  def float_to_double(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Double]
  // --- Float infix operations
  def infix_+(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded20, ctx: SourceContext): Rep[Float] = float_plus(lhs, rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded6, ctx: SourceContext): Rep[Float] = float_minus(lhs, rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded6, ctx: SourceContext): Rep[Float] = float_times(lhs, rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded6, ctx: SourceContext): Rep[Float] = float_divide(lhs, rhs)


  /**
   * Int
   */

  implicit def intToIntOps(n: Int): IntOpsCls = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]): IntOpsCls = new IntOpsCls(n)
  implicit def varIntToIntOps(n: Var[Int])(implicit pos: SourceContext): IntOpsCls = new IntOpsCls(readVar(n))

  object Integer {
    def parseInt(s: Rep[String])(implicit pos: SourceContext) = obj_integer_parse_int(s)
  }

  object Int {
    def MaxValue(implicit pos: SourceContext) = obj_int_max_value
    def MinValue(implicit pos: SourceContext) = obj_int_min_value
  }

  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash
    //def /[A](rhs: Rep[A])(implicit mA: Typ[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
    // TODO Something is wrong if we just use floatValue. implicits get confused
    def floatValueL()(implicit pos: SourceContext) = int_float_value(lhs)
    def doubleValue()(implicit pos: SourceContext) = int_double_value(lhs)
    def unary_~()(implicit pos: SourceContext) = int_bitwise_not(lhs)
    def toLong(implicit pos: SourceContext) = int_to_long(lhs)
    def toDouble(implicit pos: SourceContext) = int_to_double(lhs)
    def toFloat(implicit pos: SourceContext) = int_to_float(lhs)
  }

  // --- Int/Integer object operations
  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
  def obj_int_max_value(implicit pos: SourceContext): Rep[Int]
  def obj_int_min_value(implicit pos: SourceContext): Rep[Int]
  // --- Int primitive math
  def int_plus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_minus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_times(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  // def int_divide_frac[A:Typ:Fractional](lhs: Rep[Int], rhs: Rep[A])(implicit pos: SourceContext): Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_mod(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  // --- Int binary operations
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_bitwise_not(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Int]
  // --- Int shifts
  def int_leftshift(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftarith(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftlogical(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  // --- Int casting
  def int_float_value(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Float]
  def int_double_value(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Double]
  def int_to_long(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Long]
  def int_to_float(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Float]
  def int_to_double(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Double]
  // --- Int infix operations
  def infix_%(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_mod(lhs, rhs)
  def infix_&(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_binaryor(lhs, rhs)
  def infix_^(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_binaryxor(lhs, rhs)
  def infix_<<(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_leftshift(lhs, rhs)
  def infix_>>(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_rightshiftarith(lhs, rhs)
  def infix_>>>(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_rightshiftlogical(lhs, rhs)


  /**
   * Long
   */
  object Long {
    def parseLong(s: Rep[String])(implicit pos: SourceContext) = obj_long_parse_long(s)
    def MaxValue(implicit pos: SourceContext) = obj_long_max_value
    def MinValue(implicit pos: SourceContext) = obj_long_min_value
  }

  // --- Long object operations
  def obj_long_parse_long(s: Rep[String])(implicit pos: SourceContext): Rep[Long]
  def obj_long_max_value(implicit pos: SourceContext): Rep[Long]
  def obj_long_min_value(implicit pos: SourceContext): Rep[Long]
  // --- Long primitive math
  def long_plus(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_minus(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_times(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_divide(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_mod(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  // --- Long binary operations
  def long_binaryand(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryxor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  // --- Long shifts
  def long_shiftleft(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_shiftright_signed(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  // --- Long casting
  def long_to_int(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Int]
  def long_to_float(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Float]
  def long_to_double(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Double]
  // --- Long infix operations
  def infix_+(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded30, ctx: SourceContext): Rep[Long] = long_plus(lhs, rhs)
  def infix_-(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded16, ctx: SourceContext): Rep[Long] = long_minus(lhs, rhs)
  def infix_*(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded16, ctx: SourceContext): Rep[Long] = long_times(lhs, rhs)
  def infix_/(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded16, ctx: SourceContext): Rep[Long] = long_divide(lhs, rhs)
  def infix_%(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_mod(lhs, rhs)
  def infix_&(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_binaryor(lhs, rhs)
  def infix_^(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_binaryxor(lhs, rhs)
  def infix_<<(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext) = long_shiftleft(lhs, rhs)
  def infix_>>(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext) = long_shiftright_signed(lhs, rhs)
  def infix_>>>(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext) = long_shiftright_unsigned(lhs, rhs)
  def infix_toInt(lhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_to_int(lhs)
  def infix_toDouble(lhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_to_double(lhs)
  def infix_toFloat(lhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_to_float(lhs)

  /**
   * Enumerate all combinations of primitive math.
   * Avoids certain fragile behavior, including compiler crashes and some erroneous or inaccessible type errors.
   */
  def infix_-(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_minus(unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_minus(unit(lhs),rhs)
  def infix_-(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Double, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Long, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long] = long_minus(unit(lhs), rhs)
  def infix_-(lhs: Long, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Long, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Long, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_minus(unit(lhs), rhs)
  def infix_-(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Long)(implicit ctx: SourceContext): Rep[Long] = long_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Long)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Long)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Long], rhs: Int)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Long], rhs: Float)(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Long], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Long], rhs: Long)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_minus(lhs, rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_minus(repIntToRepFloat(lhs), rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_minus(repIntToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Long])(implicit o: Overloaded4, ctx: SourceContext): Rep[Long] = long_minus(repIntToRepLong(lhs), rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_minus(lhs,repIntToRepFloat(rhs))
  def infix_-(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_minus(repFloatToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Long])(implicit o: Overloaded8, ctx: SourceContext): Rep[Float] = float_minus(lhs, repLongToRepFloat(rhs))
  def infix_-(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_minus(lhs, repIntToRepDouble(rhs))
  def infix_-(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded10, ctx: SourceContext): Rep[Double] = double_minus(lhs, repFloatToRepDouble(rhs))
  def infix_-(lhs: Rep[Double], rhs: Rep[Long])(implicit o: Overloaded12, ctx: SourceContext): Rep[Double] = double_minus(lhs, repLongToRepDouble(rhs))
  def infix_-(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded13, ctx: SourceContext): Rep[Long] = long_minus(lhs, repIntToRepLong(rhs))
  def infix_-(lhs: Rep[Long], rhs: Rep[Float])(implicit o: Overloaded14, ctx: SourceContext): Rep[Float] = float_minus(repLongToRepFloat(lhs), rhs)
  def infix_-(lhs: Rep[Long], rhs: Rep[Double])(implicit o: Overloaded15, ctx: SourceContext): Rep[Double] = double_minus(repLongToRepDouble(lhs), rhs)

  def infix_+(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_plus(unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_plus(unit(lhs),rhs)
  def infix_+(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Double, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Long, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long] = long_plus(unit(lhs), rhs)
  def infix_+(lhs: Long, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Long, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Long, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_plus(unit(lhs), rhs)
  def infix_+(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Long)(implicit ctx: SourceContext): Rep[Long] = long_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Long)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Long)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Long], rhs: Int)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Long], rhs: Float)(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Long], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Long], rhs: Long)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded15, ctx: SourceContext): Rep[Int] = int_plus(lhs, rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded16, ctx: SourceContext): Rep[Float] = float_plus(repIntToRepFloat(lhs), rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded17, ctx: SourceContext): Rep[Double] = double_plus(repIntToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Long])(implicit o: Overloaded18, ctx: SourceContext): Rep[Long] = long_plus(repIntToRepLong(lhs), rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded19, ctx: SourceContext): Rep[Float] = float_plus(lhs,repIntToRepFloat(rhs))
  def infix_+(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded21, ctx: SourceContext): Rep[Double] = double_plus(repFloatToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Long])(implicit o: Overloaded22, ctx: SourceContext): Rep[Float] = float_plus(lhs, repLongToRepFloat(rhs))
  def infix_+(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded23, ctx: SourceContext): Rep[Double] = double_plus(lhs, repIntToRepDouble(rhs))
  def infix_+(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded24, ctx: SourceContext): Rep[Double] = double_plus(lhs, repFloatToRepDouble(rhs))
  def infix_+(lhs: Rep[Double], rhs: Rep[Long])(implicit o: Overloaded26, ctx: SourceContext): Rep[Double] = double_plus(lhs, repLongToRepDouble(rhs))
  def infix_+(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded27, ctx: SourceContext): Rep[Long] = long_plus(lhs, repIntToRepLong(rhs))
  def infix_+(lhs: Rep[Long], rhs: Rep[Float])(implicit o: Overloaded28, ctx: SourceContext): Rep[Float] = float_plus(repLongToRepFloat(lhs), rhs)
  def infix_+(lhs: Rep[Long], rhs: Rep[Double])(implicit o: Overloaded29, ctx: SourceContext): Rep[Double] = double_plus(repLongToRepDouble(lhs), rhs)

  def infix_*(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_times(unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_times(unit(lhs),rhs)
  def infix_*(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Double, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Long, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long] = long_times(unit(lhs), rhs)
  def infix_*(lhs: Long, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Long, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Long, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_times(unit(lhs), rhs)
  def infix_*(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Long)(implicit ctx: SourceContext): Rep[Long] = long_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Long)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Long)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Long], rhs: Int)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Long], rhs: Float)(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Long], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Long], rhs: Long)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_times(lhs, rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_times(repIntToRepFloat(lhs), rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_times(repIntToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Long])(implicit o: Overloaded4, ctx: SourceContext): Rep[Long] = long_times(repIntToRepLong(lhs), rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_times(lhs,repIntToRepFloat(rhs))
  def infix_*(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_times(repFloatToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Long])(implicit o: Overloaded8, ctx: SourceContext): Rep[Float] = float_times(lhs, repLongToRepFloat(rhs))
  def infix_*(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_times(lhs, repIntToRepDouble(rhs))
  def infix_*(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded10, ctx: SourceContext): Rep[Double] = double_times(lhs, repFloatToRepDouble(rhs))
  def infix_*(lhs: Rep[Double], rhs: Rep[Long])(implicit o: Overloaded12, ctx: SourceContext): Rep[Double] = double_times(lhs, repLongToRepDouble(rhs))
  def infix_*(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded13, ctx: SourceContext): Rep[Long] = long_times(lhs, repIntToRepLong(rhs))
  def infix_*(lhs: Rep[Long], rhs: Rep[Float])(implicit o: Overloaded14, ctx: SourceContext): Rep[Float] = float_times(repLongToRepFloat(lhs), rhs)
  def infix_*(lhs: Rep[Long], rhs: Rep[Double])(implicit o: Overloaded15, ctx: SourceContext): Rep[Double] = double_times(repLongToRepDouble(lhs), rhs)

  def infix_/(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_divide(unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_divide(unit(lhs),rhs)
  def infix_/(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Double, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Long, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long] = long_divide(unit(lhs), rhs)
  def infix_/(lhs: Long, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Long, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Long, rhs: Rep[Long])(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_divide(unit(lhs), rhs)
  def infix_/(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Long)(implicit ctx: SourceContext): Rep[Long] = long_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Long)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Long)(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Long], rhs: Int)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Long], rhs: Float)(implicit o: Overloaded3, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Long], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Long], rhs: Long)(implicit o: Overloaded3, ctx: SourceContext): Rep[Long] = long_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_divide(lhs, rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_divide(repIntToRepFloat(lhs), rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_divide(repIntToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Long])(implicit o: Overloaded4, ctx: SourceContext): Rep[Long] = long_divide(repIntToRepLong(lhs), rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_divide(lhs,repIntToRepFloat(rhs))
  def infix_/(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_divide(repFloatToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Long])(implicit o: Overloaded8, ctx: SourceContext): Rep[Float] = float_divide(lhs, repLongToRepFloat(rhs))
  def infix_/(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_divide(lhs, repIntToRepDouble(rhs))
  def infix_/(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded10, ctx: SourceContext): Rep[Double] = double_divide(lhs, repFloatToRepDouble(rhs))
  def infix_/(lhs: Rep[Double], rhs: Rep[Long])(implicit o: Overloaded12, ctx: SourceContext): Rep[Double] = double_divide(lhs, repLongToRepDouble(rhs))
  def infix_/(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded13, ctx: SourceContext): Rep[Long] = long_divide(lhs, repIntToRepLong(rhs))
  def infix_/(lhs: Rep[Long], rhs: Rep[Float])(implicit o: Overloaded14, ctx: SourceContext): Rep[Float] = float_divide(repLongToRepFloat(lhs), rhs)
  def infix_/(lhs: Rep[Long], rhs: Rep[Double])(implicit o: Overloaded15, ctx: SourceContext): Rep[Double] = double_divide(repLongToRepDouble(lhs), rhs)
}

trait PrimitiveOpsExp extends PrimitiveOps with BaseExp { this: ImplicitOps =>

  implicit def byteTyp: Typ[Byte] = manifestTyp
  implicit def charTyp: Typ[Char] = manifestTyp
  implicit def shortTyp: Typ[Short] = manifestTyp
  implicit def intTyp: Typ[Int] = manifestTyp
  implicit def longTyp: Typ[Long] = manifestTyp
  implicit def floatTyp: Typ[Float] = manifestTyp
  implicit def doubleTyp: Typ[Double] = manifestTyp

  /**
   * Double
   */
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]
  case class ObjDoublePositiveInfinity() extends Def[Double]
  case class ObjDoubleNegativeInfinity() extends Def[Double]
  case class ObjDoubleMinValue() extends Def[Double]
  case class ObjDoubleMaxValue() extends Def[Double]

  case class DoublePlus(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleMinus(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleTimes(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleDivide(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]

  case class DoubleFloatValue(lhs: Exp[Double]) extends Def[Float]
  case class DoubleToInt(lhs: Exp[Double]) extends Def[Int]
  case class DoubleToFloat(lhs: Exp[Double]) extends Def[Float]

  def obj_double_parse_double(s: Exp[String])(implicit pos: SourceContext) = toAtom(ObjDoubleParseDouble(s))
  def obj_double_positive_infinity(implicit pos: SourceContext) = toAtom(ObjDoublePositiveInfinity())
  def obj_double_negative_infinity(implicit pos: SourceContext) = toAtom(ObjDoubleNegativeInfinity())
  def obj_double_min_value(implicit pos: SourceContext) = toAtom(ObjDoubleMinValue())
  def obj_double_max_value(implicit pos: SourceContext) = toAtom(ObjDoubleMaxValue())

  def double_plus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = toAtom(DoublePlus(lhs,rhs))
  def double_minus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = toAtom(DoubleMinus(lhs,rhs))
  def double_times(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = toAtom(DoubleTimes(lhs,rhs))
  def double_divide(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = toAtom(DoubleDivide(lhs,rhs))

  def double_float_value(lhs: Exp[Double])(implicit pos: SourceContext) = toAtom(DoubleFloatValue(lhs))
  def double_to_int(lhs: Exp[Double])(implicit pos: SourceContext) = toAtom(DoubleToInt(lhs))
  def double_to_float(lhs: Exp[Double])(implicit pos: SourceContext) = toAtom(DoubleToFloat(lhs))


  /**
   * Float
   */
  case class ObjFloatParseFloat(s: Exp[String]) extends Def[Float]
  case class FloatToInt(lhs: Exp[Float]) extends Def[Int]
  case class FloatToDouble(lhs: Exp[Float]) extends Def[Double]
  case class FloatPlus(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]
  case class FloatMinus(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]
  case class FloatTimes(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]
  case class FloatDivide(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]

  def obj_float_parse_float(s: Exp[String])(implicit pos: SourceContext) = toAtom(ObjFloatParseFloat(s))

  def float_plus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = toAtom(FloatPlus(lhs,rhs))
  def float_minus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = toAtom(FloatMinus(lhs,rhs))
  def float_times(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = toAtom(FloatTimes(lhs,rhs))
  def float_divide(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = toAtom(FloatDivide(lhs,rhs))

  def float_to_int(lhs: Exp[Float])(implicit pos: SourceContext) = toAtom(FloatToInt(lhs))
  def float_to_double(lhs: Exp[Float])(implicit pos: SourceContext) = toAtom(FloatToDouble(lhs))


  /**
   * Int
   */
  case class ObjIntegerParseInt(s: Exp[String]) extends Def[Int]
  case class ObjIntMaxValue() extends Def[Int]
  case class ObjIntMinValue() extends Def[Int]

  case class IntPlus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMinus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntTimes(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  // case class IntDivideFrac[A:Typ:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]
  case class IntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMod(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]

  case class IntBinaryOr(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryAnd(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryXor(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBitwiseNot(lhs: Exp[Int]) extends Def[Int]

  case class IntShiftLeft(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightArith(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightLogical(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]

  case class IntDoubleValue(lhs: Exp[Int]) extends Def[Double]
  case class IntFloatValue(lhs: Exp[Int]) extends Def[Float]
  case class IntToLong(lhs: Exp[Int]) extends Def[Long]
  case class IntToFloat(lhs: Exp[Int]) extends Def[Float]
  case class IntToDouble(lhs: Exp[Int]) extends Def[Double]

  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext) = toAtom(ObjIntegerParseInt(s))
  def obj_int_max_value(implicit pos: SourceContext) = toAtom(ObjIntMaxValue())
  def obj_int_min_value(implicit pos: SourceContext) = toAtom(ObjIntMinValue())

  def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = toAtom(IntPlus(lhs,rhs))
  def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = toAtom(IntMinus(lhs,rhs))
  def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = toAtom(IntTimes(lhs,rhs))
  // def int_divide_frac[A:Typ:Fractional](lhs: Exp[Int], rhs: Exp[A])(implicit pos: SourceContext) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = toAtom(IntDivide(lhs, rhs))
  def int_mod(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntMod(lhs, rhs))

  def int_binaryor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntBinaryOr(lhs, rhs))
  def int_binaryand(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntBinaryAnd(lhs, rhs))
  def int_binaryxor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntBinaryXor(lhs, rhs))
  def int_bitwise_not(lhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntBitwiseNot(lhs))

  def int_leftshift(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntShiftLeft(lhs, rhs))
  def int_rightshiftarith(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntShiftRightArith(lhs, rhs))
  def int_rightshiftlogical(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntShiftRightLogical(lhs, rhs))

  def int_double_value(lhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntDoubleValue(lhs))
  def int_float_value(lhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntFloatValue(lhs))
  def int_tolong(lhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntToLong(lhs))
  def int_to_float(lhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntToFloat(lhs))
  def int_to_double(lhs: Exp[Int])(implicit pos: SourceContext) = toAtom(IntToDouble(lhs))


  /**
   * Long
   */
  case class ObjLongParseLong(s: Exp[String]) extends Def[Long]
  case class ObjLongMaxValue() extends Def[Long]
  case class ObjLongMinValue() extends Def[Long]

  case class LongPlus(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongMinus(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongTimes(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongDivide(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongMod(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]

  case class LongBinaryOr(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryXor(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]

  case class LongShiftLeft(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightSigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightUnsigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]

  case class LongToInt(lhs: Exp[Long]) extends Def[Int]
  case class LongToFloat(lhs: Exp[Long]) extends Def[Float]
  case class LongToDouble(lhs: Exp[Long]) extends Def[Double]

  def obj_long_parse_long(s: Exp[String])(implicit pos: SourceContext) = toAtom(ObjLongParseLong(s))
  def obj_long_max_value(implicit pos: SourceContext) = toAtom(ObjLongMaxValue())
  def obj_long_min_value(implicit pos: SourceContext) = toAtom(ObjLongMinValue())

  def long_plus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = toAtom(LongPlus(lhs, rhs))
  def long_minus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = toAtom(LongMinus(lhs, rhs))
  def long_times(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = toAtom(LongTimes(lhs, rhs))
  def long_divide(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = toAtom(LongDivide(lhs, rhs))
  def long_mod(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = toAtom(LongMod(lhs, rhs))

  def long_binaryor(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = toAtom(LongBinaryOr(lhs,rhs))
  def long_binaryand(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = toAtom(LongBinaryAnd(lhs,rhs))
  def long_binaryxor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext) = toAtom(LongBinaryXor(lhs,rhs))

  def long_shiftleft(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(LongShiftLeft(lhs,rhs))
  def long_shiftright_signed(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext) = toAtom(LongShiftRightSigned(lhs,rhs))
  def long_shiftright_unsigned(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = toAtom(LongShiftRightUnsigned(lhs,rhs))

  def long_to_int(lhs: Exp[Long])(implicit pos: SourceContext) = toAtom(LongToInt(lhs))
  def long_to_float(lhs: Exp[Long])(implicit pos: SourceContext) = toAtom(LongToFloat(lhs))
  def long_to_double(lhs: Exp[Long])(implicit pos: SourceContext) = toAtom(LongToDouble(lhs))

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case ObjDoubleParseDouble(x) => obj_double_parse_double(f(x))
    case ObjDoublePositiveInfinity() => obj_double_positive_infinity
    case ObjDoubleNegativeInfinity() => obj_double_negative_infinity
    case ObjDoubleMinValue() => obj_double_min_value
    case ObjDoubleMaxValue() => obj_double_max_value
    case DoubleFloatValue(x) => double_float_value(f(x))
    case DoubleToInt(x) => double_to_int(f(x))
    case DoubleToFloat(x) => double_to_float(f(x))
    case DoublePlus(x,y) => double_plus(f(x),f(y))
    case DoubleMinus(x,y) => double_minus(f(x),f(y))
    case DoubleTimes(x,y) => double_times(f(x),f(y))
    case DoubleDivide(x,y) => double_divide(f(x),f(y))
    case ObjFloatParseFloat(x) => obj_float_parse_float(f(x))
    case FloatToInt(x) => float_to_int(f(x))
    case FloatToDouble(x) => float_to_double(f(x))
    case FloatPlus(x,y) => float_plus(f(x),f(y))
    case FloatMinus(x,y) => float_minus(f(x),f(y))
    case FloatTimes(x,y) => float_times(f(x),f(y))
    case FloatDivide(x,y) => float_divide(f(x),f(y))
    case ObjIntegerParseInt(x) => obj_integer_parse_int(f(x))
    case ObjIntMaxValue() => obj_int_max_value
    case ObjIntMinValue() => obj_int_min_value
    case IntDoubleValue(x) => int_double_value(f(x))
    case IntFloatValue(x) => int_float_value(f(x))
    case IntBitwiseNot(x) => int_bitwise_not(f(x))
    case IntPlus(x,y) => int_plus(f(x),f(y))
    case IntMinus(x,y) => int_minus(f(x),f(y))
    case IntTimes(x,y) => int_times(f(x),f(y))
    case IntDivide(x,y) => int_divide(f(x),f(y))
    case IntMod(x,y) => int_mod(f(x),f(y))
    case IntBinaryOr(x,y) => int_binaryor(f(x),f(y))
    case IntBinaryAnd(x,y) => int_binaryand(f(x),f(y))
    case IntBinaryXor(x,y) => int_binaryxor(f(x),f(y))
    case IntToLong(x) => int_tolong(f(x))
    case IntToFloat(x) => int_to_float(f(x))
    case IntToDouble(x) => int_to_double(f(x))
    case IntShiftLeft(x,y) => int_leftshift(f(x),f(y))
    case IntShiftRightLogical(x,y) => int_rightshiftlogical(f(x),f(y))
    case IntShiftRightArith(x,y) => int_rightshiftarith(f(x),f(y))
    case ObjLongParseLong(x) => obj_long_parse_long(f(x))
    case ObjLongMaxValue() => obj_long_max_value
    case ObjLongMinValue() => obj_long_min_value
    case LongShiftLeft(x,y) => long_shiftleft(f(x),f(y))
    case LongBinaryOr(x,y) => long_binaryor(f(x),f(y))
    case LongBinaryAnd(x,y) => long_binaryand(f(x),f(y))
    case LongBinaryXor(x,y) => long_binaryxor(f(x),f(y))
    case LongToInt(x) => long_to_int(f(x))
    case LongMod(x,y) => long_mod(f(x),f(y))
    case LongShiftRightSigned(x,y) => long_shiftright_signed(f(x),f(y))
    case LongShiftRightUnsigned(x,y) => long_shiftright_unsigned(f(x),f(y))
    case LongToFloat(x) => long_to_float(f(x))
    case LongToDouble(x) => long_to_double(f(x))
    case LongPlus(x,y) => long_plus(f(x),f(y))
    case LongMinus(x,y) => long_minus(f(x),f(y))
    case LongTimes(x,y) => long_times(f(x),f(y))
    case LongDivide(x,y) => long_divide(f(x),f(y))
    case _ => super.mirror(e,f)
  }
}

trait PrimitiveOpsExpOpt extends PrimitiveOpsExp {
  override def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs,rhs) match {
    case (Const(a),Const(b)) => unit(a+b)
    case (Const(0),b) => b
    case (a,Const(0)) => a
    case _ => super.int_plus(lhs,rhs)
  }
  override def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs,rhs) match {
    case (Const(a),Const(b)) => unit(a-b)
    case (a,Const(0)) => a
    case (Def(IntPlus(llhs,lrhs)), rhs) if lrhs.equals(rhs) => llhs  // a + b - b
    case _ => super.int_minus(lhs,rhs)
  }
  override def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs,rhs) match {
    case (Const(a),Const(b)) => unit(a*b)
    case (Const(0),b) => Const(0)
    case (Const(1),b) => b
    case (a,Const(0)) => Const(0)
    case (a,Const(1)) => a
    case _ => super.int_times(lhs,rhs)
  }
  override def int_to_float(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Float] = lhs match {
    case Const(x) => Const(x.toFloat)
    case _ => super.int_to_float(lhs)
  }

  override def int_to_double(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Double] = lhs match {
    case Const(x) => Const(x.toDouble)
    case _ => super.int_to_double(lhs)
  }

  override def float_to_double(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Double] = lhs match {
    case Const(x) => Const(x.toDouble)
    case Def(IntToFloat(x)) => int_to_double(x)
    case _ => super.float_to_double(lhs)
  }

  override def double_to_int(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Int] = lhs match {
    case Const(x) => Const(x.toInt)
    case Def(IntToDouble(x)) => x
    case _ => super.double_to_int(lhs)
  }

  override def long_plus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = (lhs, rhs) match {
    case (Const(0l), r) => r
    case (l, Const(0l)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => super.long_plus(lhs,rhs)
  }

  override def long_minus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = (lhs, rhs) match {
    case (l, Const(0l)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => super.long_minus(lhs,rhs)
  }

  override def long_times(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = (lhs, rhs) match {
    case (l@Const(0l), r) => l
    case (l, r@Const(0l)) => r
    case (Const(1l), r) => r
    case (l, Const(1l)) => l
    case (Const(x), Const(y)) => Const(x*y)
    case _ => super.long_times(lhs,rhs)
  }
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, src"java.lang.Double.parseDouble($s)")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "scala.Double.PositiveInfinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "scala.Double.NegativeInfinity")
    case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
    case ObjDoubleMaxValue() => emitValDef(sym, "scala.Double.MaxValue")
    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case DoublePlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case DoubleMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case DoubleTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case DoubleDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case DoubleToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case DoubleToFloat(lhs) => emitValDef(sym, quote(lhs) + ".toFloat")
    case ObjFloatParseFloat(s) => emitValDef(sym, "java.lang.Float.parseFloat(" + quote(s) + ")")
    case FloatToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case FloatToDouble(lhs) => emitValDef(sym, quote(lhs) + ".toDouble")
    case FloatPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case FloatMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case FloatTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case FloatDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
    case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
    case ObjIntMinValue() => emitValDef(sym, "scala.Int.MinValue")
    case IntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case IntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case IntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
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
    case IntToFloat(lhs) => emitValDef(sym, quote(lhs) + ".toFloat")
    case IntToDouble(lhs) => emitValDef(sym, quote(lhs) + ".toDouble")
    case ObjLongParseLong(s) => emitValDef(sym, "java.lang.Long.parseLong(" + quote(s) + ")")
    case ObjLongMaxValue() => emitValDef(sym, "scala.Long.MaxValue")
    case ObjLongMinValue() => emitValDef(sym, "scala.Long.MinValue")
    case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case LongBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case LongShiftRightSigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
    case LongToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case LongToFloat(lhs) => emitValDef(sym, quote(lhs) + ".toFloat")
    case LongToDouble(lhs) => emitValDef(sym, quote(lhs) + ".toDouble")
    case LongPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case LongMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case LongTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case LongDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case LongMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenPrimitiveOps extends CLikeGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, "strtod(" + quote(s) + ".c_str(),NULL)")
    case ObjDoubleMinValue() => emitValDef(sym, "DBL_MIN")
    case ObjDoubleMaxValue() => emitValDef(sym, "DBL_MAX")
    case DoubleFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
    case DoublePlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case DoubleMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case DoubleTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case DoubleDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case DoubleToInt(lhs) => emitValDef(sym, "(int32_t)" + quote(lhs))
    case DoubleToFloat(lhs) => emitValDef(sym, "(float)" + quote(lhs))
    case ObjFloatParseFloat(s) => emitValDef(sym, "strtof(" + quote(s) + ".c_str(),NULL)")
    case FloatToInt(lhs) => emitValDef(sym, "(int32_t)" + quote(lhs))
    case FloatToDouble(lhs) => emitValDef(sym, "(double)" + quote(lhs))
    case FloatPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case FloatMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case FloatTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case FloatDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case ObjIntMaxValue() => emitValDef(sym, "INT32_MAX")
    case ObjIntMinValue() => emitValDef(sym, "INT32_MAX")
    case ObjIntegerParseInt(s) => emitValDef(sym, "atoi(" + quote(s) + ".c_str())")
    case IntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case IntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case IntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case IntShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case IntShiftRightArith(lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case IntShiftRightLogical(lhs, rhs) => emitValDef(sym, "(uint32_t)" + quote(lhs) + " >> " + quote(rhs))
    case IntDoubleValue(lhs) => emitValDef(sym, "(double)"+quote(lhs))
    case IntFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
    case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case IntToLong(lhs) => emitValDef(sym, "(int64_t)"+quote(lhs))
    case IntToFloat(lhs) => emitValDef(sym, "(float)"+quote(lhs))
    case IntToDouble(lhs) => emitValDef(sym, "(double)"+quote(lhs))
    case ObjLongMaxValue() => emitValDef(sym, "INT64_MAX")
    case ObjLongMinValue() => emitValDef(sym, "INT64_MIN")
    case ObjLongParseLong(s) => emitValDef(sym, "strtod(" + quote(s) + ".c_str(),NULL)")
    case LongMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case LongBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case LongShiftRightSigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, "(uint64_t)" + quote(lhs) + " >> " + quote(rhs))
    case LongToInt(lhs) => emitValDef(sym, "(int32_t)"+quote(lhs))
    case LongToFloat(lhs) => emitValDef(sym, "(float)"+quote(lhs))
    case LongToDouble(lhs) => emitValDef(sym, "(double)"+quote(lhs))
    case LongPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case LongMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case LongTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case LongDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case LongMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoublePositiveInfinity() => emitValDef(sym, "__longlong_as_double(0x7ff0000000000000ULL)")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "__longlong_as_double(0xfff0000000000000ULL)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenPrimitiveOps extends OpenCLGenBase with CLikeGenPrimitiveOps

trait CGenPrimitiveOps extends CGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoublePositiveInfinity() => emitValDef(sym, "INFINITY")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "-INFINITY")
    case _ => super.emitNode(sym, rhs)
  }
}
