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
  implicit def repIntToRepDouble(x: Rep[Int]): Rep[Double] = x.toDouble
  implicit def repIntToRepFloat(x: Rep[Int]): Rep[Float] = x.toFloat
  implicit def repFloatToRepDouble(x: Rep[Float]): Rep[Double] = x.toDouble
      
  
  /**
   * Enumerate all combinations of primitive math.
   * Avoids certain fragile behavior, including compiler crashes and some erroneous or inaccessible type errors.
   */  
  def infix_-(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_minus(unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_minus(unit(lhs),rhs)  
  def infix_-(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_minus(unit(lhs),rhs)  
  def infix_-(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs),rhs)
  def infix_-(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs),rhs)
  def infix_-(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_minus(lhs, unit(rhs))  
  def infix_-(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_minus(lhs, rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_minus(repIntToRepFloat(lhs), rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_minus(repIntToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = float_minus(lhs,repIntToRepFloat(rhs))  
  def infix_-(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_minus(lhs, rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_minus(repFloatToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_minus(lhs,repIntToRepDouble(rhs))  
  def infix_-(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = double_minus(lhs,repFloatToRepDouble(rhs))
  def infix_-(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_minus(lhs,rhs)    

  def infix_+(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_plus(unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_plus(unit(lhs),rhs)  
  def infix_+(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_plus(unit(lhs),rhs)  
  def infix_+(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs),rhs)
  def infix_+(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs),rhs)
  def infix_+(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_plus(lhs, unit(rhs))  
  def infix_+(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded15, ctx: SourceContext): Rep[Int] = int_plus(lhs, rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded16, ctx: SourceContext): Rep[Float] = float_plus(repIntToRepFloat(lhs), rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded17, ctx: SourceContext): Rep[Double] = double_plus(repIntToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded18, ctx: SourceContext): Rep[Float] = float_plus(lhs,repIntToRepFloat(rhs))  
  def infix_+(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded19, ctx: SourceContext): Rep[Float] = float_plus(lhs, rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded20, ctx: SourceContext): Rep[Double] = double_plus(repFloatToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded21, ctx: SourceContext): Rep[Double] = double_plus(lhs,repIntToRepDouble(rhs))  
  def infix_+(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded22, ctx: SourceContext): Rep[Double] = double_plus(lhs,repFloatToRepDouble(rhs))
  def infix_+(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded23, ctx: SourceContext): Rep[Double] = double_plus(lhs,rhs)    

  def infix_*(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_times(unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_times(unit(lhs),rhs)  
  def infix_*(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_times(unit(lhs),rhs)  
  def infix_*(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_times(unit(lhs),rhs)
  def infix_*(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs),rhs)
  def infix_*(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_times(lhs, unit(rhs))  
  def infix_*(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_times(lhs, rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_times(repIntToRepFloat(lhs), rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_times(repIntToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = float_times(lhs,repIntToRepFloat(rhs))  
  def infix_*(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_times(lhs, rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_times(repFloatToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_times(lhs,repIntToRepDouble(rhs))  
  def infix_*(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = double_times(lhs,repFloatToRepDouble(rhs))
  def infix_*(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_times(lhs,rhs)    

  def infix_/(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_divide(unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_divide(unit(lhs),rhs)  
  def infix_/(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_divide(unit(lhs),rhs)  
  def infix_/(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs),rhs)
  def infix_/(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs),rhs)
  def infix_/(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_divide(lhs, unit(rhs))  
  def infix_/(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_divide(lhs, rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_divide(repIntToRepFloat(lhs), rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_divide(repIntToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = float_divide(lhs,repIntToRepFloat(rhs))  
  def infix_/(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_divide(lhs, rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_divide(repFloatToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_divide(lhs,repIntToRepDouble(rhs))  
  def infix_/(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = double_divide(lhs,repFloatToRepDouble(rhs))
  def infix_/(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_divide(lhs,rhs)      

  /**
   *  Double
   */
  implicit def doubleToDoubleOps(n: Double): DoubleOpsCls = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]): DoubleOpsCls = new DoubleOpsCls(n)
  implicit def varDoubleToDoubleOps(n: Var[Double]): DoubleOpsCls = new DoubleOpsCls(readVar(n))
  
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

  def obj_double_parse_double(s: Rep[String])(implicit pos: SourceContext): Rep[Double]
  def obj_double_positive_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_negative_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_min_value(implicit pos: SourceContext): Rep[Double]
  def obj_double_max_value(implicit pos: SourceContext): Rep[Double]
  def double_float_value(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float]
  def double_plus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_minus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_times(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_divide(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]  
  def double_to_int(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Int]
  def double_to_float(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float]
  
  /**
   * Float
   */
  object Float {
    def parseFloat(s: Rep[String])(implicit pos: SourceContext) = obj_float_parse_float(s)
  }

  def infix_toInt(lhs: Rep[Float])(implicit o: Overloaded1, pos: SourceContext): Rep[Int] = float_to_int(lhs)
  def infix_toDouble(lhs: Rep[Float])(implicit o: Overloaded1, pos: SourceContext): Rep[Double] = float_to_double(lhs) 
  
  def obj_float_parse_float(s: Rep[String])(implicit pos: SourceContext): Rep[Float]
  def float_plus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_minus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_times(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_divide(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]         
  def float_to_int(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Int]
  def float_to_double(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Double]
  
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

  implicit def intToIntOps(n: Int): IntOpsCls = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]): IntOpsCls = new IntOpsCls(n)
  implicit def varIntToIntOps(n: Var[Int]): IntOpsCls = new IntOpsCls(readVar(n))
    
  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash        
    //def /[A](rhs: Rep[A])(implicit mA: Manifest[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
    // TODO Something is wrong if we just use floatValue. implicits get confused
    def floatValueL()(implicit pos: SourceContext) = int_float_value(lhs)
    def doubleValue()(implicit pos: SourceContext) = int_double_value(lhs)
    def unary_~()(implicit pos: SourceContext) = int_bitwise_not(lhs)
    def toLong(implicit pos: SourceContext) = int_tolong(lhs)
    def toDouble(implicit pos: SourceContext) = int_to_double(lhs)
    def toFloat(implicit pos: SourceContext) = int_to_float(lhs)        
  }

  
  def infix_%(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_mod(lhs, rhs)
  def infix_&(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_binaryor(lhs, rhs)
  def infix_^(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_binaryxor(lhs, rhs)
  def infix_<<(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_leftshift(lhs, rhs)
  def infix_>>(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_rightshiftarith(lhs, rhs)
  def infix_>>>(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_rightshiftlogical(lhs, rhs)

  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
  def obj_int_max_value(implicit pos: SourceContext): Rep[Int]
  def obj_int_min_value(implicit pos: SourceContext): Rep[Int]
  def int_plus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_minus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_times(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  // def int_divide_frac[A:Manifest:Fractional](lhs: Rep[Int], rhs: Rep[A])(implicit pos: SourceContext): Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  
  def int_mod(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_float_value(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Float]
  def int_double_value(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Double]
  def int_bitwise_not(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Int]
  def int_tolong(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Long]
  def int_to_float(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Float]
  def int_to_double(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Double]
  def int_leftshift(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftarith(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftlogical(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]

  /**
   * Long
   */
  object Long {
    def parseLong(s: Rep[String])(implicit pos: SourceContext) = obj_long_parse_long(s)
  }

  def infix_%(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_mod(lhs, rhs)
  def infix_&(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_binaryor(lhs, rhs)
  def infix_<<(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext) = long_shiftleft(lhs, rhs)
  def infix_>>>(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext) = long_shiftright_unsigned(lhs, rhs)
  def infix_toInt(lhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_toint(lhs)
    
  def obj_long_parse_long(s: Rep[String])(implicit pos: SourceContext): Rep[Long]
  def long_mod(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryand(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_shiftleft(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_toint(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Int]
}

trait PrimitiveOpsExp extends PrimitiveOps with EffectExp {
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
  case class DoubleToInt(lhs: Exp[Double]) extends Def[Int]
  case class DoubleToFloat(lhs: Exp[Double]) extends Def[Float]
  case class DoublePlus(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleMinus(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleTimes(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleDivide(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]  

  def obj_double_parse_double(s: Exp[String])(implicit pos: SourceContext) = ObjDoubleParseDouble(s)
  def obj_double_positive_infinity(implicit pos: SourceContext) = ObjDoublePositiveInfinity()
  def obj_double_negative_infinity(implicit pos: SourceContext) = ObjDoubleNegativeInfinity()
  def obj_double_min_value(implicit pos: SourceContext) = ObjDoubleMinValue()
  def obj_double_max_value(implicit pos: SourceContext) = ObjDoubleMaxValue()
  def double_float_value(lhs: Exp[Double])(implicit pos: SourceContext) = DoubleFloatValue(lhs)
  def double_to_int(lhs: Exp[Double])(implicit pos: SourceContext) = DoubleToInt(lhs)
  def double_to_float(lhs: Exp[Double])(implicit pos: SourceContext) = DoubleToFloat(lhs)
  def double_plus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoublePlus(lhs,rhs)
  def double_minus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoubleMinus(lhs,rhs)
  def double_times(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoubleTimes(lhs,rhs)
  def double_divide(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoubleDivide(lhs,rhs)

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
  
  def obj_float_parse_float(s: Exp[String])(implicit pos: SourceContext) = ObjFloatParseFloat(s)
  def float_to_int(lhs: Exp[Float])(implicit pos: SourceContext) = FloatToInt(lhs)
  def float_to_double(lhs: Exp[Float])(implicit pos: SourceContext) = FloatToDouble(lhs)  
  def float_plus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatPlus(lhs,rhs)
  def float_minus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatMinus(lhs,rhs)
  def float_times(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatTimes(lhs,rhs)
  def float_divide(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatDivide(lhs,rhs)
   
  /**
   * Int
   */
  case class ObjIntegerParseInt(s: Exp[String]) extends Def[Int]
  case class ObjIntMaxValue() extends Def[Int]
  case class ObjIntMinValue() extends Def[Int]
  case class IntPlus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMinus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntTimes(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  // case class IntDivideFrac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]
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
  case class IntToFloat(lhs: Exp[Int]) extends Def[Float]
  case class IntToDouble(lhs: Exp[Int]) extends Def[Double]

  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext) = ObjIntegerParseInt(s)
  def obj_int_max_value(implicit pos: SourceContext) = ObjIntMaxValue()
  def obj_int_min_value(implicit pos: SourceContext) = ObjIntMinValue()
  def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs, rhs) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => IntPlus(lhs,rhs)
  }
  def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs, rhs) match {
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => IntMinus(lhs, rhs)
  }
  def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs, rhs) match {
    case (l@Const(0), r) => l
    case (l, r@Const(0)) => r
    case (Const(1), r) => r
    case (l, Const(1)) => l
    case (Const(x), Const(y)) => Const(x*y)
    case _ => IntTimes(lhs, rhs)
  }
  // def int_divide_frac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A])(implicit pos: SourceContext) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = IntDivide(lhs, rhs)
  def int_mod(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntMod(lhs, rhs)
  def int_binaryor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryOr(lhs, rhs)
  def int_binaryand(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryAnd(lhs, rhs)
  def int_binaryxor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryXor(lhs, rhs)
  def int_double_value(lhs: Exp[Int])(implicit pos: SourceContext) = IntDoubleValue(lhs)
  def int_float_value(lhs: Exp[Int])(implicit pos: SourceContext) = IntFloatValue(lhs)
  def int_bitwise_not(lhs: Exp[Int])(implicit pos: SourceContext) = IntBitwiseNot(lhs)
  def int_tolong(lhs: Exp[Int])(implicit pos: SourceContext) = IntToLong(lhs)
  def int_to_float(lhs: Exp[Int])(implicit pos: SourceContext) = IntToFloat(lhs)
  def int_to_double(lhs: Exp[Int])(implicit pos: SourceContext) = IntToDouble(lhs)
  def int_leftshift(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftLeft(lhs, rhs)
  def int_rightshiftarith(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftRightArith(lhs, rhs)
  def int_rightshiftlogical(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftRightLogical(lhs, rhs)


  /**
   * Long
   */
  case class ObjLongParseLong(s: Exp[String]) extends Def[Long]
  case class LongBinaryOr(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongShiftLeft(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightUnsigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongToInt(lhs: Exp[Long]) extends Def[Int]
  case class LongMod(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]

  def obj_long_parse_long(s: Exp[String])(implicit pos: SourceContext) = ObjLongParseLong(s)
  def long_binaryor(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = LongBinaryOr(lhs,rhs)
  def long_binaryand(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = LongBinaryAnd(lhs,rhs)  
  def long_shiftleft(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = LongShiftLeft(lhs,rhs)
  def long_shiftright_unsigned(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = LongShiftRightUnsigned(lhs,rhs)
  def long_toint(lhs: Exp[Long])(implicit pos: SourceContext) = LongToInt(lhs)
  def long_mod(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = LongMod(lhs, rhs)
    
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = ({
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    e match {
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
      case LongMod(x,y) => long_mod(f(x),f(y))
      case LongShiftLeft(x,y) => long_shiftleft(f(x),f(y))
      case LongBinaryOr(x,y) => long_binaryor(f(x),f(y))
      case LongBinaryAnd(x,y) => long_binaryand(f(x),f(y))
      case LongToInt(x) => long_toint(f(x))
      case LongShiftRightUnsigned(x,y) => long_shiftright_unsigned(f(x),f(y))

      case Reflect(ObjDoubleParseDouble(x), u, es) => reflectMirrored(Reflect(ObjDoubleParseDouble(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjDoublePositiveInfinity(), u, es) => reflectMirrored(Reflect(ObjDoublePositiveInfinity(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjDoubleNegativeInfinity(), u, es) => reflectMirrored(Reflect(ObjDoubleNegativeInfinity(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjDoubleMinValue(), u, es) => reflectMirrored(Reflect(ObjDoubleMinValue(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjDoubleMaxValue(), u, es) => reflectMirrored(Reflect(ObjDoubleMaxValue(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(DoubleFloatValue(x), u, es) => reflectMirrored(Reflect(DoubleFloatValue(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(DoubleToInt(x), u, es) => reflectMirrored(Reflect(DoubleToInt(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(DoubleToFloat(x), u, es) => reflectMirrored(Reflect(DoubleToFloat(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(DoublePlus(x,y), u, es) => reflectMirrored(Reflect(DoublePlus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(DoubleMinus(x,y), u, es) => reflectMirrored(Reflect(DoubleMinus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(DoubleTimes(x,y), u, es) => reflectMirrored(Reflect(DoubleTimes(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(DoubleDivide(x,y), u, es) => reflectMirrored(Reflect(DoubleDivide(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(FloatToInt(x), u, es) => reflectMirrored(Reflect(FloatToInt(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(FloatToDouble(x), u, es) => reflectMirrored(Reflect(FloatToDouble(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(FloatPlus(x,y), u, es) => reflectMirrored(Reflect(FloatPlus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(FloatMinus(x,y), u, es) => reflectMirrored(Reflect(FloatMinus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(FloatTimes(x,y), u, es) => reflectMirrored(Reflect(FloatTimes(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(FloatDivide(x,y), u, es) => reflectMirrored(Reflect(FloatDivide(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjIntegerParseInt(x), u, es) => reflectMirrored(Reflect(ObjIntegerParseInt(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjIntMinValue(), u, es) => reflectMirrored(Reflect(ObjIntMinValue(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjIntMaxValue(), u, es) => reflectMirrored(Reflect(ObjIntMaxValue(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntDoubleValue(x), u, es) => reflectMirrored(Reflect(IntDoubleValue(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntFloatValue(x), u, es) => reflectMirrored(Reflect(IntFloatValue(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntBitwiseNot(x), u, es) => reflectMirrored(Reflect(IntBitwiseNot(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntPlus(x,y), u, es) => reflectMirrored(Reflect(IntPlus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntMinus(x,y), u, es) => reflectMirrored(Reflect(IntMinus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntTimes(x,y), u, es) => reflectMirrored(Reflect(IntTimes(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntDivide(x,y), u, es) => reflectMirrored(Reflect(IntDivide(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntMod(x,y), u, es) => reflectMirrored(Reflect(IntMod(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntBinaryOr(x,y), u, es) => reflectMirrored(Reflect(IntBinaryOr(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntBinaryAnd(x,y), u, es) => reflectMirrored(Reflect(IntBinaryAnd(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntBinaryXor(x,y), u, es) => reflectMirrored(Reflect(IntBinaryXor(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntToLong(x), u, es) => reflectMirrored(Reflect(IntToLong(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntToFloat(x), u, es) => reflectMirrored(Reflect(IntToFloat(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntToDouble(x), u, es) => reflectMirrored(Reflect(IntToDouble(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)    
      case Reflect(IntShiftLeft(x,y), u, es) => reflectMirrored(Reflect(IntShiftLeft(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntShiftRightLogical(x,y), u, es) => reflectMirrored(Reflect(IntShiftRightLogical(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(IntShiftRightArith(x,y), u, es) => reflectMirrored(Reflect(IntShiftRightArith(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)    
      case Reflect(LongMod(x,y), u, es) => reflectMirrored(Reflect(LongMod(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(LongShiftLeft(x,y), u, es) => reflectMirrored(Reflect(LongShiftLeft(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(LongShiftRightUnsigned(x,y), u, es) => reflectMirrored(Reflect(LongShiftRightUnsigned(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(LongBinaryOr(x,y), u, es) => reflectMirrored(Reflect(LongBinaryOr(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(LongBinaryAnd(x,y), u, es) => reflectMirrored(Reflect(LongBinaryAnd(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)    
      case Reflect(LongToInt(x), u, es) => reflectMirrored(Reflect(LongToInt(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]
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
    case (Def(IntPlus(llhs,lrhs)), rhs) if lrhs.equals(rhs) => llhs
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
    case LongMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ObjDoubleParseDouble(s) => emitValDef(sym, "strtod(" + quote(s) + ",NULL)")
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
      case ObjIntegerParseInt(s) => emitValDef(sym, "atoi(" + quote(s) + ".c_str())")
      case ObjIntMaxValue() => emitValDef(sym, "INT_MAX")
      case ObjIntMinValue() => emitValDef(sym, "INT_MIN")    
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
      case ObjLongParseLong(s) => emitValDef(sym, "strtod(" + quote(s) + ".c_str(),NULL)")
      case LongMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
      case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
      case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))    
      case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
      case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, "(uint64_t)" + quote(lhs) + " >> " + quote(rhs))    
      case LongToInt(lhs) => emitValDef(sym, "(int32_t)"+quote(lhs))
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ObjDoublePositiveInfinity() => emitValDef(sym, "__longlong_as_double(0x7ff0000000000000ULL)")
      case ObjDoubleNegativeInfinity() => emitValDef(sym, "__longlong_as_double(0xfff0000000000000ULL)")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait OpenCLGenPrimitiveOps extends OpenCLGenBase with CLikeGenPrimitiveOps

trait CGenPrimitiveOps extends CGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ObjDoublePositiveInfinity() => emitValDef(sym, "INFINITY")
      case ObjDoubleNegativeInfinity() => emitValDef(sym, "-INFINITY")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

