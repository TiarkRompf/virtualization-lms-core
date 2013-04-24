package scala.lms
package ops

import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftNumeric {
  this: Base =>

  implicit def numericToNumericRep[T:Numeric:Manifest](x: T) = unit(x)
}

trait NumericOps extends Variables {
  this: PrimitiveOps =>

  // workaround for infix not working with manifests
  implicit def numericToNumericOps[T:Numeric:Manifest](n: T) = new NumericOpsCls(unit(n))
  implicit def repNumericToNumericOps[T:Numeric:Manifest](n: Rep[T]) = new NumericOpsCls(n)
  implicit def varNumericToNumericOps[T:Numeric:Manifest](n: Var[T]) = new NumericOpsCls(readVar(n))

  class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T]){
    def +[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,unit(c(rhs)))
    def +(rhs: Rep[T])(implicit pos: SourceContext) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T])(implicit pos: SourceContext) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T])(implicit pos: SourceContext) = numeric_times(lhs,rhs)
    def /(rhs: Rep[T])(implicit pos: SourceContext) = numeric_divide(lhs,rhs)
  }

  /**
   * Special case primitives by enumerating all combinations. 
   * Avoids certain fragile behavior, including compiler crashes and some erroneous or inaccessible type errors.
   */  
  def infix_-(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = numeric_minus[Int](unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_minus[Float](unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_minus[Double](unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = numeric_minus[Float](unit(lhs),rhs)  
  def infix_-(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_minus[Float](unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_minus[Double](unit(lhs), rhs)
  def infix_-(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = numeric_minus[Double](unit(lhs),rhs)  
  def infix_-(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = numeric_minus[Double](unit(lhs),rhs)
  def infix_-(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_minus[Double](unit(lhs),rhs)
  def infix_-(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = numeric_minus[Int](lhs, unit(rhs))  
  def infix_-(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = numeric_minus[Float](lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_minus[Float](lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = numeric_minus[Float](lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs, unit(rhs))
  // def infix_-(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = numeric_minus[Int](lhs, rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = numeric_minus[Float](repIntToRepFloat(lhs), rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = numeric_minus[Double](repIntToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = numeric_minus[Float](lhs,repIntToRepFloat(rhs))  
  def infix_-(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = numeric_minus[Float](lhs, rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = numeric_minus[Double](repFloatToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs,repIntToRepDouble(rhs))  
  def infix_-(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs,repFloatToRepDouble(rhs))
  def infix_-(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = numeric_minus[Double](lhs,rhs)    

  def infix_+(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = numeric_plus[Int](unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_plus[Float](unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_plus[Double](unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = numeric_plus[Float](unit(lhs),rhs)  
  def infix_+(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_plus[Float](unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_plus[Double](unit(lhs), rhs)
  def infix_+(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = numeric_plus[Double](unit(lhs),rhs)  
  def infix_+(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = numeric_plus[Double](unit(lhs),rhs)
  def infix_+(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_plus[Double](unit(lhs),rhs)
  def infix_+(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = numeric_plus[Int](lhs, unit(rhs))  
  def infix_+(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = numeric_plus[Float](lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_plus[Float](lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = numeric_plus[Float](lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs, unit(rhs))
  // def infix_+(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded15, ctx: SourceContext): Rep[Int] = numeric_plus[Int](lhs, rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded16, ctx: SourceContext): Rep[Float] = numeric_plus[Float](repIntToRepFloat(lhs), rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded17, ctx: SourceContext): Rep[Double] = numeric_plus[Double](repIntToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded18, ctx: SourceContext): Rep[Float] = numeric_plus[Float](lhs,repIntToRepFloat(rhs))  
  def infix_+(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded19, ctx: SourceContext): Rep[Float] = numeric_plus[Float](lhs, rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded20, ctx: SourceContext): Rep[Double] = numeric_plus[Double](repFloatToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded21, ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs,repIntToRepDouble(rhs))  
  def infix_+(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded22, ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs,repFloatToRepDouble(rhs))
  def infix_+(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded23, ctx: SourceContext): Rep[Double] = numeric_plus[Double](lhs,rhs)    

  def infix_*(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = numeric_times[Int](unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_times[Float](unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_times[Double](unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = numeric_times[Float](unit(lhs),rhs)  
  def infix_*(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_times[Float](unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_times[Double](unit(lhs), rhs)
  def infix_*(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = numeric_times[Double](unit(lhs),rhs)  
  def infix_*(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = numeric_times[Double](unit(lhs),rhs)
  def infix_*(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_times[Double](unit(lhs),rhs)
  def infix_*(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = numeric_times[Int](lhs, unit(rhs))  
  def infix_*(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = numeric_times[Float](lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_times[Float](lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = numeric_times[Float](lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs, unit(rhs))
  // def infix_*(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = numeric_times[Int](lhs, rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = numeric_times[Float](repIntToRepFloat(lhs), rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = numeric_times[Double](repIntToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = numeric_times[Float](lhs,repIntToRepFloat(rhs))  
  def infix_*(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = numeric_times[Float](lhs, rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = numeric_times[Double](repFloatToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs,repIntToRepDouble(rhs))  
  def infix_*(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs,repFloatToRepDouble(rhs))
  def infix_*(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = numeric_times[Double](lhs,rhs)    

  def infix_/(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = numeric_divide[Int](unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_divide[Float](unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_divide[Double](unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = numeric_divide[Float](unit(lhs),rhs)  
  def infix_/(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_divide[Float](unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_divide[Double](unit(lhs), rhs)
  def infix_/(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = numeric_divide[Double](unit(lhs),rhs)  
  def infix_/(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = numeric_divide[Double](unit(lhs),rhs)
  def infix_/(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = numeric_divide[Double](unit(lhs),rhs)
  def infix_/(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = numeric_divide[Int](lhs, unit(rhs))  
  def infix_/(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = numeric_divide[Float](lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = numeric_divide[Float](lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = numeric_divide[Float](lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs, unit(rhs))
  // def infix_/(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = numeric_divide[Int](lhs, rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = numeric_divide[Float](repIntToRepFloat(lhs), rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = numeric_divide[Double](repIntToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = numeric_divide[Float](lhs,repIntToRepFloat(rhs))  
  def infix_/(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = numeric_divide[Float](lhs, rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = numeric_divide[Double](repFloatToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs,repIntToRepDouble(rhs))  
  def infix_/(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs,repFloatToRepDouble(rhs))
  def infix_/(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = numeric_divide[Double](lhs,rhs)    

  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_divide[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait NumericOpsExp extends NumericOps with VariablesExp with BaseFatExp {
  this: PrimitiveOpsExp =>
  
  abstract class DefMN[A:Manifest:Numeric] extends Def[A] {
    def mev = manifest[A]
    def aev = implicitly[Numeric[A]]
  }

  case class NumericPlus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericMinus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericTimes[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericDivide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]

  def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericTimes(lhs, rhs)
  def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericDivide(lhs, rhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@NumericPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}


trait NumericOpsExpOpt extends NumericOpsExp {
  this: PrimitiveOpsExp =>
  
  override def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].plus(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => x
    case _ => super.numeric_plus(lhs,rhs)
  }
  override def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].minus(x,y))
    case _ => super.numeric_minus(lhs,rhs)
  }
  override def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].times(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => Const(x)
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => Const(y)
    case (Const(x), y) if x == implicitly[Numeric[T]].one => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].one => x
    case _ => super.numeric_times(lhs,rhs)
  }
  override def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    // CAVEAT: Numeric doesn't have .div, Fractional has
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].asInstanceOf[Fractional[T]].div(x,y))
    case _ => super.numeric_divide(lhs,rhs)
  }
}


trait ScalaGenNumericOps extends ScalaGenFat {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case NumericDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenNumericOps extends CLikeGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case NumericPlus(a,b) =>
          emitValDef(sym, quote(a) + " + " + quote(b))
        case NumericMinus(a,b) =>
          emitValDef(sym, quote(a) + " - " + quote(b))
        case NumericTimes(a,b) =>
          emitValDef(sym, quote(a) + " * " + quote(b))
        case NumericDivide(a,b) =>
          emitValDef(sym, quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenNumericOps extends CudaGenBase with CLikeGenNumericOps
trait OpenCLGenNumericOps extends OpenCLGenBase with CLikeGenNumericOps
trait CGenNumericOps extends CGenBase with CLikeGenNumericOps

