package test2

import test1._

import java.io.PrintWriter

trait Arith extends Base {
  implicit def unit(x: Double): Rep[Double]

  def __ext__+(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def __ext__-(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def __ext__*(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def __ext__/(x: Rep[Double], y: Rep[Double]): Rep[Double]
}


trait ArithExp extends Arith with BaseExp {
  
  implicit def unit(x: Double) = Const(x)
  
  case class Plus(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Minus(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Times(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Div(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  def __ext__+(x: Exp[Double], y: Exp[Double]) = Plus(x, y)
  def __ext__-(x: Exp[Double], y: Exp[Double]) = Minus(x, y)
  def __ext__*(x: Exp[Double], y: Exp[Double]) = Times(x, y)
  def __ext__/(x: Exp[Double], y: Exp[Double]) = Div(x, y)
}

trait ArithExpOpt extends ArithExp {

  override def __ext__+(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x + y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case (Const(0.0) | Const(-0.0), y) => y
    case _ => super.__ext__+(x, y)
  }

  override def __ext__-(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x - y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case _ => super.__ext__-(x, y)
  }

  override def __ext__*(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case (x, Const(1.0)) => x
    case (Const(1.0), y) => y
    case (x, Const(0.0) | Const(-0.0)) => Const(0.0)
    case (Const(0.0) | Const(-0.0), y) => Const(0.0)
    case _ => super.__ext__*(x, y)
  }

  override def __ext__/(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x / y)
    case (x, Const(1.0)) => x
    case _ => super.__ext__/(x, y)
  }

}


trait ArithExpOpt2 extends ArithExpOpt {

  override def __ext__+(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (x, Def(Minus(Const(0.0) | Const(-0.0), y))) => __ext__-(x, y)
    case _ => super.__ext__+(x, y)
  }

  override def __ext__-(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (x, Def(Minus(Const(0.0) | Const(-0.0), y))) => __ext__+(x, y)
    case _ => super.__ext__-(x, y)
  }

  override def __ext__*(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (x, Const(-1.0)) => __ext__-(0.0, x)
    case (Const(-1.0), y) => __ext__-(0.0, y)
    case _ => super.__ext__*(x, y)
  }
}

trait ScalaCodegenArith extends ScalaCodegen { this: ArithExp =>
  
  override def emitNode(sym: Sym[_], rhs: Def[_], stream: PrintWriter) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b), stream)
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b), stream)
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b), stream)
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b), stream)
    case _ => super.emitNode(sym, rhs, stream)
  }
}






trait Trig extends Base {

  implicit def unit(x: Double): Rep[Double]

  def sin(x: Rep[Double]): Rep[Double]
  def cos(x: Rep[Double]): Rep[Double]

}

trait TrigExp extends Trig with BaseExp {

  case class Sin(x: Exp[Double]) extends Def[Double]
  case class Cos(x: Exp[Double]) extends Def[Double]

  def sin(x: Exp[Double]) = Sin(x)
  def cos(x: Exp[Double]) = Cos(x)
}

trait TrigExpOpt extends TrigExp {

  override def sin(x: Exp[Double]) = x match {
    case Const(x) => math.sin(x)
    case _ => super.sin(x)
  }
  
  override def cos(x: Exp[Double]) = x match {
    case Const(x) => math.cos(x)
    case _ => super.cos(x)
  }

}


trait TrigExpOpt2 extends TrigExpOpt {
  override def cos(x: Exp[Double]) = x match {
    case Const(x) if { val z = x / math.Pi / 0.5; z != 0 && z == z.toInt } => Const(0.0)
    case _ => super.cos(x)
  }
}


trait Arrays extends Base {

  class ArrayOps[T](x: Rep[Array[T]]) {
    def apply(i: Int) = arrayApply(x, i)
  }
  implicit def array2arrayOps[T](x: Rep[Array[T]]) = new ArrayOps(x)

  def arrayApply[T](x: Rep[Array[T]], i:Int): Rep[T]
  //def arrayUpdate(x: Rep[Double]): Rep[Unit]
  def makeArray[T](x: List[Rep[T]]): Rep[Array[T]]
}

trait ArrayExp extends Arrays with BaseExp {
  case class ArrayApply[T](x:Rep[Array[T]], i:Int) extends Def[T]
  //case class ArrayUpdate[T](x:Rep[Array[T]], i:Int) extends Def[T]
  case class MakeArray[T](x:List[Rep[T]]) extends Def[Array[T]]

  def arrayApply[T](x: Rep[Array[T]], i:Int) = ArrayApply(x, i)
  //def arrayUpdate(x: Rep[Double]) = ArrayUpdate(x)
  def makeArray[T](x: List[Rep[T]]) = MakeArray(x)
}

trait ScalaCodegenArrays extends ScalaCodegen { this: ArrayExp =>
  override def emitNode(sym: Sym[_], rhs: Def[_], stream: PrintWriter) = rhs match {
    case ArrayApply(x,i) =>  emitValDef(sym, "" + quote(x) + ".apply(" + i + ")", stream)
    case MakeArray(x) =>  emitValDef(sym, "Array(" + x.map(quote).mkString(",") + ")", stream)
    case _ => super.emitNode(sym, rhs, stream)
  }
}





trait Relat extends Base {

  implicit def unit(x: Double): Rep[Double]

/*
  implicit def doubleRepRelat(x: Rep[Double]) = new {
    def >(y: Rep[Double]) = greater(x,y)
    def <(y: Rep[Double]) = less(x,y)
    def >=(y: Rep[Double]) = greaterEqual(x,y)
    def <=(y: Rep[Double]) = lessEqual(x,y)
    def ==(y: Rep[Double]) = equal(x,y)
    def !=(y: Rep[Double]) = notEqual(x,y)
  }
*/
  
  def min(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def max(x: Rep[Double], y: Rep[Double]): Rep[Double]

/*  
  def greater(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def less(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def greaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def lessEqual(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def equal(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def notEqual(x: Rep[Double], y: Rep[Double]): Rep[Double]
*/
}

trait RelatExp extends Relat with BaseExp {
  
  case class Min(x: Rep[Double], y: Rep[Double]) extends Def[Double]
  case class Max(x: Rep[Double], y: Rep[Double]) extends Def[Double]
  
  def min(x: Rep[Double], y: Rep[Double]) = Min(x,y)
  def max(x: Rep[Double], y: Rep[Double]) = Max(x,y)
}

trait RelatExpOpt extends RelatExp {
  
  override def min(x: Rep[Double], y: Rep[Double]) = (x,y) match {
    case (Const(x), Const(y)) => math.min(x,y)
    case _ => super.min(x,y)
  }
  override def max(x: Rep[Double], y: Rep[Double]) = (x,y) match {
    case (Const(x), Const(y)) => math.max(x,y)
    case _ => super.max(x,y)
  }
}
