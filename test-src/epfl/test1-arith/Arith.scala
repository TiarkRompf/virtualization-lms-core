package scala.lms
package epfl
package test1

import common._

import scala.reflect.SourceContext
import java.io.PrintWriter

trait LiftArith {
  this: Arith =>

  implicit def numericToRep[T:Numeric:Typ](x: T) = unit(x)
}

trait Arith extends Base with LiftArith {
  //todo removed this, I can see now that having these implicits replicated everywhere can force us to control the
  //types that are allowed to be lifted more explicitly
  //implicit def unit(x: Double): Rep[Double]

  implicit def intTyp: Typ[Int]
  implicit def doubleTyp: Typ[Double] 

  // aks: this is a workaround for the infix methods not intercepting after Typs were added everywhere
  implicit def intToArithOps(i: Int) = new arithOps(unit(i))
  implicit def intToRepDbl(i: Int) : Rep[Double] = unit(i)

  class arithOps(x: Rep[Double]){
    def +(y: Rep[Double]) = infix_+(x,y)
    def -(y: Rep[Double]) = infix_-(x,y)
    def *(y: Rep[Double]) = infix_*(x,y)
    def /(y: Rep[Double]) = infix_/(x,y)
  }

  def infix_+(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def infix_-(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def infix_*(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def infix_/(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext): Rep[Double]
}


trait ArithExp extends Arith with BaseExp {
  //todo removed below as now handled in Base traits
  //implicit def unit(x: Double) = Const(x)

  implicit def intTyp: Typ[Int] = ManifestTyp(implicitly)
  implicit def doubleTyp: Typ[Double] = ManifestTyp(implicitly)
  
  case class Plus(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Minus(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Times(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Div(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  def infix_+(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = Plus(x, y)
  def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = Minus(x, y)
  def infix_*(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = Times(x, y)
  def infix_/(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = Div(x, y)
  
  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Plus(x,y) => f(x) + f(y)
    case Minus(x,y) => f(x) - f(y)
    case Times(x,y) => f(x) * f(y)
    case Div(x,y) => f(x) / f(y)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ArithExpOpt extends ArithExp {

  override def infix_+(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x + y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case (Const(0.0) | Const(-0.0), y) => y
    case _ => super.infix_+(x, y)
  }

  override def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x - y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case _ => super.infix_-(x, y)
  }

  override def infix_*(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case (x, Const(1.0)) => x
    case (Const(1.0), y) => y
    case (x, Const(0.0) | Const(-0.0)) => Const(0.0)
    case (Const(0.0) | Const(-0.0), y) => Const(0.0)
    case _ => super.infix_*(x, y)
  }

  override def infix_/(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x / y)
    case (x, Const(1.0)) => x
    case _ => super.infix_/(x, y)
  }

}



trait ScalaGenArith extends ScalaGenBase {
  val IR: ArithExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
