package scala.virtualization.lms
package epfl
package test1

import common._

import reflect.SourceContext

import java.io.PrintWriter

trait LiftArith {
  this: Arith =>

  implicit def numericToRep[T:Numeric:Manifest](x: T) = unit(x)
}

trait Arith extends Base with LiftArith {
  //todo removed this, I can see now that having these implicits replicated everywhere can force us to control the
  //types that are allowed to be lifted more explicitly
  //implicit def unit(x: Double): Rep[Double]

  // aks: this is a workaround for the infix methods not intercepting after Manifests were added everywhere
  implicit def intToArithOps(i: Int) = new arithOps(unit(i))
  implicit def intToRepDbl(i: Int) : Rep[Double] = unit(i)

  class arithOps(x: Rep[Double]){
    def +(y: Rep[Double]) = infix_+(x,y)
    def -(y: Rep[Double]) = infix_-(x,y)
    def *(y: Rep[Double]) = infix_*(x,y)
    def /(y: Rep[Double]) = infix_/(x,y)
  }

  def infix_+(x: Rep[Double], y: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def infix_-(x: Rep[Double], y: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def infix_*(x: Rep[Double], y: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def infix_/(x: Rep[Double], y: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
}


trait ArithExp extends Arith with BaseExp {
  //todo removed below as now handled in Base traits
  //implicit def unit(x: Double) = Const(x)
  
  case class Plus(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) extends Def[Double](Some(ctx))
  case class Minus(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) extends Def[Double](Some(ctx))
  case class Times(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) extends Def[Double](Some(ctx))
  case class Div(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) extends Def[Double](Some(ctx))

  def infix_+(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = Plus(x, y)
  def infix_-(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = Minus(x, y)
  def infix_*(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = Times(x, y)
  def infix_/(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = Div(x, y)
}

trait ArithExpOpt extends ArithExp {

  override def infix_+(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x + y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case (Const(0.0) | Const(-0.0), y) => y
    case _ => super.infix_+(x, y)
  }

  override def infix_-(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x - y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case _ => super.infix_-(x, y)
  }

  override def infix_*(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case (x, Const(1.0)) => x
    case (Const(1.0), y) => y
    case (x, Const(0.0) | Const(-0.0)) => Const(0.0)
    case (Const(0.0) | Const(-0.0), y) => Const(0.0)
    case _ => super.infix_*(x, y)
  }

  override def infix_/(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = (x, y) match {
    case (Const(x), Const(y)) => Const(x / y)
    case (x, Const(1.0)) => x
    case _ => super.infix_/(x, y)
  }

}



trait ScalaGenArith extends ScalaGenBase {
  val IR: ArithExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
