package scala.lms
package epfl
package test1

import common._

trait Trig extends Base {

  //todo removed
  //implicit def unit(x: Double): Rep[Double]

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
    case Const(x) => unit(math.sin(x))
    case _ => super.sin(x)
  }
  
  override def cos(x: Exp[Double]) = x match {
    case Const(x) => unit(math.cos(x))
    case _ => super.cos(x)
  }

}