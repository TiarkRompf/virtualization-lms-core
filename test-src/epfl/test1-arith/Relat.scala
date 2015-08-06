package scala.lms
package epfl
package test1

import common._

trait Relat extends Base {


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

trait RelatExpOpt extends RelatExp with LiftAll {
  
  override def min(x: Rep[Double], y: Rep[Double]) = (x,y) match {
    case (Const(x), Const(y)) => math.min(x,y)
    case _ => super.min(x,y)
  }
  override def max(x: Rep[Double], y: Rep[Double]) = (x,y) match {
    case (Const(x), Const(y)) => math.max(x,y)
    case _ => super.max(x,y)
  }
}
