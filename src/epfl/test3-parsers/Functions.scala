package scala.virtualization.lms
package epfl
package test3

import common._
import test1._
import test2._




trait FunctionsExp extends Functions with BaseExp { // shadow trait with same name in core package
    
  // FIXME: there might be a conflict since this pull ins internal.Effects which is different from test1.Effects
    
  case class Lambda[A,B](fun: Exp[A] => Exp[B]) extends Def[A => B]
  case class Apply[A,B](fun: Exp[A => B], arg: Exp[A]) extends Def[B]
  
  def doLambda[A,B](fun: Exp[A] => Exp[B])(implicit mA: Manifest[A]): Exp[A => B] = Lambda(fun)
  def doApply[A,B](fun: Exp[A => B], arg: Exp[A]): Exp[B] = Apply(fun, arg)
}

trait FunctionsExpUnfoldAll extends FunctionsExp {

  override def doApply[A,B](fun: Exp[A => B], arg: Exp[A]): Exp[B] = fun match {
    case Def(Lambda(fun)) => fun(arg)
    case _ => super.doApply(fun, arg)
  }

}


