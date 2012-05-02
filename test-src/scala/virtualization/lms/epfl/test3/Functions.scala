package scala.virtualization.lms
package epfl
package test3

import common._
import test1._
import test2._

import scala.reflect.SourceContext



trait FunctionsExp extends Functions with BaseExp { // shadow trait with same name in core package
    
  // FIXME: there might be a conflict since this pulls in internal.Effects which is different from test1.Effects
  case class Lambda[A:Manifest,B:Manifest](fun: Exp[A] => Exp[B]) extends Def[A => B]
  case class Lambda2[A1:Manifest,A2:Manifest,B:Manifest](fun: (Exp[A1],Exp[A2]) => Exp[B]) extends Def[(A1,A2) => B]
  case class Apply[A:Manifest,B:Manifest](fun: Exp[A => B], arg: Exp[A]) extends Def[B]
  
  def doLambda[A:Manifest,B:Manifest](fun: Exp[A] => Exp[B])(implicit ctx: SourceContext): Exp[A => B] = Lambda(fun)
  def doLambda2[A1:Manifest,A2:Manifest,B:Manifest](fun: (Exp[A1],Exp[A2]) => Exp[B])(implicit ctx: SourceContext) : Exp[(A1,A2) => B] = Lambda2(fun)
  def doApply[A:Manifest,B:Manifest](fun: Exp[A => B], arg: Exp[A])(implicit ctx: SourceContext): Exp[B] = Apply(fun, arg)
}

trait FunctionsExpUnfoldAll extends FunctionsExp {

  override def doApply[A:Manifest,B:Manifest](fun: Exp[A => B], arg: Exp[A])(implicit ctx: SourceContext): Exp[B] = fun match {
    case Def(Lambda(fun)) => fun(arg)
    case _ => super.doApply(fun, arg)
  }

}


