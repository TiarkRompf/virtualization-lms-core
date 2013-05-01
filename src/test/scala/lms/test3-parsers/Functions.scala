package scala.lms
package test3

import ops._
import test1._
import test2._

import scala.reflect.SourceContext



trait FunctionsExp extends Functions with BaseExp { // shadow trait with same name in core package

  // FIXME: there might be a conflict since this pulls in internal.Effects which is different from test1.Effects
  case class Lambda[A:TypeRep,B:TypeRep](fun: Exp[A] => Exp[B]) extends Def[A => B]
  case class Lambda2[A1:TypeRep,A2:TypeRep,B:TypeRep](fun: (Exp[A1],Exp[A2]) => Exp[B]) extends Def[(A1,A2) => B]
  case class Apply[A:TypeRep,B:TypeRep](fun: Exp[A => B], arg: Exp[A]) extends Def[B]
  case class Apply2[A1:TypeRep,A2:TypeRep,B:TypeRep](fun: Exp[(A1,A2) => B], arg1: Exp[A1], arg2: Exp[A2]) extends Def[B]

  def doLambda[A:TypeRep,B:TypeRep](fun: Exp[A] => Exp[B])(implicit pos: SourceContext): Exp[A => B] = Lambda(fun)
  def doLambda2[A1:TypeRep,A2:TypeRep,B:TypeRep](fun: (Exp[A1],Exp[A2]) => Exp[B])(implicit pos: SourceContext) : Exp[(A1,A2) => B] = Lambda2(fun)
  def doApply[A:TypeRep,B:TypeRep](fun: Exp[A => B], arg: Exp[A])(implicit pos: SourceContext): Exp[B] = Apply(fun, arg)
  def doApply2[A1:TypeRep,A2:TypeRep,B:TypeRep](fun: Exp[(A1,A2) => B], arg1: Exp[A1], arg2: Exp[A2])(implicit pos: SourceContext): Exp[B] = Apply2(fun, arg1, arg2)
}

trait FunctionsExpUnfoldAll extends FunctionsExp {

  override def doApply[A:TypeRep,B:TypeRep](fun: Exp[A => B], arg: Exp[A])(implicit pos: SourceContext): Exp[B] = fun match {
    case Def(Lambda(fun)) => fun(arg)
    case _ => super.doApply(fun, arg)
  }
  override def doApply2[A1:TypeRep,A2:TypeRep,B:TypeRep](fun: Exp[(A1,A2) => B], arg1: Exp[A1], arg2: Exp[A2])(implicit pos: SourceContext): Exp[B] = fun match {
    case Def(Lambda2(fun)) => fun(arg1, arg2)
    case _ => super.doApply2(fun, arg1, arg2)
  }

}


