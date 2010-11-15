package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenEffect, ScalaGenEffect}

trait Functions extends Base {

  implicit def doLambda[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B]): Rep[A => B]

  implicit def toLambdaOps[A:Manifest,B:Manifest](fun: Rep[A => B]) = new LambdaOps(fun)
  
  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = doApply(f,x)
  }
  def doApply[A:Manifest,B:Manifest](fun: Rep[A => B], arg: Rep[A]): Rep[B]

}

trait FunctionsExp extends Functions with EffectExp {

  case class Lambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], x: Sym[A], y: Exp[B]) extends Def[A => B]
  case class Apply[A:Manifest,B:Manifest](f: Exp[A => B], arg: Exp[A]) extends Def[B]

  def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]) : Exp[A => B] = {

    val x = fresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site. 
                               // TODO: this will not work if f is recursive. 
                               // need to incorporate the other pieces at some point.
    Lambda(f, x, y)
  }

  def doApply[A:Manifest,B:Manifest](f: Exp[A => B], x: Exp[A]): Exp[B] = f match {

    case Def(Lambda(_,_,Def(Reify(_,_)))) => 
      // if function result is known to be effectful, so is application
      reflectEffect(Apply(f,x))
    case Def(Lambda(_,_,_)) => 
      // if function result is known to be pure, so is application
      Apply(f, x)
    case _ => // unknown function, assume it is effectful
      reflectEffect(Apply(f, x))
  }
}

trait ScalaGenFunctions extends ScalaGenEffect {
  val IR: FunctionsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) if shallow => Nil // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println("val " + quote(sym) + " = {" + quote(x) + ": (" + x.Type + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenFunctions extends CudaGenEffect {
  val IR: FunctionsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) if shallow => Nil // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case e@Lambda(fun, x, y) =>

      // The version for inlined device function
      stream.println("%s %s = %s;".format(x.Type.toString, quote(x), quote(sym)+"_1"))
      emitBlock(y)
      stream.println("%s %s = %s;".format(y.Type.toString, quote(sym), quote(getBlockResult(y))))

      // The version for separate device function
      /*
      //TODO: If function parameter was originally tuple, then each element should be renamed?
      val freeVars = buildScheduleForResult(y).filter(scope.contains(_)).map(_.sym)
      stream.println("__device__ %s %s(%s %s) {".format(e.mB, quote(sym), e.mA, quote(x)))
      emitBlock(y)
      stream.println("%s %s = %s;".format(e.mB, quote(sym), quote(getBlockResult(y))))
      stream.println("return %s;".format(quote(getBlockResult(y))))
      stream.println("}")
      */

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}