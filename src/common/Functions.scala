package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, ScalaGenEffect}

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

trait BaseGenFunctions extends GenericNestedCodegen {
  val IR: FunctionsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) if shallow => Nil // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, Def(Reify(y, es))) => x :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y)
    case Lambda(f, x, y) => x :: boundSyms(y)
    //case Lambda2(f, x1, x2, Def(Reify(y, es))) => x1 :: x2 :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y)
    //case Lambda2(f, x1, x2, y) => x1 :: x2 :: boundSyms(y)
    //case Lambda(f, x, Def(a,lst)) => x :: boundSyms(y)
    case _ => Nil
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case Lambda(f, x, y) => getFreeVarBlock(y,List(x.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }
}

trait ScalaGenFunctions extends ScalaGenEffect with BaseGenFunctions {
  import IR._

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