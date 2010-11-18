package scala.virtualization.lms
package common

import java.io.PrintWriter

trait Functions extends Base {

  implicit def doLambda[A,B](fun: Rep[A] => Rep[B])(implicit mA: Manifest[A]): Rep[A => B]
  class LambdaOps[A,B](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = doApply(f,x)
  }
  def doApply[A,B](fun: Rep[A => B], arg: Rep[A]): Rep[B]

}

trait FunctionsExp extends Functions with EffectExp {

  case class Lambda[A,B](f: Exp[A] => Exp[B], x: Sym[A], y: Exp[B])(implicit val mA: Manifest[A]) extends Def[A => B]
  case class Apply[A,B](f: Exp[A => B], arg: Exp[A]) extends Def[B]

  def doLambda[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A]) : Exp[A => B] = {
    val x = fresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site. 
                               // TODO: this will not work if f is recursive. 
                               // need to incorporate the other pieces at some point.
    Lambda(f, x, y)
  }
  
  def doApply[A,B](f: Exp[A => B], x: Exp[A]): Exp[B] = f match {
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

/*  
  // TODO: right now were trying to hoist as much as we can out of functions. that might not always be appropriate!

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) if shallow => Nil // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }
*/

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match { // treat effects as bound symbols, just like the fun param
    case Lambda(f, x, Def(Reify(y, es))) => x :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y) // i.e. they must live inside
    case Lambda(f, x, y) => x :: boundSyms(y)
    case _ => super.boundSyms(e)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println("val " + quote(sym) + " = {" + quote(x) + ": (" + e.mA + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case Apply(fun, arg) => 
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}