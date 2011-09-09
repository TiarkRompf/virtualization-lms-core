package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}

trait Functions extends Base {

  implicit def doLambda[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B]): Rep[A => B]
  implicit def doLambda2[A1:Manifest,A2:Manifest,B:Manifest](fun: (Rep[A1],Rep[A2]) => Rep[B]): Rep[(A1,A2) => B]

  implicit def toLambdaOps[A:Manifest,B:Manifest](fun: Rep[A => B]) = new LambdaOps(fun)
  
  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = doApply(f,x)
  }

  def doApply[A:Manifest,B:Manifest](fun: Rep[A => B], arg: Rep[A]): Rep[B]

}

trait FunctionsExp extends Functions with EffectExp {

  case class Lambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], x: Sym[A], y: Exp[B]) extends Def[A => B]
  case class Lambda2[A1:Manifest,A2:Manifest,B:Manifest](f: (Exp[A1],Exp[A2]) => Exp[B], x1: Sym[A1], x2: Sym[A2], y: Exp[B]) extends Def[(A1,A2) => B]

  case class Apply[A:Manifest,B:Manifest](f: Exp[A => B], arg: Exp[A]) extends Def[B]

  def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]) : Exp[A => B] = {

    val x = fresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site. 
                               // TODO: this will not work if f is recursive. 
                               // need to incorporate the other pieces at some point.
    Lambda(f, x, y)
  }

  def doLambda2[A1:Manifest,A2:Manifest,B:Manifest](f: (Exp[A1],Exp[A2]) => Exp[B]) : Exp[(A1,A2) => B] = {

    val x1 = fresh[A1]
    val x2 = fresh[A2]
    val y = reifyEffects(f(x1,x2)) // unfold completely at the definition site.
                               // TODO: this will not work if f is recursive.
                               // need to incorporate the other pieces at some point.
    Lambda2(f, x1, x2, y)
  }

  def doApply[A:Manifest,B:Manifest](f: Exp[A => B], x: Exp[A]): Exp[B] = f match {

    case Def(Lambda(_,_,Def(Reify(_,_,_)))) => 
      // if function result is known to be effectful, so is application
      reflectEffect(Apply(f,x))
    case Def(Lambda(_,_,_)) => 
      // if function result is known to be pure, so is application
      Apply(f, x)
    case _ => // unknown function, assume it is effectful
      reflectEffect(Apply(f, x))
  }
  
  // TODO: right now were trying to hoist as much as we can out of functions. 
  // That might not always be appropriate. A promising strategy would be to have
  // explicit 'hot' and 'cold' functions. 

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case Lambda2(f, x1, x2, y) => syms(y)
    case _ => super.syms(e)
  }

/*
  override def hotSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case Lambda2(f, x1, x2, y) => syms(y)
    case _ => super.hotSyms(e)
  }
*/

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Lambda(f, x, y) => freqHot(y)
    case Lambda2(f, x1, x2, y) => freqHot(y)
    case _ => super.symsFreq(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => x :: effectSyms(y)
    case Lambda2(f, x1, x2, y) => x1 :: x2 :: effectSyms(y)
    case _ => super.boundSyms(e)
  }  
}

trait BaseGenFunctions extends GenericNestedCodegen {
  val IR: FunctionsExp
  import IR._


}

trait ScalaGenFunctions extends ScalaGenEffect with BaseGenFunctions {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println("val " + quote(sym) + " = {" + quote(x) + ": (" + x.Type + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + y.Type)
      stream.println("}")

    case e@Lambda2(fun, x1, x2, y) =>
      stream.println("val " + quote(sym) + " = { (" + quote(x1) + ": " + x1.Type + ", " + quote(x2) + ": " + x2.Type + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + y.Type)
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenFunctions extends CudaGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        // The version for inlined device function
        stream.println(addTab() + "%s %s = %s;".format(remap(x.Type), quote(x), quote(sym)+"_1"))
        emitBlock(y)
        stream.println(addTab() + "%s %s = %s;".format(remap(y.Type), quote(sym), quote(getBlockResult(y))))

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

      case e@Lambda2(fun, x1, x2, y) =>
        // The version for inlined device function
        stream.println(addTab() + "%s %s = %s;".format(remap(x1.Type), quote(x1), quote(sym)+"_1"))
        stream.println(addTab() + "%s %s = %s;".format(remap(x2.Type), quote(x2), quote(sym)+"_2"))
        emitBlock(y)
        stream.println(addTab() + "%s %s = %s;".format(remap(y.Type), quote(sym), quote(getBlockResult(y))))
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CGenFunctions extends CGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        throw new GenerationFailedException("CGenFunctions: Lambda is not supported yet")
      case e@Lambda2(fun, x1, x2, y) =>
        throw new GenerationFailedException("CGenFunctions: Lambda2 is not supported yet")
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}