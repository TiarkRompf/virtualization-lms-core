package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.reflect.SourceContext

trait Functions extends Base {

  implicit def doLambda[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[A => B]
  implicit def doLambda2[A1:Manifest,A2:Manifest,B:Manifest](fun: (Rep[A1],Rep[A2]) => Rep[B])(implicit pos: SourceContext): Rep[(A1,A2) => B]

  implicit def toLambdaOps[A:Manifest,B:Manifest](fun: Rep[A => B]) = new LambdaOps(fun)
  
  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A])(implicit pos: SourceContext): Rep[B] = doApply(f,x)
  }

  def doApply[A:Manifest,B:Manifest](fun: Rep[A => B], arg: Rep[A])(implicit pos: SourceContext): Rep[B]
  def doApply2[A1:Manifest,A2:Manifest,B:Manifest](fun: Rep[(A1,A2) => B], arg1: Rep[A1], arg2: Rep[A2])(implicit pos: SourceContext): Rep[B]

}

trait FunctionsExp extends Functions with EffectExp {

  case class Lambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], x: Sym[A], y: Block[B]) extends Def[A => B] {
    val m = manifest[A => B]
    val mA = manifest[A]
    val mB = manifest[B]
  }
  
  case class Lambda2[A1:Manifest,A2:Manifest,B:Manifest](f: (Exp[A1],Exp[A2]) => Exp[B], x1: Sym[A1], x2: Sym[A2], y: Block[B]) extends Def[(A1,A2) => B]{
    val mA1 = manifest[A1]
    val mA2 = manifest[A2]
    val mB = manifest[B]
  }

  case class Apply[A:Manifest,B:Manifest](f: Exp[A => B], arg: Exp[A]) extends Def[B] {
    val mA = manifest[A]
    val mB = manifest[B]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = 
    (e match {
       case l@Lambda(func,x,y) =>
         if (f.hasContext)
           toAtom(Lambda(f(func),x,Block(f.reflectBlock(y)))(l.mA, l.mB))
         else
           Lambda(f(func),x,f(y))(l.mA, l.mB)
           
       case l@Lambda2(func, x1, x2, y) =>
         if (f.hasContext)
           toAtom(Lambda2(f(func), x1, x2, Block(f.reflectBlock(y)))(l.mA1, l.mA2, l.mB))
         else
           Lambda2(f(func),x1,x2,f(y))(l.mA1, l.mA2, l.mB)
           
       case a@Apply(func, arg) =>
           toAtom(Apply(f(func), f(arg))(a.mA, a.mB))
           
       case _ => super.mirror(e, f)
       
    }).asInstanceOf[Exp[A]]
  
  def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B])(implicit pos: SourceContext) : Exp[A => B] = {
    val x = fresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site. 
                               // TODO: this will not work if f is recursive. 
                               // need to incorporate the other pieces at some point.
    Lambda(f, x, y)
  }

  def doLambda2[A1:Manifest,A2:Manifest,B:Manifest](f: (Exp[A1],Exp[A2]) => Exp[B])(implicit pos: SourceContext) : Exp[(A1,A2) => B] = {

    val x1 = fresh[A1]
    val x2 = fresh[A2]
    val y = reifyEffects(f(x1,x2)) // unfold completely at the definition site.
                               // TODO: this will not work if f is recursive.
                               // need to incorporate the other pieces at some point.
    Lambda2(f, x1, x2, y)
  }

  def doApply[A:Manifest,B:Manifest](f: Exp[A => B], x: Exp[A])(implicit pos: SourceContext): Exp[B] = f match {
/*
    case Def(Lambda(_,_,Def(Reify(_,_,_)))) => 
      // if function result is known to be effectful, so is application
      reflectEffect(Apply(f,x))
*/
    case Def(Lambda(_,_,y)) => 
      // if function result is known to be pure, so is application
      // TODO: what about 
      val ye = summarizeEffects(y)
      reflectEffect(Apply(f, x), ye)
    case _ => // unknown function, assume it is effectful TODO: global vs simple?
      reflectEffect(Apply(f, x))
  }

  def doApply2[A1:Manifest,A2:Manifest,B:Manifest](fun: Exp[(A1,A2) => B], arg1: Exp[A1], arg2: Exp[A2])(implicit pos: SourceContext): Exp[B] = sys.error("TODO!")

  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case Lambda2(f, x1, x2, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => x :: effectSyms(y)
    case Lambda2(f, x1, x2, y) => x1 :: x2 :: effectSyms(y)
    case _ => super.boundSyms(e)
  }  

// TODO: right now were trying to hoist as much as we can out of functions. 
// That might not always be appropriate. A promising strategy would be to have
// explicit 'hot' and 'cold' functions. 

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

}

trait BaseGenFunctions extends GenericNestedCodegen {
  val IR: FunctionsExp
  import IR._


}

trait ScalaGenFunctions extends ScalaGenEffect with BaseGenFunctions {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println("@inline")
      stream.println("final def " + quote(sym) + " (" + quote(x) + ": (" + x.tp + ")) = {")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + y.tp)
      stream.println("}")

    case e@Lambda2(fun, x1, x2, y) =>
      stream.println("@inline")
      stream.println("final def " + quote(sym) + " (" + quote(x1) + ": " + x1.tp + ", " + quote(x2) + ": " + x2.tp + ") = { ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + y.tp)
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenFunctions extends CudaGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        // The version for inlined device function
        stream.println(addTab() + "%s %s = %s;".format(remap(x.tp), quote(x), quote(sym)+"_1"))
        emitBlock(y)
        stream.println(addTab() + "%s %s = %s;".format(remap(y.tp), quote(sym), quote(getBlockResult(y))))

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
        stream.println(addTab() + "%s %s = %s;".format(remap(x1.tp), quote(x1), quote(sym)+"_1"))
        stream.println(addTab() + "%s %s = %s;".format(remap(x2.tp), quote(x2), quote(sym)+"_2"))
        emitBlock(y)
        stream.println(addTab() + "%s %s = %s;".format(remap(y.tp), quote(sym), quote(getBlockResult(y))))
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait OpenCLGenFunctions extends OpenCLGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        throw new GenerationFailedException("OpenCLGenFunctions: Lambda is not supported yet")
      case e@Lambda2(fun, x1, x2, y) =>
        throw new GenerationFailedException("OpenCLGenFunctions: Lambda2 is not supported yet")
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CGenFunctions extends CGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
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
