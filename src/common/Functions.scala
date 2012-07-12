package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.virtualization.lms.util.ClosureCompare

import scala.reflect.SourceContext

trait Functions extends Base {

  def doLambda[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[A => B]
  implicit def fun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)

  implicit def toLambdaOps[A:Manifest,B:Manifest](fun: Rep[A => B]) = new LambdaOps(fun)
  
  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A])(implicit pos: SourceContext): Rep[B] = doApply(f,x)
  }

  def doApply[A:Manifest,B:Manifest](fun: Rep[A => B], arg: Rep[A])(implicit pos: SourceContext): Rep[B]
}

trait TupledFunctions extends Functions with TupleOps {
  implicit def fun[B:Manifest](f: () => Rep[B]): Rep[Unit=>B] =
    fun((t: Rep[Unit]) => f())
  implicit def fun[A1:Manifest,A2:Manifest,B:Manifest](f: (Rep[A1], Rep[A2]) => Rep[B]): Rep[((A1,A2))=>B] =
    fun((t: Rep[(A1,A2)]) => f(tuple2_get1(t), tuple2_get2(t)))
  implicit def fun[A1:Manifest,A2:Manifest,A3:Manifest,B:Manifest](f: (Rep[A1], Rep[A2], Rep[A3]) => Rep[B]): Rep[((A1,A2,A3))=>B] =
    fun((t: Rep[(A1,A2,A3)]) => f(tuple3_get1(t), tuple3_get2(t), tuple3_get3(t)))
  implicit def fun[A1:Manifest,A2:Manifest,A3:Manifest,A4:Manifest,B:Manifest](f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4]) => Rep[B]): Rep[((A1,A2,A3,A4))=>B] =
    fun((t: Rep[(A1,A2,A3,A4)]) => f(tuple4_get1(t), tuple4_get2(t), tuple4_get3(t), tuple4_get4(t)))
  implicit def fun[A1:Manifest,A2:Manifest,A3:Manifest,A4:Manifest,A5:Manifest,B:Manifest](f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5]) => Rep[B]): Rep[((A1,A2,A3,A4,A5))=>B] =
    fun((t: Rep[(A1,A2,A3,A4,A5)]) => f(tuple5_get1(t), tuple5_get2(t), tuple5_get3(t), tuple5_get4(t), tuple5_get5(t)))

  class LambdaOps2[A1:Manifest,A2:Manifest,B:Manifest](f: Rep[((A1,A2)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2]) = doApply(f,(x1, x2))
    def apply(x: Rep[(A1,A2)]): Rep[B] = doApply(f,x)
  }
  class LambdaOps3[A1:Manifest,A2:Manifest,A3:Manifest,B:Manifest](f: Rep[((A1,A2,A3)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2], x3: Rep[A3]) = doApply(f,(x1, x2, x3))
    def apply(x: Rep[(A1,A2,A3)]): Rep[B] = doApply(f,x)
  }
  class LambdaOps4[A1:Manifest,A2:Manifest,A3:Manifest,A4:Manifest,B:Manifest](f: Rep[((A1,A2,A3,A4)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4]) = doApply(f,(x1, x2, x3, x4))
    def apply(x: Rep[(A1,A2,A3,A4)]): Rep[B] = doApply(f,x)
  }
  class LambdaOps5[A1:Manifest,A2:Manifest,A3:Manifest,A4:Manifest,A5:Manifest,B:Manifest](f: Rep[((A1,A2,A3,A4,A5)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5]) = doApply(f,(x1, x2, x3, x4, x5))
    def apply(x: Rep[(A1,A2,A3,A4,A5)]): Rep[B] = doApply(f,x)
  }
  implicit def toLambdaOpsAny[B:Manifest](fun: Rep[Any => B]) =
    toLambdaOps(fun)
  implicit def toLambdaOps2[A1:Manifest,A2:Manifest,B:Manifest](fun: Rep[((A1,A2)) => B]) =
    new LambdaOps2(fun)
  implicit def toLambdaOps3[A1:Manifest,A2:Manifest,A3:Manifest,B:Manifest](fun: Rep[((A1,A2,A3)) => B]) =
    new LambdaOps3(fun)
  implicit def toLambdaOps4[A1:Manifest,A2:Manifest,A3:Manifest,A4:Manifest,B:Manifest](fun: Rep[((A1,A2,A3,A4)) => B]) =
    new LambdaOps4(fun)
  implicit def toLambdaOps5[A1:Manifest,A2:Manifest,A3:Manifest,A4:Manifest,A5:Manifest,B:Manifest](fun: Rep[((A1,A2,A3,A4,A5)) => B]) =
    new LambdaOps5(fun)
}

trait FunctionsExp extends Functions with EffectExp {
  case class Lambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B]
  case class Apply[A:Manifest,B:Manifest](f: Exp[A => B], arg: Exp[A]) extends Def[B]

  // unboxedFresh and unbox are hooks that can be overridden to
  // implement multiple-arity functions with tuples. These two methods
  // should be overridden consistently. unboxedFresh is used when
  // creating an abstraction, and unbox when applying it. See
  // TupledFunctionsExp for an example.

  def unboxedFresh[A:Manifest] : Exp[A] = fresh[A]
  def unbox[A:Manifest](x : Exp[A])(implicit pos: SourceContext) : Exp[A] = x

  def doLambdaDef[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]) : Def[A => B] = {
    val x = unboxedFresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site. 

    Lambda(f, x, y)
  }

  override def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B])(implicit pos: SourceContext): Exp[A => B] =
    doLambdaDef(f)

  override def doApply[A:Manifest,B:Manifest](f: Exp[A => B], x: Exp[A])(implicit pos: SourceContext): Exp[B] = {
    val x1 = unbox(x)
    f match {
      case Def(Lambda(_,_,y)) =>
        // if function result is known to be pure, so is application
        // TODO: what about
        val ye = summarizeEffects(y)
        reflectEffect(Apply(f, x1), ye)
      case _ => // unknown function, assume it is effectful TODO: global vs simple?
        reflectEffect(Apply(f, x1))
    }
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(x) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }  

// TODO: right now were trying to hoist as much as we can out of functions. 
// That might not always be appropriate. A promising strategy would be to have
// explicit 'hot' and 'cold' functions. 

/*
  override def hotSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.hotSyms(e)
  }
*/

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Lambda(f, x, y) => freqHot(y)
    case _ => super.symsFreq(e)
  }

}

trait TupledFunctionsExp extends TupledFunctions with FunctionsExp with TupleOpsExp {
  // used to represent unboxed tuples by a list of vars
  // T will be a tuple of a specified arity
  case class UnboxedTuple[T: Manifest](val vars: List[Exp[Any]]) extends Exp[T]

  private def tupledManifest[T](m: Manifest[T]): Boolean = m.erasure.getName startsWith "scala.Tuple"
  private def tupledManifestOf[T](m: Manifest[T], arity: Int): Boolean = m.erasure.getName == "scala.Tuple" + arity

  override def unboxedFresh[A:Manifest] : Exp[A] = {
    val mA = implicitly[Manifest[A]]
    if (mA == implicitly[Manifest[Unit]] || tupledManifest(mA))
      UnboxedTuple[A](mA.typeArguments.map(fresh(_)))
    else fresh[A]
  }

  override def unbox[A:Manifest](x : Exp[A])(implicit pos: SourceContext) : Exp[A] = {
    val mA = implicitly[Manifest[A]]
    x match {
      case _ : UnboxedTuple[A] => x
      case _ if mA == implicitly[Manifest[Unit]] =>
	UnboxedTuple[A](List())
      case _ if tupledManifestOf(mA, 2) =>
	x match { case t : Rep[(a1,a2)] =>
	  UnboxedTuple[A](List(
	    tuple2_get1(t)(mA.typeArguments(0).asInstanceOf[Manifest[a1]], pos),
	    tuple2_get2(t)(mA.typeArguments(1).asInstanceOf[Manifest[a2]], pos)))
	}
      case _ if tupledManifestOf(mA, 3) =>
	x match { case t : Rep[(a1,a2,a3)] =>
	  UnboxedTuple[A](List(
	    tuple3_get1(t)(mA.typeArguments(0).asInstanceOf[Manifest[a1]], pos),
	    tuple3_get2(t)(mA.typeArguments(0).asInstanceOf[Manifest[a2]], pos),
	    tuple3_get3(t)(mA.typeArguments(1).asInstanceOf[Manifest[a3]], pos)))
	}
      case _ if tupledManifestOf(mA, 4) =>
	x match { case t : Rep[(a1,a2,a3,a4)] =>
	  UnboxedTuple[A](List(
	    tuple4_get1(t)(mA.typeArguments(0).asInstanceOf[Manifest[a1]], pos),
	    tuple4_get2(t)(mA.typeArguments(0).asInstanceOf[Manifest[a2]], pos),
	    tuple4_get3(t)(mA.typeArguments(0).asInstanceOf[Manifest[a3]], pos),
	    tuple4_get4(t)(mA.typeArguments(1).asInstanceOf[Manifest[a4]], pos)))
	}
      case _ if tupledManifestOf(mA, 5) =>
	x match { case t : Rep[(a1,a2,a3,a4,a5)] =>
	  UnboxedTuple[A](List(
	    tuple5_get1(t)(mA.typeArguments(0).asInstanceOf[Manifest[a1]], pos),
	    tuple5_get2(t)(mA.typeArguments(0).asInstanceOf[Manifest[a2]], pos),
	    tuple5_get3(t)(mA.typeArguments(0).asInstanceOf[Manifest[a3]], pos),
	    tuple5_get4(t)(mA.typeArguments(0).asInstanceOf[Manifest[a4]], pos),
	    tuple5_get5(t)(mA.typeArguments(1).asInstanceOf[Manifest[a5]], pos)))
	}
      case _ => x
    }
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, UnboxedTuple(xs), y) => xs.flatMap(syms) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }  
}

trait FunctionsRecursiveExp extends FunctionsExp with ClosureCompare {
  var funTable: List[(Sym[_], Any)] = List()
  override def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B])(implicit pos: SourceContext): Exp[A => B] = {
    val can = canonicalize(f)
    funTable.find(_._2 == can) match {
      case Some((funSym, _)) =>
        funSym.asInstanceOf[Exp[A=>B]]
      case _ =>
        val funSym = fresh[A=>B]
        funTable = (funSym,can)::funTable
        createDefinition(funSym, doLambdaDef(f))
        funSym
    }
  }
  
}

trait TupledFunctionsRecursiveExp extends FunctionsRecursiveExp with TupledFunctionsExp

trait GenericGenUnboxedTupleAccess extends GenericNestedCodegen {
  val IR: TupledFunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Tuple2Access1(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(0)))
    case Tuple2Access2(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(1)))

    case Tuple3Access1(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(0)))
    case Tuple3Access2(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(1)))
    case Tuple3Access3(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(2)))

    case Tuple4Access1(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(0)))
    case Tuple4Access2(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(1)))
    case Tuple4Access3(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(2)))
    case Tuple4Access4(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(3)))

    case Tuple5Access1(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(0)))
    case Tuple5Access2(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(1)))
    case Tuple5Access3(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(2)))
    case Tuple5Access4(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(3)))
    case Tuple5Access5(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(4)))

    case _ => super.emitNode(sym, rhs)
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
      emitValDef(sym, "{" + quote(x) + ": (" + x.tp + ") => ")
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
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}
