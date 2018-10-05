package scala.lms
package common

import java.io.PrintWriter

import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException, CNestedCodegen}
import scala.lms.util.ClosureCompare

import scala.reflect.SourceContext

trait Functions extends Base {

  def doLambda[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[A => B]
  implicit def fun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)

  implicit def toLambdaOps[A:Manifest,B:Manifest](fun: Rep[A => B]) = new LambdaOps(fun)

  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A])(implicit pos: SourceContext): Rep[B] = doApply(f,x)
  }

  def doApply[A:Manifest,B:Manifest](fun: Rep[A => B], arg: Rep[A])(implicit pos: SourceContext): Rep[B]
  def uninlinedFunc0[B:Manifest](f: Function0[Rep[B]]): Rep[Unit=>B]
  def uninlinedFunc1[A:Manifest,B:Manifest](f: Function1[Rep[A],Rep[B]]): Rep[A=>B]
  def uninlinedFunc2[A1:Manifest,A2:Manifest,B:Manifest](f: Function2[Rep[A1],Rep[A2],Rep[B]]): Rep[(A1,A2)=>B]
  def uninlinedFunc3[A1:Manifest,A2:Manifest,A3:Manifest,B:Manifest](f: Function3[Rep[A1],Rep[A2],Rep[A3],Rep[B]]): Rep[(A1,A2,A3)=>B]
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

  class LambdaOps0[B:Manifest](f: Rep[Unit => B]) {
    def apply() = doApply(f,unit())
  }
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
  implicit def toLambdaOps0[B:Manifest](fun: Rep[Unit => B]) =
    new LambdaOps0(fun)
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
  case class Lambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B] { val mA = manifest[A]; val mB = manifest[B] }
  case class Apply[A:Manifest,B:Manifest](f: Exp[A => B], arg: Exp[A]) extends Def[B] {
    val mA = manifest[A]
    val mB = manifest[B]
  }
  case class UninlinedFunc0[B:Manifest](b: Block[B]) extends Def[Unit => B] {
    val mB = manifest[B]
  }
  case class UninlinedFunc1[A:Manifest,B:Manifest](s:Sym[A], b: Block[B]) extends Def[A => B] {
	val mA = manifest[A]
    val mB = manifest[B]
  }
  case class UninlinedFunc2[A1:Manifest,A2:Manifest,B:Manifest](s1:Sym[A1], s2:Sym[A2], b: Block[B]) extends Def[(A1,A2) => B] {
	val mA1 = manifest[A1]
	val mA2 = manifest[A2]
    val mB = manifest[B]
  }
  case class UninlinedFunc3[A1:Manifest,A2:Manifest,A3:Manifest,B:Manifest](s1:Sym[A1], s2:Sym[A2], s3:Sym[A3], b: Block[B]) extends Def[(A1,A2,A3) => B] {
	val mA1 = manifest[A1]
	val mA2 = manifest[A2]
	val mA3 = manifest[A3]
    val mB = manifest[B]
  }


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

  /* BEGINNING UNINLINED FUNCTIONS */
  val functionList0 = new scala.collection.mutable.HashMap[Sym[Any],Block[Any]]()
  val functionList1 = new scala.collection.mutable.HashMap[Sym[Any],(Sym[Any],Block[Any])]()
  val functionList2 = new scala.collection.mutable.HashMap[Sym[Any],(Sym[Any],Sym[Any],Block[Any])]()
  val functionList3 = new scala.collection.mutable.HashMap[Sym[Any],(Sym[Any],Sym[Any],Sym[Any],Block[Any])]()
  def uninlinedFunc0[B:Manifest](f: Function0[Rep[B]]) = {
	val b = reifyEffects(f())
    uninlinedFunc0(b)
  }
  def uninlinedFunc0[B:Manifest](b: Block[B]) = {
    val l = reflectEffect(UninlinedFunc0(b), Pure())
    functionList0 += (l.asInstanceOf[Sym[Any]] -> b)
	l
  }

  def uninlinedFunc1[A:Manifest,B:Manifest](f: Function1[Rep[A],Rep[B]]) = {
    val s = fresh[A]
	val b = reifyEffects(f(s))
    uninlinedFunc1(s,b)
  }
  def uninlinedFunc1[A:Manifest,B:Manifest](s: Sym[A], b: Block[B]) = {
    val l = reflectEffect(UninlinedFunc1(s,b), Pure())
    functionList1 += (l.asInstanceOf[Sym[Any]] -> (s,b))
	l
  }

  def uninlinedFunc2[A1:Manifest,A2:Manifest,B:Manifest](f: Function2[Rep[A1],Rep[A2],Rep[B]]) = {
    val s1 = fresh[A1]
    val s2 = fresh[A2]
	val b = reifyEffects(f(s1,s2))
    uninlinedFunc2(s1,s2,b)
  }
  def uninlinedFunc2[A1:Manifest,A2:Manifest,B:Manifest](s1: Sym[A1], s2: Sym[A2], b: Block[B]) = {
    val l = reflectEffect(UninlinedFunc2(s1,s2,b), Pure())
    functionList2 += (l.asInstanceOf[Sym[Any]] -> (s1,s2,b))
	l
  }

  def uninlinedFunc3[A1:Manifest,A2:Manifest,A3:Manifest,B:Manifest](f: Function3[Rep[A1],Rep[A2],Rep[A3],Rep[B]]) = {
    val s1 = fresh[A1]
    val s2 = fresh[A2]
    val s3 = fresh[A3]
	val b = reifyEffects(f(s1,s2,s3))
    uninlinedFunc3(s1,s2,s3,b)
  }
  def uninlinedFunc3[A1:Manifest,A2:Manifest,A3:Manifest,B:Manifest](s1: Sym[A1], s2: Sym[A2], s3: Sym[A3], b: Block[B]) = {
    val l = reflectEffect(UninlinedFunc3(s1,s2,s3,b), Pure())
    functionList3 += (l.asInstanceOf[Sym[Any]] -> (s1,s2,s3,b))
	l
  }
  /* END OF UNINLINED FUNCTIONS */

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Lambda(g,x,y) => toAtom(Lambda(f(g),f(x),f(y))(e.mA,e.mB))(mtype(manifest[A]),implicitly[SourceContext])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
	case UninlinedFunc0(f) => effectSyms(f)
	case UninlinedFunc1(s,f) => s :: effectSyms(f)
	case UninlinedFunc2(s1,s2,f) => s1 :: s2 :: effectSyms(f)
	case UninlinedFunc3(s1,s2,s3,f) => s1 :: s2 :: s3 :: effectSyms(f)
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
            tuple3_get2(t)(mA.typeArguments(1).asInstanceOf[Manifest[a2]], pos),
            tuple3_get3(t)(mA.typeArguments(2).asInstanceOf[Manifest[a3]], pos)))
        }
      case _ if tupledManifestOf(mA, 4) =>
        x match { case t : Rep[(a1,a2,a3,a4)] =>
          UnboxedTuple[A](List(
            tuple4_get1(t)(mA.typeArguments(0).asInstanceOf[Manifest[a1]], pos),
            tuple4_get2(t)(mA.typeArguments(1).asInstanceOf[Manifest[a2]], pos),
            tuple4_get3(t)(mA.typeArguments(2).asInstanceOf[Manifest[a3]], pos),
            tuple4_get4(t)(mA.typeArguments(3).asInstanceOf[Manifest[a4]], pos)))
        }
      case _ if tupledManifestOf(mA, 5) =>
        x match { case t : Rep[(a1,a2,a3,a4,a5)] =>
          UnboxedTuple[A](List(
            tuple5_get1(t)(mA.typeArguments(0).asInstanceOf[Manifest[a1]], pos),
            tuple5_get2(t)(mA.typeArguments(1).asInstanceOf[Manifest[a2]], pos),
            tuple5_get3(t)(mA.typeArguments(2).asInstanceOf[Manifest[a3]], pos),
            tuple5_get4(t)(mA.typeArguments(3).asInstanceOf[Manifest[a4]], pos),
            tuple5_get5(t)(mA.typeArguments(4).asInstanceOf[Manifest[a5]], pos)))
        }
      case _ => x
    }
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
	case UninlinedFunc0(f) => effectSyms(f)
	case UninlinedFunc1(s,f) => s :: effectSyms(f)
	case UninlinedFunc2(s1,s2,f) => s1 :: s2 :: effectSyms(f)
	case UninlinedFunc3(s1,s2,s3,f) => s1 :: s2 :: s3 :: effectSyms(f)
	case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
	case UninlinedFunc0(f) => effectSyms(f)
	case UninlinedFunc1(s,f) => s :: effectSyms(f)
	case UninlinedFunc2(s1,s2,f) => s1 :: s2 :: effectSyms(f)
	case UninlinedFunc3(s1,s2,s3,f) => s1 :: s2 :: s3 :: effectSyms(f)
	case _ => super.containSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, UnboxedTuple(xs), y) => xs.flatMap(syms) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Lambda(g,UnboxedTuple(xs),y) => toAtom(Lambda(f(g),UnboxedTuple(f(xs))(e.mA),f(y))(e.mA,e.mB))(mtype(manifest[A]),implicitly[SourceContext])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
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
      emitValDef(sym, "{" + quote(x, true) + ": (" + x.tp + ") => ")
      emitBlock(y)
      if (y.tp != manifest[Unit]) stream.println(quote(getBlockResult(y)) + ": " + y.tp)
      stream.println("}")

	case UninlinedFunc0(b) => /* Handled in emitFunctions */ {}
	case UninlinedFunc1(s,b) => /* Handled in emitFunctions */ {}
	case UninlinedFunc2(s1,s2,b) => /* Handled in emitFunctions */ {}
	case UninlinedFunc3(s1,s2,s3,b) => /* Handled in emitFunctions */ {}
    case Apply(fun, arg) =>
      arg match {
          case Const(()) => emitValDef(sym, quote(fun) + "()")
          case _ => emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
      }

    case _ => super.emitNode(sym, rhs)
  }

  override def emitFunctions(out: PrintWriter) = {
	functionList0.foreach(func => {
		stream.println("def " + quote(func._1) + "() = {")
		emitBlock(func._2)
		emitBlockResult(func._2)
		stream.println("}")
	})
	functionList0.clear
  }
}

trait ScalaGenTupledFunctions extends ScalaGenFunctions with GenericGenUnboxedTupleAccess {
  val IR: TupledFunctionsExp
  import IR._

  override def quote(x: Exp[Any]) : String = x match {
    case UnboxedTuple(t) => t.map(quote).mkString("((", ",", "))")
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, UnboxedTuple(xs), y) =>
      emitValDef(sym, "{" + xs.map(s=>quote(s, true)+":"+remap(s.tp)).mkString("(",",",")") + " => ")
      emitBlock(y)
      var ytp = remap(y.tp).toString;
      if (ytp != manifest[Unit]) stream.println(quote(getBlockResult(y)) + ": " + ytp )
      stream.println("}")
    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, quote(fun) + args.map(quote).mkString("(", ",", ")"))
    case _ => super.emitNode(sym,rhs)
  }

  def unwrapTupleStr(s: String): Array[String] = {
    if (s.startsWith("scala.Tuple")) s.slice(s.indexOf("[")+1,s.length-1).filter(c => c != ' ').split(",")
    else scala.Array(s)
  }

  override def remap[A](m: Manifest[A]): String = m.toString match {
    case f if f.startsWith("scala.Function") =>
      val targs = m.typeArguments.dropRight(1)
      val res = remap(m.typeArguments.last)
      val targsUnboxed = targs.flatMap(t => unwrapTupleStr(remap(t)))
      val sep = if (targsUnboxed.length > 0) "," else ""
      "scala.Function" + (targsUnboxed.length) + "[" + targsUnboxed.mkString(",") + sep + res + "]"

    case _ => super.remap(m)
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

trait CGenFunctions extends CNestedCodegen with CGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]) = rhs match {
	case UninlinedFunc0(f) => {
	  LIRTraversal(f)
      sym.atPhase(LIRLowering) {
        functionList0 -= sym
        functionList0 += (sym -> LIRLowering(f))
        sym
      }
    }
	case UninlinedFunc1(s,f) => {
	  LIRTraversal(f)
      sym.atPhase(LIRLowering) {
        functionList1 -= sym
        functionList1 += (sym -> (s,LIRLowering(f)))
        sym
      }
    }
	case UninlinedFunc2(s1,s2,f) => {
	  LIRTraversal(f)
      sym.atPhase(LIRLowering) {
        functionList2 -= sym
        functionList2 += (sym -> (s1,s2,LIRLowering(f)))
        sym
      }
    }
	case UninlinedFunc3(s1,s2,s3,f) => {
	  LIRTraversal(f)
      sym.atPhase(LIRLowering) {
        functionList3 -= sym
        functionList3 += (sym -> (s1,s2,s3,LIRLowering(f)))
        sym
      }
    }
	case a@Apply(fun, arg) =>
		sym.atPhase(LIRLowering) {
            val funSym = fun.asInstanceOf[Sym[_]]
            val rM = (fun match {
                case s if functionList0.contains(funSym) => getBlockResult(functionList0(funSym)).tp
                case _ => a.mB
            }).asInstanceOf[Manifest[T]]
			doApply(fun, arg)(a.mA, rM, implicitly[SourceContext]).asInstanceOf[Exp[T]]
		}
	case _ => super.lowerNode(sym, rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println(remap(y.tp)+" "+quote(sym)+"("+remap(x.tp)+" "+quote(x)+") {")
      emitBlock(y)
      val z = getBlockResult(y)
      if (remap(z.tp) != "void")
        stream.println("return " + quote(z) + ";")
      stream.println("}")
	case UninlinedFunc0(b) => /* Handled in emitFunctions */ {}
	case UninlinedFunc1(s,b) => /* Handled in emitFunctions */ {}
	case UninlinedFunc2(s1,s2,b) => /* Handled in emitFunctions */ {}
	case UninlinedFunc3(s1,s2,s3,b) => /* Handled in emitFunctions */ {}
    case a@Apply(fun, arg) =>
      arg match {
        case Const(x) => x match {
          case t: scala.Tuple2[Exp[_],Exp[_]] =>
			emitValDef(sym, quote(fun) + "(" + quote(t._1) + "," + quote(t._2) + ")")
		  case () => emitValDef(sym, quote(fun) + "()")
          case _ => emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
        }
        case _ => emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
      }
    case _ => super.emitNode(sym, rhs)
  }
  override def emitFunctions(out: PrintWriter) = {
    // Output prototypes to resolve dependencies
	functionList0.foreach(f=>stream.println(remap(getBlockResult(f._2).tp) + " " + quote(f._1) + "();"))
	functionList1.foreach(f=>stream.println(remap(getBlockResult(f._2._2).tp) + " " + quote(f._1) + "(" + remap(f._2._1.tp) + " " + quote(f._2._1) + ");"))
	functionList2.foreach(f=>stream.println(remap(getBlockResult(f._2._3).tp) + " " + quote(f._1) + "(" + remap(f._2._1.tp) + " " + quote(f._2._1) + ", " + remap(f._2._2.tp) + " " + quote(f._2._2) +");\n"))
	functionList3.foreach(f=>stream.println(remap(getBlockResult(f._2._4).tp) + " " + quote(f._1) + "(" + remap(f._2._1.tp) + " " + quote(f._2._1) + ", " + remap(f._2._2.tp) + " " + quote(f._2._2) + ", " + remap(f._2._3.tp) + " " + quote(f._2._3) + ");\n"))
	// Output actual functions
	functionList0.foreach(func => {
		stream.println(remap(getBlockResult(func._2).tp) + " " + quote(func._1) + "() {")
		emitBlock(func._2)
		stream.println("return " + quote(getBlockResult(func._2)) + ";")
	    stream.println("}\n")
	})
	functionList1.foreach(func => {
		stream.print(remap(getBlockResult(func._2._2).tp) + " " + quote(func._1) + "(")
		stream.print(remap(func._2._1.tp) + " " + quote(func._2._1))
		stream.println(") {")
		emitBlock(func._2._2)
		stream.println("return " + quote(getBlockResult(func._2._2)) + ";")
	    stream.println("}\n")
	})
	functionList2.foreach(func => {
		stream.print(remap(getBlockResult(func._2._3).tp) + " " + quote(func._1) + "(")
		stream.print(remap(func._2._1.tp) + " " + quote(func._2._1) + ", ")
		stream.print(remap(func._2._2.tp) + " " + quote(func._2._2))
		stream.println(") {")
		emitBlock(func._2._3)
		stream.println("return " + quote(getBlockResult(func._2._3)) + ";")
	    stream.println("}\n")
	})
	functionList3.foreach(func => {
		stream.print(remap(getBlockResult(func._2._4).tp) + " " + quote(func._1) + "(")
		stream.print(remap(func._2._1.tp) + " " + quote(func._2._1) + ", ")
		stream.print(remap(func._2._2.tp) + " " + quote(func._2._2) + ", ")
		stream.print(remap(func._2._3.tp) + " " + quote(func._2._3))
		stream.println(") {")
		emitBlock(func._2._4)
		stream.println("return " + quote(getBlockResult(func._2._4)) + ";")
	    stream.println("}\n")
	})
	functionList0.clear
	functionList1.clear
	functionList2.clear
	functionList3.clear
  }
}

trait CGenTupledFunctions extends CGenFunctions with GenericGenUnboxedTupleAccess {
  val IR: TupledFunctionsExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, UnboxedTuple(xs), y) =>
      stream.println(remap(y.tp)+" "+quote(sym)+"("+xs.map(s=>remap(s.tp)+" "+quote(s)).mkString(",")+") {")
      emitBlock(y)
      val z = getBlockResult(y)
      if (remap(z.tp) != "void")
        stream.println("return " + quote(z) + ";")
      stream.println("}")
    case a@Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, quote(fun) + args.map(quote).mkString("(", ",", ")"))
    case _ => super.emitNode(sym,rhs)
  }
}
