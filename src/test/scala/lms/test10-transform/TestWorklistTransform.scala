package scala.lms
package test10

import ops._
import internal.{NestedBlockTraversal}
import util.OverloadHack

import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}
import test8._

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

// investigate worklist transform phases (separate from optimization).
// in particular notation like this:
//
//    VectorPlus(a,b).atPhase(xform) {
//      val data = array(vlength(a)) { i => vapply(a,i) +  vapply(b,i) }
//      vfromarray(data)
//    }



trait FWTransform1 extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>

  class MyWorklistTransformer extends WorklistTransformer { val IR: self.type = self }

  def xform: MyWorklistTransformer


  // ---------- Exp api

  implicit def toAfter[A:Manifest](x: Def[A]) = new { def atPhase(t: MyWorklistTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }
  implicit def toAfter[A](x: Exp[A]) = new { def atPhase(t: MyWorklistTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }

  // transform x to y at the *next* iteration of t.
  // note: if t is currently active, it will continue the current pass with x = x.
  // do we need a variant that replaces x -> y immediately if t is active?

  def transformAtPhase[A](x: Exp[A])(t: MyWorklistTransformer)(y: => Exp[A]): Exp[A] = {
    t.register(x)(y)
    x
  }


  // ----------

  // we need to apply the current substitution to each Def we create:
  // Foo(x) atPhase(t) { bar(x) }   <--- x in bar(x)  will refer to a sym that may have been replaced itself

  protected override implicit def toAtom[A:Manifest](d: Def[A])(implicit pos: SourceContext): Exp[A] = { // override createDefinition instead?
    val in = syms(d)
    val actual = xform(in)

    if (in != actual) {
      println("toAtom transform "+d+" " + in + " -> " + actual)
      mirror(d,xform)
    } else {
      super.toAtom(d)
    }
  }

}

trait VectorExpTrans1 extends FWTransform1 with VectorExp with ArrayLoopsExp with ArrayMutationExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp
    with EqualExpOpt with StructExp //with VariablesExpOpt
    with IfThenElseExpOpt with WhileExpOptSpeculative with RangeOpsExp with PrintExp {


  // TODO: this is not very modular. should it be more modular?
  // can we (do we even want to?) add the atPhase transforms in a
  // separate trait? the argument against it is that we could just
  // implement the transform by pattern matching against Defs

  override def vzeros(n: Rep[Int]) =  VectorZeros(n).atPhase(xform) { vfromarray(array(n) { i => 0 }) }

  override def vapply[T:Manifest](a: Rep[Vector[T]], x: Rep[Int]) = (a,x) match {
    case (Def(VectorLiteral(ax)), Const(x)) => ax(x)
    case _ =>
      VectorApply(a,x).atPhase(xform) { vtoarray(a).at(x) }
  }



  // how does this interact with CSE?
  // Foo(x) atPhase(t) { bar(x) }   <--- Foo(x) may resolve to existing sym z, which is then scheduled to be replaced

  override def vplus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = (a,b) match {
    //case (Def(VectorZeros(n)), b) => b
    //case (a, Def(VectorZeros(n))) => a
    case _ =>
      VectorPlus(a,b).atPhase(xform) {
        val data = array(vlength(a)) { i => vapply(a,i) + vapply(b,i) }
        vfromarray(data)
      }
  }


/*
  override def vlength[T:Manifest](a: Rep[Vector[T]]) = a match {
    case Def(VectorFromArray(b)) => b.length
    case _ => super.vlength(a)
  }

  case class VectorFromArray[T](a: Rep[Array[T]]) extends Def[Vector[T]]
  case class VectorToArray[T](a: Rep[Vector[T]]) extends Def[Array[T]]

  def vfromarray[A:Manifest](x: Exp[Array[A]]): Exp[Vector[A]] = VectorFromArray(x)
  def vtoarray[A:Manifest](x: Exp[Vector[A]]): Exp[Array[A]] = x match {
    case Def(VectorFromArray(z)) => z
    case _ => VectorToArray(x)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case VectorFromArray(a) => vfromarray(f(a))
    case VectorToArray(a) => vtoarray(f(a))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // gadt fail
*/

  def vfromarray[A:Manifest](x: Exp[Array[A]]): Exp[Vector[A]] = struct(ClassTag[Vector[A]]("Vector"), "data" -> x, "length" -> x.length)
  def vtoarray[A:Manifest](x: Exp[Vector[A]]): Exp[Array[A]] = field[Array[A]](x, "data")

  override def vlength[T:Manifest](a: Rep[Vector[T]]) = {
//    VectorLength(a).atPhase(xform) {
      field[Int](a, "length")
//    }
  }


  val xform = new MyWorklistTransformer

}



class TestForward1 extends FileDiffSuite {

  val prefix = "test-out/epfl/test10-"

  trait DSL extends VectorOps with Arith with OrderingOps with BooleanOps with LiftVariables
    with IfThenElse with While with RangeOps with Print {
    def test(x: Rep[Int]): Rep[Unit]
  }
  trait Impl extends DSL with VectorExpTrans1 with ArithExp with OrderingOpsExpOpt with BooleanOpsExp
    with EqualExpOpt with StructFatExpOptCommon //with VariablesExpOpt
    with IfThenElseExpOpt with WhileExpOptSpeculative with RangeOpsExp with PrintExp { self =>
    override val verbosity = 2

    val codegen = new ScalaGenVector with ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps
      with ScalaGenVariables with ScalaGenIfThenElseFat with ScalaGenStruct with ScalaGenRangeOps
      with ScalaGenPrint with ScalaGenFatStruct { val IR: self.type = self }

    codegen.withStream(new PrintWriter(System.out)) {
      println("### first")
      val b1 = reifyEffects(test(fresh))
      println("--- code ---")
      codegen.emitBlock(b1)
      codegen.stream.flush
      def iter(n: Int, b1: Block[Unit]): Unit = if (n > 0) {
        println()
        println("### next")
        val b2 = xform.runOnce(b1)
        println("--- code ---")
        codegen.emitBlock(b2)
        codegen.stream.flush
        if (!xform.isDone) iter(n-1,b2)
      }
      iter(10,b1) // fixed num of iterations for now
    }
  }


  def testWorklist1 = withOutFileChecked(prefix+"worklist1") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val z = vzeros(100)
        val y = vzeros(100)
        val a = vplus(z,y) // this is disabled right now but could trigger rewriting before going to arrays
        val b = vplus(z,a)
        print(b)
      }
    }
    new Prog with Impl
  }

  def testWorklist2 = withOutFileChecked(prefix+"worklist2") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val z = vzeros(100)
        val y = vliteral(List(z))
        val a = vapply(y,0) // shortcut before switching to arrays
        print(a)
      }
    }
    new Prog with Impl
  }

  def testWorklist3 = withOutFileChecked(prefix+"worklist3") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val z1 = vzeros(100)
        val z2 = vzeros(50)
        val z = if (x > 0) z1 else z2
        val y = vzeros(100)
        val a = vplus(z,y)
        print(a)
      }
    }
    new Prog with Impl
  }

}