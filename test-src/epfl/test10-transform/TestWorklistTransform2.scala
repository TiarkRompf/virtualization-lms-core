package scala.virtualization.lms
package epfl
package test10

import common._
import internal.{NestedBlockTraversal}
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}
import test8._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

// investigate worklist transform phases (separate from optimization).
// in particular notation like this:
//
//    VectorPlus(a,b).atPhase(xform) {
//      val data = array(vlength(a)) { i => vapply(a,i) +  vapply(b,i) }
//      vfromarray(data)
//    }



trait FWTransform2 extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>
  
  class MyWorklistTransformer extends WorklistTransformer { val IR: self.type = self }
  
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
    
  
  def onCreate[A:Manifest](s: Sym[A], d: Def[A]): Exp[A] = s

  // ----------
  
  override def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    onCreate(s,d)(s.tp)
    super.createDefinition(s,d)
  }

}

trait VectorExpTrans2 extends FWTransform2 with VectorExp with ArrayLoopsExp with ArrayMutationExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
    with EqualExpOpt with StructExp //with VariablesExpOpt 
    with IfThenElseExpOpt with WhileExpOptSpeculative with RangeOpsExp with PrintExp {
  
  
  def vzeros_xform(n: Rep[Int]) = vfromarray(array(n) { i => 0 })

  def vapply_xform[T:Manifest](a: Rep[Vector[T]], x: Rep[Int]) = vtoarray(a).at(x)

  def vplus_xform(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    val data = array(vlength(a)) { i => vapply(a,i) + vapply(b,i) }
    vfromarray(data)
  }

  def vlength_xform[T:Manifest](a: Rep[Vector[T]]) = field[Int](a, "length")
  

  def vfromarray[A:Manifest](x: Exp[Array[A]]): Exp[Vector[A]] = struct(ClassTag[Vector[A]]("Vector"), "data" -> x, "length" -> x.length)
  def vtoarray[A:Manifest](x: Exp[Vector[A]]): Exp[Array[A]] = field[Array[A]](x, "data")


  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = (d match {
    case VectorZeros(n)   => s.atPhase(xform) { vzeros_xform(xform(n)).asInstanceOf[Exp[A]] }
    case VectorApply(a,x) => s.atPhase(xform) { vapply_xform(xform(a), xform(x)).asInstanceOf[Exp[A]] }
    case VectorLength(x)  => s.atPhase(xform) { vlength_xform(xform(x)).asInstanceOf[Exp[A]] }
    case VectorPlus(a,b)  => s.atPhase(xform) { vplus_xform(xform(a),xform(b)).asInstanceOf[Exp[A]] }
    case _ => super.onCreate(s,d)
  }).asInstanceOf[Exp[A]]


  val xform = new MyWorklistTransformer

  override def vapply[T:Manifest](a: Rep[Vector[T]], x: Rep[Int]) = (a,x) match {
    case (Def(VectorLiteral(ax)), Const(x)) => ax(x)
    case _ => super.vapply(a,x)
  }
  
}



class TestForward2 extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test10-"
  
  trait DSL extends VectorOps with Arith with OrderingOps with BooleanOps with LiftVariables 
    with IfThenElse with While with RangeOps with Print {
    def test(x: Rep[Int]): Rep[Unit]
  }
  trait Impl extends DSL with VectorExpTrans2 with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
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
  
  def testWorklist1 = withOutFileChecked(prefix+"worklist21") {
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

  def testWorklist2 = withOutFileChecked(prefix+"worklist22") {
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

  def testWorklist3 = withOutFileChecked(prefix+"worklist23") {
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