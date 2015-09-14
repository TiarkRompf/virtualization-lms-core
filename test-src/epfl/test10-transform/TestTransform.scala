package scala.lms
package epfl
package test10

import common._
import internal._
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}
import test8._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext



class TestTransform extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test10-"
  
  trait DSL extends VectorOps with LiftPrimitives with PrimitiveOps with OrderingOps with BooleanOps with LiftVariables 
    with IfThenElse with While with RangeOps with Print {
    def test(x: Rep[Int]): Rep[Unit]
  }
  
  trait Impl extends DSL with VectorExp with PrimitiveOpsExp with OrderingOpsExpOpt with BooleanOpsExp 
    with EqualExpOpt with ArrayMutationExp with IfThenElseFatExp with LoopsFatExp with WhileExpOptSpeculative 
    with StringOpsExp with SeqOpsExp    
    with RangeOpsExp with PrintExp with FatExpressions { self =>
    override val verbosity = 1
    val runner = new Runner { val p: self.type = self }
    runner.run()
  }
  
  trait Codegen extends ScalaGenVector with ScalaGenArrayMutation with ScalaGenPrimitiveOps with ScalaGenOrderingOps 
    with ScalaGenVariables with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhileOptSpeculative 
    with ScalaGenRangeOps with ScalaGenPrint {
    val IR: Impl
  }
  
  
  trait Runner {
    val p: Impl
    def run() = {
      import p.{intTyp,unitTyp}
      val x = p.fresh[Int]
      val y = p.reifyEffects(p.test(x))

      val codegen = new Codegen { val IR: p.type = p }

      val graph = p.globalDefs
      println("-- full graph")
      graph foreach println

      println("-- before transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(y)
      }

      val trans = new MyTransformer {
        val IR: p.type = p
      }
      try {
        val z = trans.transformBlock(y)

        println("-- after transformation")
        codegen.withStream(new PrintWriter(System.out)) {
          codegen.emitBlock(z)
        }
      } catch {
        case ex =>
        println("error: " + ex)
      }
      println("-- done")      
    }
  }
  
  
  trait MyTransformer extends ForwardTransformer { 
    val IR: Impl
    import IR.{__newVar => _, _}
    
    // a + b --> b + a, but only in then-branches of an if-then-else
    
    var isInThenBranch = false
    
    override def transformStm(stm: Stm): Exp[Any] = stm match {
      case TP(s,VectorPlus(a,b)) if isInThenBranch =>
        println("replacing " + stm)
        vplus(apply(b),apply(a))
      case TP(s,Reflect(IfThenElse(c,a,b), u, es)) =>
        println("encountering if then else " + stm)
        __ifThenElse(apply(c), {
          val saveFlag = isInThenBranch
          isInThenBranch = true
          val r = reflectBlock(a)
          isInThenBranch = saveFlag
          r
        }, {
          reflectBlock(b)
        })(mtype(s.tp),mpos(s.pos))
      case _ => super.transformStm(stm)
    }
  }
  
  
  
  
  // test simple block transform
  def testTransform1 = withOutFileChecked(prefix+"transform1") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val z = vzeros(100)
        val y = vzeros(100)
        val a = vplus(z,y)
        val b = vplus(z,a)
        print(b)
      }
    }
    new Prog with Impl
  }

  def testTransform2 = withOutFileChecked(prefix+"transform2") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val a = vzeros(100) // will be moved into branches
        val b = vzeros(50)
        val c = vplus(a,b)
        if (x == 0) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
      }
    }
    new Prog with Impl
  }

  def testTransform3 = withOutFileChecked(prefix+"transform3") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val a = vzeros(100) // will be moved into branches
        val b = vzeros(50)
        val c = vplus(a,b)
        if (x == 0) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
        if (x == 1) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
      }
    }
    new Prog with Impl
  }

}
