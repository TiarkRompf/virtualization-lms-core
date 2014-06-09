package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.{OverloadHack, GraphUtil}
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait MyFusionProgArith extends Arith with ArrayLoopsMC with Print with OrderingOps {
//with PrimitiveOps with LiftNumeric with BooleanOps {
  def test(x: Rep[Int]): Rep[Unit]
}

trait ImplArith extends MyFusionProgArith with ArithExp  with ArrayLoopsMCFatExp with ArrayLoopsMCFusionExtractors
    with IfThenElseFatExp with PrintExp with OrderingOpsExp
//    with NumericOpsExp with PrimitiveOpsExp with BooleanOpsExp
    with LoopFusionCore2 { self =>
  override val verbosity = 2 // 1: only printlog, 2: also printdbg
  val runner = new RunnerArith { val p: self.type = self }
  runner.run()
}

trait CodegenArith extends ScalaGenArith with ScalaGenPrint
  with ScalaGenOrderingOps with ScalaGenIfThenElse
  with ScalaGenNumericOps with ScalaGenPrimitiveOps
  with ScalaGenBooleanOps with ScalaGenArrayLoopsMCFat { val IR: ImplArith }

trait FusionCodegenArith extends CodegenArith with LoopFusionSchedulingOpt { val IR: ImplArith }


trait RunnerArith {
  val p: ImplArith
  def run() = {
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new CodegenArith { val IR: p.type = p }
    val fusionCodegenArith = new FusionCodegenArith { val IR: p.type = p }

    val graph = p.globalDefs
    println("-- full graph")
    graph foreach println

    println("\n-- before transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(y)
    }
    
    val verticalTransf = new LoopFusionVerticalTransformer {
      val IR: p.type = p
    }
    val horTransf = new LoopFusionHorizontalTransformer {
      val IR: p.type = p
    }

    try {
      println("\n-- vertical transformation")

      val v = verticalTransf.transformBlock(y)
      // TODO how to transmit state more cleanly?
      val vFused = verticalTransf.FusedSyms.getFusedSyms
      println("\n(VFT) all vertically fused: " + vFused.values.toList.distinct.map(_._1).mkString("\n"))

      println("\n-- after vertical transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(v)
      }

      println("\n-- horizontal transformation")
      horTransf.setVerticallyFusedSyms(vFused)
      val h = horTransf.transformBlock(v)

      val hFused = horTransf.AllFusionScopes.get
      println("\n(HFT) all horizontally fused: " + hFused.mkString("\n"))


      println("\n-- after horizontal transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(h)
      }

      println("\n-- fusion")

      fusionCodegenArith.withStream(new PrintWriter(System.out)) {
        fusionCodegenArith.emitBlock(h)
      }

      println("-- done")

      // TODO how to run the program? have block, not f: Exp[A] => Exp[B]
      // val test = compile({x: Int => h})
      // test(42)
    } catch {
      case ex: Throwable => println("error: " + ex)
    }
  }
}

// Original loop fusion tests
class TestFusion4 extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-wip2-"

  def testFusionTransform00 = withOutFileChecked(prefix+"fusion00") {
    trait Prog extends MyFusionProgArith with ImplArith {
      implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
      def test(x: Rep[Int]) = {
        val constant = array(100) { i => 1 }
        val linear = array(100) { i => 2*i }
        val affine = array(100) { i => constant.at(i) + linear.at(i) }

        def square(x: Rep[Double]) = x*x
        def mean(x: Rep[Array[Double]]) = sumD(x.length) { i => x.at(i) } / x.length
        def variance(x: Rep[Array[Double]]) = sumD(x.length) { i => square(x.at(i)) } / x.length - square(mean(x))

        val data = affine  
        val m = mean(data)
        val v = variance(data)

        print(m)
        print(v)
      }
    }
    new Prog with ImplArith
  }

  def testFusionTransform01 = withOutFileChecked(prefix+"fusion01") {
    trait Prog extends MyFusionProgArith with ImplArith {
      implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
      def test(x: Rep[Int]) = {
        def filter[T:Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) = 
          arrayIf(x.length)({ i => p(x.at(i)) }, { i => x.at(i) })
        
        val range = array(100) { i => i }
        val odds = filter(range) { z => z > 50 }
        val res = sumD(odds.length) { i => odds.at(i) }
            
        print(res)
      }
    }
    new Prog with ImplArith
  }
  
  def testFusionTransform02 = withOutFileChecked(prefix+"fusion02") {
    trait Prog extends MyFusionProgArith with ImplArith {
      def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
      def test(x: Rep[Int]) = {
        // there was a bug were this would lead to a recursive schedule:
        // as,bs are fused (no dependencies)
        // cs,ds are fused (no dependencies)
        // but there are cross deps ds->as, bs->cs ...
        
        val cs = array(100) { i => 9.0 }
        val as = array(50) { i => 3.0 }
        val bs = array(50) { i => cs.foo }
        val ds = array(100) { i => as.foo }
        
        print(as)
        print(bs)
        print(cs)
        print(ds)
      }
    }
    new Prog with ImplArith
  }

  def testFusionTransform03 = withOutFileChecked(prefix+"fusion03") {
    trait Prog extends MyFusionProgArith with ImplArith {
      def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
      def test(x: Rep[Int]) = {
        // ds is fused with as, bs&cs,
        // as.at(i) -> ax, bs.at(i) -> bx, cs.at(i) -> cx
        // x is now consumer of ax and bx which are in same (fused) scope,
        // so fuse again
        // note that ax, bx and cx are indep of i, so hoisted, but fusion happens
        // on lower level despite them being on higher level because in the
        // original code they are on the lower level too, they're in scope because
        // outer loops are fused.
      
        val as = array(100) { i => 
          array(50) { j => 1.0 } // ax
        }
        val bs = array(100) { i => 
          array(50) { j => 2.0 } // bx
        }
        val cs = array(100) { i => 
          sumD(50) { j => 4.0 } // cx
        }
        val ds = array(100) { i => 
          val x = sumD(50) { j => as.at(i).at(j) + bs.at(i).at(j) }
          val y = cs.at(i)
          x + y
        }

        print(as)
        print(bs)
        print(cs)
        print(ds)
      }
    }
    new Prog with ImplArith
  }

  def testFusionTransform04 = withOutFileChecked(prefix+"fusion04") {
    trait Prog extends MyFusionProgArith with ImplArith {
      def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
      def test(x: Rep[Int]) = {
        // Same as last test, but now inner loops not hoisted because they
        // depend on the outer index variable.
        // ds is fused with as, bs&cs,
        // as.at(i) -> ax, bs.at(i) -> bx, cs.at(i) -> cx
        // x is now consumer of ax and bx which are in same (fused) scope,
        // so fuse again
      
        val as = array(100) { i => 
          array(i) { j => 1.0 }
        }
        val bs = array(100) { i => 
          array(i) { j => 2.0 }
        }
        val cs = array(100) { i => 
          sumD(i) { j => 4.0 }
        }
        val ds = array(100) { i => 
          val x = sumD(i) { j => as.at(i).at(j) + bs.at(i).at(j) }
          val y = cs.at(i)
          x + y
        }
        print(as)
        print(bs)
        print(cs)
        print(ds)
      }
    }
    new Prog with ImplArith
  }

  def testFusionTransform05 = withOutFileChecked(prefix+"fusion05") {
    trait Prog extends MyFusionProgArith with ImplArith {
      // override def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = if (x == y) {
      //   println("*** removing self subtraction " + x + " - " + y)
      //   0
      // } else super.infix_-(x,y) //  optimizations to trigger test behavior

      def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
      def test(x: Rep[Int]) = {    
        // Successive fusion with some extra DCE, better than the original
        // loop fusion because as actually goes away once it becomes dead

        val as = array(200) { i => 1.0 }
        val bs = array(200) { i => 2.0 }
        val cs = array(100) { i => 
          array(i) { j => as.foo }
        }
        val ds = array(100) { i => 
          val dx = cs.at(i)
          array(i) { j => dx.at(j) - as.foo }
          // this will become as.foo - as.foo = 0  -->  i.e. as becomes dead but is already fused with bs, which is used...
        }

        print(bs)
        print(ds)
      }
    }
    new Prog with ImplArith
  }
}
