package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.{OverloadHack, GraphUtil}
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait ArrayLoopsExpFixes extends ArrayLoopsExp {
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s, i, body) => 
      // First process body, otherwise shape or index could take onceSubst
      val newBody = mirrorFatDef(body, f)
      simpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], newBody)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayElem(y) => ArrayElem(reifyEffects(f.reflectBlock(y)))
    case ReduceElem(y) => ReduceElem(reifyEffects(f.reflectBlock(y)))
    case ArrayIfElem(c,y) =>
      // Cheating, need to process body first, otherwise condition takes
      // onceSubst of body if it's a common subexpression
      val body = reifyEffects(f.reflectBlock(y))
      ArrayIfElem(f.reflectBlock(Block(c)),body)
    case FlattenElem(y) => FlattenElem(reifyEffects(f.reflectBlock(y)))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]

  case class EmptyArray[T]() extends Def[Array[T]]
  def emptyArray[T:Manifest](): Rep[Array[T]] = EmptyArray[T]()
}

trait ScalaGenArrayLoopsFatFixes extends ScalaGenArrayLoopsFat {
  val IR: ArrayLoopsFatExp with ArrayLoopsExpFixes
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@EmptyArray() => 
      stream.println("var " + quote(sym) + " = new " + sym.tp + "(0)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ArrayLoopFusionExtractors extends ArrayLoopsExpFixes with LoopFusionExtractors {

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }

  override def unapplySimpleDomain(e: Def[Any]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }


  override def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = e match {
    case SimpleLoop(s, _, ArrayElem(_)) => Some(s)
    case _ => super.unapplyFixedDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Block(a)) => Some(a) //TODO: block??
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,Block(a)) => Some((a,List(c))) //TODO: block?
    case _ => super.unapplySimpleCollectIf(e)
  }

  override def unapplyReduce[T](e: Def[T]) = e match {
    case ReduceElem(Block(a: Exp[Double])) => // ReduceElem is sum of doubles...
      Some((a.asInstanceOf[Exp[T]], Some(unit(0.0).asInstanceOf[Exp[T]]), Some(true)))
    // what about ReduceIfElem?
    case _ => super.unapplyReduce(e)
  }

  override def unapplyMultiCollect[T](e: Def[T]) = e match {
    case FlattenElem(Block(a: Exp[T])) => Some((a.asInstanceOf[Exp[T]], Some((() => {
        emptyArray()(mtype(a.tp.typeArguments(0)))
      }).asInstanceOf[() => Exp[T]])))
    case _ => super.unapplyMultiCollect(e)
  }

}

trait MyFusionProg extends NumericOps with PrimitiveOps with LiftNumeric with OrderingOps 
    with BooleanOps with ArrayLoops with Print {
  def test(x: Rep[Int]): Rep[Unit]
}

trait Impl extends MyFusionProg with NumericOpsExp with PrimitiveOpsExp with ArrayLoopsFatExp with PrintExp
    with IfThenElseFatExp with OrderingOpsExp with BooleanOpsExp with ArrayLoopFusionExtractors 
    with LoopFusionCore2 { self =>
  override val verbosity = 2 // 1: only printlog, 2: also printdbg
  val runner = new Runner { val p: self.type = self }
  runner.run()
}

trait Codegen extends ScalaGenNumericOps with ScalaGenPrimitiveOps
  with ScalaGenPrint with ScalaGenOrderingOps with ScalaGenIfThenElse
  with ScalaGenBooleanOps with ScalaGenArrayLoopsFatFixes { val IR: Impl }

trait FusionCodegen extends Codegen with LoopFusionSchedulingOpt { val IR: Impl }


trait Runner /*extends internal.ScalaCompile*/ {
  val p: Impl
  def run() = {
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new Codegen { val IR: p.type = p }
    val fusionCodegen = new FusionCodegen { val IR: p.type = p }

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
      println("\n(VFT) all vertically fused: " + vFused.values.toList.distinct.mkString("\n"))

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

      fusionCodegen.setFusedSyms(hFused)
      fusionCodegen.withStream(new PrintWriter(System.out)) {
        fusionCodegen.emitBlock(h)
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



// =========================================
//                  Tests
// =========================================


class TestFusion3 extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-wip-"

  def testFusionTransform00 = withOutFileChecked(prefix+"fusion00") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // and range not dce'd, hor.
        val range = array(100) { i => i + 1 }
        val odds = array(range.length) { i => range.at(i) + 1 }
        print(range.at(0))
        print(odds.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform01 = withOutFileChecked(prefix+"fusion01") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // and range dce'd
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val odds = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          val y = !x
          (y, range.at(i) + 2) }
        print(odds.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform02 = withOutFileChecked(prefix+"fusion02") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // but range kept around, hor.
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val odds = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          val y = !x
          (y, range.at(i) + 2) }
        print(odds.length)
        print(range.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform03 = withOutFileChecked(prefix+"fusion03") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer, shape replaced so range is dce'd
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val more = arrayIf(range.length) { i => (i > 50, i > 60) }
        print(more.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform04 = withOutFileChecked(prefix+"fusion04") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // length moved out and replaced with constant, so is consumer, hor.
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val arr2 = array(range.length) { i => range.at(i) + range.length }
        print(arr2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform05 = withOutFileChecked(prefix+"fusion05") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer, constant replaced
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val arr3 = array(range.length) { i => range.at(i + 1) }
        print(arr3.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform06 = withOutFileChecked(prefix+"fusion06") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer and arr2 is also consumer of range
        // multiple indep consumers fused, range dce'd, hor.
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          (x, range.at(i) * 2) }
        val arr2 = arrayIf(range.length) { i =>
          (range.at(i) < 20, range.at(i) * 3) }
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform07 = withOutFileChecked(prefix+"fusion07") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer and arr2 is also consumer of range
        // multiple indep consumers fused, all hor.
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          (x, range.at(i) * 2) }
        val arr2 = arrayIf(range.length) { i =>
          (range.at(i) < 20, range.at(i) * 3) }
        print(range.at(0))
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform08 = withOutFileChecked(prefix+"fusion08") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // successive consumers fused     
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = array(range.length) { i =>
          val x = range.at(i) * 4
          x * 2 }
        val arr2 = arrayIf(arr1.length) { i =>
          val x = arr1.at(i) > 20
          (x, arr1.at(i) * 3) }
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform09 = withOutFileChecked(prefix+"fusion09") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // SimpleCollect + MultiCollect: inner loop not fused
        val range = array(100) { i => i + 1 }
        val range4 = flatten(range.length) { i => array(range.length) { ii => ii + range.at(i) } }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform10 = withOutFileChecked(prefix+"fusion10") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {     
        // nested array arrI not fused with range
        // arrI & arrJ fused
        // arrI dce'd, no hor. because arr1 deps on range
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = array(100) { i =>
          val x = i * 4
          val arrI = array(range.length) { ii =>
            range.at(ii) * 5 + i
          }
          val arrJ = array(arrI.length) { i =>
            arrI.at(i) * 6
          }

          x * arrJ.at(0)
        }
        print(arr1.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform11 = withOutFileChecked(prefix+"fusion11") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // no horizontal fusion across levels
        val arr1 = array(90) { i =>
          val arrI = array(100) { ii =>
            ii + i
          }
          arrI.at(0)
        }
        val arr2 = array(100) { i =>
          i + 2
        }
        print(arr1.at(0))
        print(arr2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform12 = withOutFileChecked(prefix+"fusion12") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {    
        // horizontal fusion in nested loop (not vertical since not consumer)
        val arr1 = array(100) { i =>
          val arrI = array(80) { ii =>
            ii + i
          }
          val arrJ = array(80) { ii =>
            ii * i
          }
          i * arrI.at(0) * arrJ.at(0)
        }
        print(arr1.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform13 = withOutFileChecked(prefix+"fusion13") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // reconstructed consumers fused, no hor.
        // constant
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) + 2 }
        print(range2.at(0))
        // immutable sym
        val l = x + 5
        val range3 = array(l) { i => i + 3 }
        val range4 = array(l) { i => range3.at(i) + 4 }
        print(range4.at(0))
        // TODO test other cases (subst)?
      }
    }
    new Prog with Impl
  }

  def testFusionTransform14 = withOutFileChecked(prefix+"fusion14") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple producers (zip): fuse with real and then 1st reconstructed
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = array(range2.length) { i => range.at(i) + range2.at(i) }
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform15 = withOutFileChecked(prefix+"fusion15") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // multiple producers (zip): fuse with first reconstr. and then others
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = array(100) { i => range.at(i) + range2.at(i) }
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform16 = withOutFileChecked(prefix+"fusion16") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // Test all branches of consumerGoesOverRangeOfProducer
        val range = array(x) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) } 
        print(range2.length)
        val k = x + 2
        val range3 = array(k) { i => range.at(i) }            
        val range4 = array(k) { i => range3.at(i) }             
        print(range4.length)
        
        val rb = array(100) { i => i + 1 }
        val rb2 = array(rb.length) { i => rb.at(i) }          
        val rb3 = array(rb.length) { i => rb2.at(i) }         
        val rb4 = array(90) { i => rb.at(i) }                 
        val rb5 = array(rb.length) { i => rb4.at(i) }         
        val rb6 = array(100) { i => rb.at(i) }                
        print(rb3.at(1))
        print(rb5.at(1))
        print(rb6.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform17 = withOutFileChecked(prefix+"fusion17") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        // not consumer because of v (depends on producer), no hor.
        val range2 = arrayIf(range.length) { i => (range.at(i) > 0, range.at(i) + v) }
        print(range2.at(0))
        // not consumer because of v (depends on producer), hor. with range2
        val range3 = arrayIf(range.length) { i => (range.at(i) + v > 0, i) }
        print(range3.at(0))
        // not consumer because range3 uses index, no hor.
        val range4 = array(range2.length) { i => range2.at(i) + i }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

// TODO fix effects

/*
  def testFusionTransform18 = withOutFileChecked(prefix+"fusion18") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {    
        // TODO effects    
        val range = array(x) { i => i + 1 }
        print(range.length)
        val range2 = array(range.length) { i => print(range.at(i)); 1 } 
        // no  vertical (or hor.) fusion because of ordering of effects
        print(range2.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform19 = withOutFileChecked(prefix+"fusion19") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(100) { i => i + 1 }
        print(range.at(0))
        val range2 = array(100) { i => print(i); 1 } 
        // no  hor fusion because of ordering of effects
        print(range2.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform20 = withOutFileChecked(prefix+"fusion20") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // TODO AAAAHHHH effects of loops aren't tracked properly! range and print(y) get swapped!      
        val y = x + 1
        val range = array(100) { i => print(i); i }
        print(y)
        print(range.at(0))
      }
    }
    new Prog with Impl
  }
*/

  def testFusionTransform21 = withOutFileChecked(prefix+"fusion21") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple consumers all fused, range dce'd, 3 remaining hor. fused
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) + 2 }
        val range3 = array(100) { i => range.at(i) + 3 }
        val range4 = array(range.length) { i => range.at(i) + 4 }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform22 = withOutFileChecked(prefix+"fusion22") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple consumers not fused:
        // range4 depends on range3, but range already fused with range3,
        // so range4 not fused with range
        // range2&3 cse'd, range4 deps on range, so no hor.
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) }  // fuse
        val range3 = array(100) { i => range2.at(i) } // fuse
        val range4 = array(100) { i => range.at(i) + range3.at(0) } // don't fuse

        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  // Test Vertical Fusion: loop type combinations

  def testFusionTransform23 = withOutFileChecked(prefix+"fusion23") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SimpleCollect + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = array(100) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = arrayIf(range.length) { i => (i > 10, range.at(i) + 3) }
        val range4 = flatten(range.length) { i => array(10) { ii => ii + range.at(i) } }
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform24 = withOutFileChecked(prefix+"fusion24") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SimpleCollectIf + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = arrayIf(100) { i => (i > 10, i + 1) }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) }
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => ii + range.at(i) } }
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform25 = withOutFileChecked(prefix+"fusion25") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(SC) + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform26 = withOutFileChecked(prefix+"fusion26") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SCIf + SC with common subexpression between condition&body
        // range dce'd, no hor.
        val range = arrayIf(10) { ii => (1+ii > 5, 1+ii) }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform27 = withOutFileChecked(prefix+"fusion27") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(SCIf) + Any fusion
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => arrayIf(10) { ii => (i+ii > 5, i+ii) } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform28 = withOutFileChecked(prefix+"fusion28") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(MC) + Any fusion
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => flatten(10) { ii => array(5) {iii => i+ii+iii} } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform29 = withOutFileChecked(prefix+"fusion29") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(constant array SCIf) + Any fusion
        // range dce'd, hor. fuse MC-inner with other inner, hor. fuse outer
        val range = flatten(30) { i => arrayIf(10) { ii => (ii > 5, ii+1) } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }


  // Test Horizontal Fusion Prep Pass

  def testFusionTransform30 = withOutFileChecked(prefix+"fusion30") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // range survives DCE, so hor. fusion with odds
        // rangeH not fused with range since in conflict with odds
        val range = array(100) { i => i + 1 }
        print(range.at(0))
        val rangeH = array(100) { i => i + 2 }
        print(rangeH.at(0))
        val odds = arrayIf(range.length) { i => (range.at(i) > 50, rangeH.at(0) + 1) }
        print(odds.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform31 = withOutFileChecked(prefix+"fusion31") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        // not consumer because of v (depends on producer), no hor.
        val range2 = arrayIf(range.length) { i => (range.at(i) > 0, range.at(i) + v) }
        print(range2.at(0))
        // not consumer because of v (depends on producer), hor. with range2
        // new in subst
        val range3 = arrayIf(range.length) { i => (range.at(i) + v > 0, i) }
        print(range3.at(0))
        // consumer of range3, hor. with range2&3
        // subst already has it
        val range4 = array(range3.length) { i => range3.at(i) + 2 }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform32 = withOutFileChecked(prefix+"fusion32") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // regression test for bug when pBody is index, 
        // onceSubst was eaten by index transformation. Fuse vert. & hor.
        // TODO are there other cases when onceSubst expression is used too early?
        val range = arrayIf(x) { i => (i + 10 > 0, i) }
        print(range.at(0))
        val range2 = array(range.length) { i => range.at(i) + 2 }
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform33 = withOutFileChecked(prefix+"fusion33") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // Constant array, successive works, but would like to yield directly to output
        val range = flatten(30) { i => arrayIf(10) { ii => (ii > 5, ii+1) } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range2.length) { i => (range2.at(i) > 20, range2.at(i) + 3) } // SCIf

        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform34 = withOutFileChecked(prefix+"fusion34") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // fuse two MC and then successive SC consumer, vert. & hor.
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range3 = array(range2.length) { i => range2.at(i) + 4 }
        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform35 = withOutFileChecked(prefix+"fusion35") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // Fusion happens correctly inside of MC, even when loop is
        // both consumer and producer.
        // Successively fuse all three loops, with existing and generated inner ones.
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = flatten(range.length) { i => 
          val inner = array(range.at(i)) { ii => range.at(i) + ii } 
          array(inner.length) { ii => inner.at(ii) + 3 } 
        }
        val range3 = array(range2.length) { i => range2.at(i) + 4 }
        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform36 = withOutFileChecked(prefix+"fusion36") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // No vert., fuse outer horizontally and then fuse inner horizontally
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = flatten(30) { i => array(10) { ii => i+ii+2 } } 
        val range3 = flatten(30) { i => array(10) { ii => i+ii+3 } } 

        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform37 = withOutFileChecked(prefix+"fusion37") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // No vert., fuse outer horizontally and then fuse inner horizontally
        val range = array(30) { i => 
          val inner1 = array(10) { ii => i+ii }
          inner1.at(0)
        } 
        val range2 = array(30) { i => 
          val inner2 = array(10) { ii => i+ii+2 }
          inner2.at(0)
        }
        val range3 = array(30) { i => 
          val inner3 = array(10) { ii => i+ii+3 }
          inner3.at(0)
        } 

        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform38 = withOutFileChecked(prefix+"fusion38") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // indirect dependency
        // vert. fuse a&b, want to fuse c with range, but c depends on
        // a which is fused with b which depends on range, so can't!
        // hor. only a&b as well

        val range = array(30) { i => i }
        val a = array(20) { i => i * 2 }
        val b = array(a.length) { i => a.at(i) + range.at(0) }
        val c = array(range.length) { i => range.at(i) + a.at(0) }

        print(b.at(0))
        print(c.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform39 = withOutFileChecked(prefix+"fusion39") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // vert. fuse range4 with multiple producers range and range2
        // horizontally fuse range3 and range4'
        // range5 no fusion because producer is IF
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = arrayIf(100) { i => (i > 10, i + 3) }
        val range4 = array(range2.length) { i => range.at(i) + range2.at(i) }
        val range5 = array(100) { i => range3.at(i) }
        print(range3.at(0))
        print(range4.at(0))
        print(range5.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform40 = withOutFileChecked(prefix+"fusion40") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple producers (zip): no problem fusing others of same shape
        // vert. fuse range4 with multiple range and range2
        // vert. fuse range5 with range3 -> same expression, CSE'd
        // hor. fuse range4 & range3/5
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = array(100) { i => i + 3 }
        val range4 = array(range2.length) { i => range.at(i) + range2.at(i) }
        val range5 = array(100) { i => range3.at(i) }
        print(range3.at(0))
        print(range4.at(0))
        print(range5.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform41 = withOutFileChecked(prefix+"fusion41") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple producers (zip): combination of existing fusion sets works
        val range = array(100) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) + 2 }

        val range3 = array(100) { i => i + 3 }
        val range4 = array(range3.length) { i => range3.at(i) + 4 }

        val range5 = array(100) { i => range2.at(i) + range4.at(i) }
        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform42 = withOutFileChecked(prefix+"fusion42") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple producers (zip): combination of existing fusion sets works.
        // and indirect dependencies through fusion sets prevent fusion of
        // rangeConflict (all others fused)
        val range = array(100) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) + 2 }

        val range3 = array(100) { i => i + 3 }
        val range4 = array(range3.length) { i => range3.at(i) + 4 }

        val range5 = array(100) { i => range2.at(i) + range4.at(i) }
        val rangeConflict = array(range3.length) {i => range3.at(i) + range.at(0)}
        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5.at(0))
        print(rangeConflict.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform43 = withOutFileChecked(prefix+"fusion43") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // vert. fuse prod2 with range, cons with prod1&prod2
        // conflict not fused with range because range fused with prod1, but dep. on prod1
        // hor. fuse prod1, range, prod2, cons; don't fuse conflict 
        val prod1 = array(100) { i => i + 1 }
        val range = array(100) { i => i + 2 }
        val prod2 = array(range.length) { i => range.at(i) + 3 }
        val cons = array(100) { i => prod1.at(i) + prod2.at(i) }
        val conflict = array(range.length) { i => range.at(i) + 4 + prod1.at(0) }
        print(cons.at(0))
        print(conflict.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform44 = withOutFileChecked(prefix+"fusion44") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // vert. fuse prod2 with range, conflict with range
        // cons can only be fused with prod2 then, because prod2's fusion set depends on
        // prod1 through conflict.
        // hor. fuse cons and conflict, they both depend on prod1. prod2&range dce'd.
        val prod1 = array(100) { i => i + 1 }
        val range = array(100) { i => i + 2 }
        val prod2 = array(range.length) { i => range.at(i) + 3 }
        val conflict = array(range.length) { i => range.at(i) + 4 + prod1.at(0) }
        print(conflict.at(0))
        val cons = array(100) { i => prod1.at(i) + prod2.at(i) }
        print(cons.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform45 = withOutFileChecked(prefix+"fusion45") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // fuse with remapped on lower level
        val range = array(100) { i =>
          val inner1 = array(100) { j => j + 1 + i }
          val inner2 = array(100) { j => j + 2 + i }
          val innerC = array(inner2.length) { j => inner2.at(j) + 3 }
          inner1.at(0) +  inner2.at(0) + innerC.at(0)
        }
        print(range.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform46 = withOutFileChecked(prefix+"fusion46") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // range's inner is fused into range2, was emitted twice from
        // horizontal (because there are two copies in separate scopes)
        // but TTP only needs one copy since it fused the scopes
        // TODO successive index substitution
        val range = array(100) { i => array(i) { j => 1 } }
        val range2 = array(100) { i => range.at(i).at(i) }
        print(range.at(0))
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform47 = withOutFileChecked(prefix+"fusion47") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // successive arrayIndex
        // range.at(i) is substituted to inner of range and inner of range
        // is fused with inner of range2, so that range2 is cse'd
        // since it is identical to range
        val range = array(100) { i => array(i) { j => 1 } }
        val range2 = array(100) { i => 
          array(i) { j => range.at(i).at(j) }
        }
        print(range.at(0))
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform48 = withOutFileChecked(prefix+"fusion48") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // vert. fuse range2 & range3, range & range4, but cannot fuse range4 with range2
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = array(range2.length) { i => range2.at(i) + 3 }
        val range4 = array(range.length) { i => range.at(i) + range2.at(i) + range2.at(0) }

        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }
/*
  def testFusionTransform49 = withOutFileChecked(prefix+"fusion49") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // TODO
        // Once range has been fused with range3, the inner array
        // becomes independent of the loop index i and could be moved
        // to the top level and there fused with range2...
        val range = array(100) { i => 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = array(range.length) { i => 
          array(100) { j => range2.at(j) + range.at(i) } 
        }

        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform50 = withOutFileChecked(prefix+"fusion50") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // TODO
        // range's inner is fused into range2 and then successively fused
        val range = array(100) { i => array(i) { j => 1 } }
        val range2 = array(100) { i => range.at(i).at(i) }
        print(range.at(0))
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }
*/

  // TODO MC inserts SI on inner loop, problem with successive detection?

}

