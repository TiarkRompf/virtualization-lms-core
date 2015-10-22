package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.{OverloadHack, GraphUtil}
import org.scala_lang.virtualized.SourceContext
import org.scala_lang.virtualized.virtualize

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait MyFusionProg extends NumericOps with PrimitiveOps with LiftNumeric with OrderingOps 
    with BooleanOps with ArrayLoopsMC with Print with PrintX with While with Variables with LiftVariables {
  def test(x: Rep[Int]): Rep[Unit]
}

trait Impl extends MyFusionProg with NumericOpsExp with PrimitiveOpsExp with ArrayLoopsMCFatExp with PrintExp
    with IfThenElseFatExp with OrderingOpsExp with BooleanOpsExp with ArrayLoopsMCFusionExtractors
    with ArrayLoopsMCExp with LoopFusionCore with WhileExp with VariablesExp with PrintXExp { self =>
  override val verbosity = 2 // 1: only printlog, 2: also printdbg
  val runner = new Runner { val p: self.type = self }
  runner.run()
}

trait Codegen extends ScalaGenNumericOps with ScalaGenPrimitiveOps
  with ScalaGenPrint with ScalaGenPrintX with ScalaGenOrderingOps with ScalaGenIfThenElseFat
  with ScalaGenBooleanOps with ScalaGenArrayLoopsMCFat
  with ScalaGenWhile with ScalaGenVariables { val IR: Impl }

trait FusionCodegen extends Codegen with CombineTTPScheduling { val IR: Impl }


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

//    override def toString() = "Sym(" + id + ": " + tp + ")"
    val printgraph = false
    try {
      println("\n-- vertical transformation")

      val v = verticalTransf.transformBlock(y)
      val vFused = verticalTransf.FusedSyms.getFusedSyms
      println("\n(VFT) all vertically fused: " + vFused.values.toList.distinct.map(_._1).mkString("\n"))

      println("\n-- after vertical transformation")
      if (printgraph) {
        println("-- full graph")
        p.globalDefs foreach println
      }
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(v)
      }


      println("\n-- horizontal transformation")
      val h = horTransf.transformBlock(v)

      val hFused = horTransf.AllFusionScopes.get
      println("\n(HFT) all horizontally fused: " + hFused.mkString("\n"))


      println("\n-- after horizontal transformation")
      if (printgraph) {
        println("-- full graph")
        p.globalDefs foreach println      
      }
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(h)
      }

      println("\n-- fusion")

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

trait PrintX extends Base {
  def printX[T:Manifest](s: Rep[T]): Rep[T]
}

trait PrintXExp extends Print with EffectExp {
  case class PrintX[T:Manifest](s: Rep[T]) extends Def[T]
  def printX[T:Manifest](s: Rep[T]) = reflectEffect(PrintX(s))
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case PrintX(s) => PrintX(f(s))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(PrintX(s), u, es) => reflectMirrored(Reflect(PrintX(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenPrintX extends ScalaGenEffect {
  val IR: PrintXExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintX(s) =>  emitValDef(fresh[Unit], "println(" + quote(s) + ")")
      emitValDef(sym, quote(s))
    case _ => super.emitNode(sym, rhs)
  }
}


// =========================================
//                  Tests
// =========================================

@virtualize
class TestFusion3 extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-wip-"
  val suffix = ""//".check"

  def testFusionTransform00 = withOutFileChecked(prefix+"fusion00"+suffix) {
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

  def testFusionTransform01 = withOutFileChecked(prefix+"fusion01"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {  
        // range is producer, odds is consumer, range fused into odds
        // and range dce'd
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val odds = arrayIf(range.length)(
          { i => val x = range.at(i) > 50; !x },
          { i => range.at(i) + 2 })
        print(odds.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform02 = withOutFileChecked(prefix+"fusion02"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // odds not consumer because not full length of range
        // no dce, no vert., no hor.
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val odds = arrayIf(10)(
          { i => val x = range.at(i) > 50; !x },
          { i => range.at(i) + 2 })
        print(odds.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform03 = withOutFileChecked(prefix+"fusion03"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer, shape replaced so range is dce'd
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val more = arrayIf(range.length)({ i => i > 50}, { i => i > 60 })
        print(more.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform04 = withOutFileChecked(prefix+"fusion04"+suffix) {
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

  def testFusionTransform05 = withOutFileChecked(prefix+"fusion05"+suffix) {
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

  def testFusionTransform06 = withOutFileChecked(prefix+"fusion06"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer and arr2 is also consumer of range
        // multiple indep consumers fused, range dce'd, hor.
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = arrayIf(range.length)(
          { i => range.at(i) > 50 },
          { i => range.at(i) * 2 })
        val arr2 = arrayIf(range.length)(
          { i => range.at(i) < 20 }, 
          { i => range.at(i) * 3 })
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform07 = withOutFileChecked(prefix+"fusion07"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer and arr2 is also consumer of range
        // multiple indep consumers fused, all hor.
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = arrayIf(range.length)(
          { i => range.at(i) > 50 },
          { i => range.at(i) * 2 })
        val arr2 = arrayIf(range.length)(
          { i => range.at(i) < 20 }, { i => range.at(i) * 3 })
        print(range.at(0))
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }  

  def testFusionTransform08 = withOutFileChecked(prefix+"fusion08"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // successive consumers fused     
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = array(range.length) { i =>
          val x = range.at(i) * 4
          x * 2 }
        val arr2 = arrayIf(arr1.length)(
          { i => arr1.at(i) > 20}, { i => arr1.at(i) * 3 })
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform09 = withOutFileChecked(prefix+"fusion09"+suffix) {
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

  def testFusionTransform10 = withOutFileChecked(prefix+"fusion10"+suffix) {
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

  def testFusionTransform11 = withOutFileChecked(prefix+"fusion11"+suffix) {
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

  def testFusionTransform12 = withOutFileChecked(prefix+"fusion12"+suffix) {
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

  def testFusionTransform13 = withOutFileChecked(prefix+"fusion13"+suffix) {
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

  def testFusionTransform14 = withOutFileChecked(prefix+"fusion14"+suffix) {
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

  def testFusionTransform15 = withOutFileChecked(prefix+"fusion15"+suffix) {
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

  def testFusionTransform16 = withOutFileChecked(prefix+"fusion16"+suffix) {
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

  def testFusionTransform17 = withOutFileChecked(prefix+"fusion17"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        // not consumer because of v (depends on producer), no hor.
        val range2 = arrayIf(range.length)(
          { i => range.at(i) > 0}, { i => range.at(i) + v })
        print(range2.at(0))
        // not consumer because of v (depends on producer), hor. with range2
        val range3 = arrayIf(range.length)(
          { i => range.at(i) + v > 0 }, { i => i })
        print(range3.at(0))
        // not consumer because range3 uses index, no hor.
        val range4 = array(range2.length) { i => range2.at(i) + i }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform18 = withOutFileChecked(prefix+"fusion18"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {    
        // no  vertical (or hor.) fusion of range2 and range because
        // range2's effects happen-after the print depending on range
        // range3 not fused and cannot be moved because of effects
        val range = array(x) { i => i + 1 }
        print(range.at(0))
        val range2 = array(range.length) { i => print(range.at(i)); i + 45 } 
        val range3 = array(10) { i => print(i); i }
        print(range2.at(1))
        print(range3.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform19 = withOutFileChecked(prefix+"fusion19"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {   
        // no hor. fusion because of effect dependencies     
        val range = array(100) { i => i + 1 }
        print(range.at(0))
        val range2 = array(100) { i => print(i); 1 } 
        print(range2.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform20 = withOutFileChecked(prefix+"fusion20"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // don't reorder loops, no fusion
        val y = x + 1
        val range = array(100) { i => print(i); i }
        val range2 = array(200) { i => print(range.at(0)); i }
        print(range2.at(0))
        print(y)
        print(range.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform21 = withOutFileChecked(prefix+"fusion21"+suffix) {
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

  def testFusionTransform22 = withOutFileChecked(prefix+"fusion22"+suffix) {
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

  def testFusionTransform23 = withOutFileChecked(prefix+"fusion23"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SimpleCollect + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = array(100) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = arrayIf(range.length)(
          { i => i > 10 }, { i => range.at(i) + 3 })
        val range4 = flatten(range.length) { i => array(10) { ii => ii + range.at(i) } }
        val range5 = sum(range.length) { i => 2 + range.at(i) }
        val range6 = reduce[Int](range.length)(
          { i => array(2) { ii => range.at(i) + ii } },
          { (a,b) => a + b }, 0)
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
        print(range6)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform24 = withOutFileChecked(prefix+"fusion24"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SimpleCollectIf + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = arrayIf(100)({ i => i > 10 }, { i => i + 1 })
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = arrayIf(range.length)(
           { i => range.at(i) > 20 }, { i => range.at(i) + 3 })
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => ii + range.at(i) } }
        val range5 = sum(range.length) { i => 2 + range.at(i) }
        val range6 = reduce[Int](range.length)(
          { i => array(2) { ii => range.at(i) + ii } },
          { (a,b) => a + b }, 0)
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
        print(range6)

      }
    }
    new Prog with Impl
  }

  def testFusionTransform25 = withOutFileChecked(prefix+"fusion25"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(SC) + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length)(
          { i => range.at(i) > 20 }, { i => range.at(i) + 3 }) // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => 2 + range.at(i) } // R
        val range6 = reduce[Int](range.length)(
          { i => array(2) { ii => range.at(i) + ii } },
          { (a,b) => a + b }, 0)
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
        print(range6)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform26 = withOutFileChecked(prefix+"fusion26"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SCIf + SC with common subexpression between condition&body
        // range dce'd, no hor.
        val range = arrayIf(10)(
          { ii => 1+ii > 5 },
          { ii => 1+ii })
        val range2 = array(range.length) { i => range.at(i) + 2 }
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform27 = withOutFileChecked(prefix+"fusion27"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(SCIf) + Any fusion
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => arrayIf(10)(
          { ii => i+ii > 5 },
          { ii => i+ii }) } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length)(
          { i => range.at(i) > 20 },
          { i => range.at(i) + 3 }) // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => 2 + range.at(i) } // R
        val range6 = reduce[Int](range.length)(
          { i => array(2) { ii => range.at(i) + ii } },
          { (a,b) => a + b }, 0)
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
        print(range6)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform28 = withOutFileChecked(prefix+"fusion28"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(MC) + Any fusion
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => flatten(10) { ii => array(5) {iii => i+ii+iii} } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length)(
          { i => range.at(i) > 20 },
          { i => range.at(i) + 3 }) // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => 2 + range.at(i) } // R
        val range6 = reduce[Int](range.length)(
          { i => array(2) { ii => range.at(i) + ii } },
          { (a,b) => a + b }, 0)
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
        print(range6)
      }
    }
    new Prog with Impl
  }

  // TODO no array out of bounds exception
  def testFusionTransform29 = withOutFileChecked(prefix+"fusion29"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(constant array SCIf) + Any fusion
        // range dce'd, hor. fuse MC-inner with other inner, hor. fuse outer
        val range = flatten(30) { i => arrayIf(10)(
          { ii => ii > 5 },
          { ii => ii+1 }) } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length)(
          { i => range.at(i) > 20 },
          { i => range.at(i) + 3 }) // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => 2 + range.at(i) } // R
        val range6 = reduce[Int](range.length)(
          { i => array(2) { ii => range.at(i) + ii } },
          { (a,b) => a + b }, 0)
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
        print(range6)
      }
    }
    new Prog with Impl
  }

  // Test Horizontal Fusion Prep Pass

  def testFusionTransform30 = withOutFileChecked(prefix+"fusion30"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // range survives DCE, so hor. fusion with odds
        // rangeH not fused with range since in conflict with odds
        val range = array(100) { i => i + 1 }
        print(range.at(0))
        val rangeH = array(100) { i => i + 2 }
        print(rangeH.at(0))
        val odds = arrayIf(range.length)(
          { i => range.at(i) > 50 },
          { i => rangeH.at(0) + 1 })
        print(odds.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform31 = withOutFileChecked(prefix+"fusion31"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        // not consumer because of v (depends on producer), no hor.
        val range2 = arrayIf(range.length)(
          { i => range.at(i) > 0 },
          { i => range.at(i) + v })
        print(range2.at(0))
        // not consumer because of v (depends on producer), hor. with range2
        // new in subst
        val range3 = arrayIf(range.length)(
          { i => range.at(i) + v > 0 },
          { i => i })
        print(range3.at(0))
        // consumer of range3, hor. with range2&3
        // subst already has it
        val range4 = array(range3.length) { i => range3.at(i) + 2 }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform32 = withOutFileChecked(prefix+"fusion32"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // regression test for bug when pBody is index, 
        // onceSubst was eaten by index transformation. Fuse vert. & hor.
        // TODO are there other cases when onceSubst expression is used too early?
        val range = arrayIf(x)(
          { i => i + 10 > 0 },
          { i => i })
        print(range.at(0))
        val range2 = array(range.length) { i => range.at(i) + 2 }
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  // TODO no array out of bounds exception
  def testFusionTransform33 = withOutFileChecked(prefix+"fusion33"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // Constant array, successive works, but would like to yield directly to output
        val range = flatten(30) { i => arrayIf(10)(
          { ii => ii > 5 },
          { ii => ii+1 }) } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range2.length)(
          { i => range2.at(i) > 20 },
          { i => range2.at(i) + 3 }) // SCIf

        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform34 = withOutFileChecked(prefix+"fusion34"+suffix) {
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

  def testFusionTransform35 = withOutFileChecked(prefix+"fusion35"+suffix) {
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

  def testFusionTransform36 = withOutFileChecked(prefix+"fusion36"+suffix) {
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

  def testFusionTransform37 = withOutFileChecked(prefix+"fusion37"+suffix) {
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

  def testFusionTransform38 = withOutFileChecked(prefix+"fusion38"+suffix) {
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

  def testFusionTransform39 = withOutFileChecked(prefix+"fusion39"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // vert. fuse range4 with multiple producers range and range2
        // horizontally fuse range3 and range4'
        // range5 no fusion because producer is IF
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = arrayIf(100)(
          { i => i > 10 },
          { i => i + 3 })
        val range4 = array(range2.length) { i => range.at(i) + range2.at(i) }
        val range5 = array(100) { i => range3.at(i) }
        print(range3.at(0))
        print(range4.at(0))
        print(range5.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform40 = withOutFileChecked(prefix+"fusion40"+suffix) {
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

  def testFusionTransform41 = withOutFileChecked(prefix+"fusion41"+suffix) {
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

  def testFusionTransform42 = withOutFileChecked(prefix+"fusion42"+suffix) {
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

  def testFusionTransform43 = withOutFileChecked(prefix+"fusion43"+suffix) {
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

  def testFusionTransform44 = withOutFileChecked(prefix+"fusion44"+suffix) {
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

  def testFusionTransform45 = withOutFileChecked(prefix+"fusion45"+suffix) {
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

  def testFusionTransform46 = withOutFileChecked(prefix+"fusion46"+suffix) {
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

/* // TODO simulated fusion
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
  */

  def testFusionTransform48 = withOutFileChecked(prefix+"fusion48"+suffix) {
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
/*// TODO MC inserts SI on inner loop, problem with successive detection?

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

  def testFusionTransform51 = withOutFileChecked(prefix+"fusion51"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      // Fuse effectful foreach, removes intermediate array allocation
      def test(x: Rep[Int]) = {
        print(1)
        var y = 0
        val range = array(100) { i => i + 2 }
        range.foreach({ x: Rep[Int] => y = x; print(x + 3) })
        print(4)
        print(y)
      }
    }
    new Prog with Impl
  }

/* // TODO MC has Reflect(Singleton)  
  def testFusionTransform52 = withOutFileChecked(prefix+"fusion52") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // Fuse effectful producer and pure consumer
        print(1)
        val range = array(100) { i => print(i); i + 2 }
        print(3)
        val range2 = array(range.length) { i => range.at(i) + 1 } 
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }*/

  def testFusionTransform53 = withOutFileChecked(prefix+"fusion53"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // fuse pure producer and effectful consumer
        print(1)
        val range = array(100) { i => i + 2 }
        print(3)
        val range2 = array(range.length) { i => print(range.at(i)); 4 } 
        print(range.at(0))
        print(range2.at(5))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform54 = withOutFileChecked(prefix+"fusion54"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // both effectful, but non-overlapping, still no fusion
        var y = unit(0)
        val range = array(100) { i => print(i); i + 2 }
        val range2 = array(range.length) { i => y += range.at(i); range.at(i) + 4 } 
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform55 = withOutFileChecked(prefix+"fusion55"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // both effectful, but non-overlapping, still no hor. fusion
        var y = unit(0)
        val range = array(100) { i => print(i); i + 2 }
        val range2 = array(100) { i => y += i; i + 4 } 
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform56 = withOutFileChecked(prefix+"fusion56"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // range and range3 vertically fused, effectful
        // so cannot hor. fuse range and range2
        // Note: after TTP fusion, the fused loop is scheduled before
        // range2. This is not a bug in loop fusion, rather the scheduler
        // by design doesn't maintain order between simple and read/write
        // effects (despite order in reify nodes).
        val range = array(100) { i => i + 1 }
        var y = unit(0)
        val range2 = array(100) { i => y += i; i + 2 }
        val range3 = array(range.length) { i => print(range.at(i)); i + 3} 
        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform57 = withOutFileChecked(prefix+"fusion57"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple consumers, only range & range2 fused because of
        // effects
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => print(i); range.at(i) + 2 }
        val range3 = array(100) { i => print(i); range.at(i) + 3 }
        val range4 = array(range.length) { i => print(i); range.at(i) + 4 }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }
/* // TODO effectful producers?
  def testFusionTransform58 = withOutFileChecked(prefix+"fusion58") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple producers (zip): fuse range&range2 (effectful),
        // fuse range3&range4 (effectful), so range5 only fused with
        // first fused loop, even though non-overlapping effects.
        // (Easy with overlapping effects because range4 will depend
        // on range2 and so will only fuse with range4).
        var y = 0
        val range = array(100) { i => y += i; i + 1 }
        val range3 = array(100) { i => print(3); i + 3 }

        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range4 = array(range3.length) { i => range3.at(i) + 4 }

        val range5 = array(100) { i => range2.at(i) + range4.at(i) }
        print(range2.at(0))
        print(range4.at(0))
        print(range5.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform59 = withOutFileChecked(prefix+"fusion59") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // fuse range&range2 (effectful), range3 not fused because fusion set
        // is effectful
        val range = array(100) { i => print(1); i + 1 }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = array(range2.length) { i => print(range2.at(i)); i + 3 }
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }  

  def testFusionTransform60 = withOutFileChecked(prefix+"fusion60") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // no fusion because printX has both effect and returns value, so
        // some producer effects would be pulled over into consumer
        val range = array(100) { i => printX(i) }
        val range2 = array(range.length) { i => range.at(i) + 1 }
        print(range.at(0))
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }
*/

  def testFusionTransform61 = withOutFileChecked(prefix+"fusion61"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      // Fuse MC with effectful foreach, removes intermediate array allocation
      def test(x: Rep[Int]) = {
        val range = flatten(100) {i => array(i) {j => j} }
        range.foreach({ x: Rep[Int] => print(x + 3) })
      }
    }
    new Prog with Impl
  }

  def testFusionTransform62 = withOutFileChecked(prefix+"fusion62"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // fuse pure producer and effectful consumer
        print(1)
        val range = flatten(5) { i => arrayIf(10)({j => j > 5}, {j => j + i + 1 }) }
        val range2 = flatten(range.length) { i => array(2) { j => print(range.at(i) + 2); 4 + j } } 
        print(range.at(0))
        print(range2.at(5))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform63 = withOutFileChecked(prefix+"fusion63"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // fuse empty and foreach -> Unit left
        val range = flatten(10) { i => emptyArray[Int]() }
        range.foreach({ i: Rep[Int] => print(i) })
        print(1)
        // fuse if-then xx-else empty with foreach
        val range2 = arrayIf(6)({ i => i > 2 }, { i => i + 3 })
        range2.foreach({ i: Rep[Int] => print(i + 4) })
        print(5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform64 = withOutFileChecked(prefix+"fusion64"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // fuse MC(if-then empty else any) with foreach
        val range = flatten(10) { i => if (i > 5) emptyArray[Int]() else array(3) { j => i + j } }
        range.foreach({ i: Rep[Int] => print(i) })
        print(1)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform65 = withOutFileChecked(prefix+"fusion65"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // fuse MC(if-then any else empty) with foreach
        val range = flatten(8) { i => if (i > 4) array(4) { j => i + j + 1 } else emptyArray[Int]() }
        range.foreach({ i: Rep[Int] => print(i) })
        print(2)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform66 = withOutFileChecked(prefix+"fusion66"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // don't fuse if-then-else because no branch empty
        val range = flatten(6) { i => if (i > 2) array(5) { j => i + j + 1 } else singleton(i + 2) }
        range.foreach({ i: Rep[Int] => print(i) })
        print(3)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform67 = withOutFileChecked(prefix+"fusion67"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // ManyMcsingle_For
        val range1 = array(10) { i => i + 1 }
        val range2 = array(10) { i => i + 2 }
        foreach2(10) { i => print(range1.at(i) + range2.at(i)) }
      }
    }
    new Prog with Impl
  }

  def testFusionTransform68 = withOutFileChecked(prefix+"fusion68"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // Fuse empty producer with all consumers. Reduce not fused.
        val range = emptyArray[Int]()
        val range1 = array(range.length) ({ i => range.at(i) + 1 })
        val range2 = range.foreach({ e: Rep[Int] => print(e + 1) })
        val y = reduce[Int](range.length) ({ i => singleton(range.at(i) + 1) },
          { (a, b) => a + b }, 0)
        print(range1.at(0))
        print(y)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform69 = withOutFileChecked(prefix+"fusion69"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // Fuse singleton producer with all consumers. Reduce not fused.
        val range = singleton(100)
        val range1 = array(range.length) ({ i => range.at(i) + 1 })
        val range2 = range.foreach({ e: Rep[Int] => print(e + 1) })
        val y = reduce[Int](range.length) ({ i => singleton(range.at(i) + 1) },
          { (a, b) => a + b }, 0)
        print(range1.at(0))
        print(y)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform70 = withOutFileChecked(prefix+"fusion70"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // Fuse if-then-else producer with all consumers. Reduce not fused.
        val range = if (x > 2) singleton(10) else emptyArray[Int]()
        val range1 = array(range.length) ({ i => range.at(i) + 1 })
        val range2 = range.foreach({ e: Rep[Int] => print(e + 1) })
        val y = reduce[Int](range.length) ({ i => singleton(range.at(i) + 1) },
          { (a, b) => a + b }, 0)
        print(y)
        print(range1.at(0))
      }
    }
    new Prog with Impl
  }
        
  def testFusionTransform71 = withOutFileChecked(prefix+"fusion71"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // in general don't fuse empty with reduce -> but here inner of MC fused with MC of
        // reduce, and then wrapped in reduce
        val range = flatten(6) { i => if (i > 3) emptyArray[Int]() else singleton(i + 2) }
        val x = reduce[Int](range.length)({ i => singleton(range.at(i) + 1) },
          { (a, b) => a + b }, 0)
        print(range.at(0))
        print(x)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform72 = withOutFileChecked(prefix+"fusion72"+suffix) {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        val outer = array(1) { e =>
          // Before, foreach didn't get vertically fused, because it is captured in
          // the return singleton's effects, whereas array evaluated before,
          // so not in same exact scope. Now singleton doesn't have block anymore,
          // just sym, and effects are tracked by loop block.
          array(10) { i => i+e }.foreach({ x: Rep[Int] => print(x) })
          0
        }
        print(outer)
      }
    }
    new Prog with Impl
  }

  // TODO think about:
  // Vertical fusion of effectful prod causes fused to have same Reflect around,
  // so could change order, and then horizontal fusion would fuse in wrong order?
}

