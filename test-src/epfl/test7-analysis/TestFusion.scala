package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

// very preliminary!

trait Fusion extends internal.GenericNestedCodegen {
  import IR._




}


trait ScalaGenLoopsFusionOpt extends ScalaGenLoops with Fusion {
  val IR: LoopsExp
  import IR._
  
  
  case class Copy(a: Exp[Any]) extends Def[Any]
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Copy(a) => emitValDef(sym, " <- " + quote(a))
    case _ => super.emitNode(sym, rhs)
  }
  
  
  
  
  override def emitBlock(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    focusBlock(result) {
      
      // find loops at current top level
      var loops = focusExactScope(result) { levelScope => 
        levelScope collect {
          case e @ TP(_, LoopArray(_,_,_)) => e
          case e @ TP(_, LoopReduce(_,_,_)) => e
        }
      }
      
      if (loops.nonEmpty) {
        var Wloops: List[List[TP[_]]] = loops.map(List(_))
        var currentScope = innerScope
        var done = false
        do {
          
          // utils
          def getLoopVar(e: TP[_]) = e match {
            case e @ TP(_, LoopArray(_,x,_)) => x
            case e @ TP(_, LoopReduce(_,x,_)) => x
          }
          def getLoopRes(e: TP[_]) = e match {
            case e @ TP(_, LoopArray(_,_,y)) => y
            case e @ TP(_, LoopReduce(_,_,y)) => y
          }

          def WgetLoopVar(e: List[TP[_]]) = e map getLoopVar
          def WgetLoopRes(e: List[TP[_]]) = e map getLoopRes

          val loopSyms = loops map (_.sym)
          val loopVars = loops map getLoopVar

          val WloopSyms = Wloops map (_ map (_.sym))
          val WloopVars = Wloops map WgetLoopVar

          
          // find negative dependencies (those that block fusion)
          
          val WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies
            val otherLoopSyms = loopSyms diff (dx map (_.sym))
            getSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
              case e@TP(_, ArrayIndex(a, i)) if (WgetLoopVar(dx) contains i) =>
                Nil
              case sc =>
                syms(sc.rhs).intersect(otherLoopSyms).flatMap { otherLoop => dx map (d => (otherLoop, d.sym)) }
            }
          }.distinct
        
          println("wtableneg: " + WtableNeg)
          
          
          // partitioning
          
          def canFuse(a: Sym[Any], b: Sym[Any]): Boolean = (WtableNeg intersect List((a,b),(b,a))).isEmpty
          def canFuse2(a: List[Sym[Any]], b: List[Sym[Any]]): Boolean = {
            val cartesian = a.flatMap(x=>b.flatMap(y=>List((x,y),(y,x))))
            (WtableNeg intersect cartesian).isEmpty
          }

          var partitionsIn = WloopSyms
          var partitionsOut = Nil:List[List[Sym[Any]]]
        
          for (b <- partitionsIn) {
            // try to add to an item in partitionsOut, if not possible add as-is
            partitionsOut.find(a => canFuse2(a,b)) match {
              case Some(a) => 
                val fused = (b:::a)
                partitionsOut = fused :: (partitionsOut.filterNot(_ == a))
              case None => partitionsOut = b::partitionsOut
            }
          }
        
          println("partitions: " + partitionsOut)
        
        
          // actually do the fusion:
        
          if ((partitionsOut intersect partitionsIn) != partitionsOut) {
            Wloops = partitionsOut.map(_ map (s => currentScope.find(_.sym == s).get))

            // equalize loop variables (TODO!)

            // within fused loops, remove accesses to outcomes of the fusion

            currentScope = currentScope.map { // a(i) where a = Loop { i => ... } (remove those!)
              case e@TP(_, ArrayIndex(a, i)) =>
                Wloops.find(_ exists (_.sym == a)) match {
                  case Some(fused) if WgetLoopVar(fused) contains i => 
                    val f = TP(e.sym, Copy(getLoopRes(fused.find(_.sym == a).get)))
                    // replace e.sym by getLoopRes(fused.find(_.sym == a))
                    println("replace " + e.sym + " by " + (f.rhs match { case Copy(a) => a }) + 
                      " / " + e + " by " + f)
                    e//f
                  case _ => e
                }
              case e => e
            }

            println("try once more ...")
          } else {
            println("no changes, we're done")
            done = true
          }
        
        } while (!done)
       
        innerScope = currentScope
      }
  

      // do what super does
      focusExactScope(result) { levelScope => 
        for (TP(sym, rhs) <- levelScope) {
          emitNode(sym, rhs)
        }
      }
    }
  }
  

}




// trait NestLambdaProg extends Arith with Functions with Print 
// --> from TestCodeMotion.scala

trait FusionProg extends Arith with Loops with Print {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {
    
    val one = array(100) { i => 1 }

    val grow = array(100) { i => 2*i }

    val add = array(100) { i => one.at(i) + grow.at(i) }
    
    def square(x: Rep[Double]) = x*x
    def mean(x: Rep[Array[Double]]) = reduce(100) { i => x.at(i) } / 100.0
    def vari(x: Rep[Array[Double]]) = reduce(100) { i => square(x.at(i)) } / 100.0 - square(mean(x))
    
    val m = mean(add)
    val v = vari(add)
    
//    val stddev = sqrt(variance)


    print(m)
    print(v)
  }
  
}


class TestFusion extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testFusion1 = {
    withOutFile(prefix+"fusion1") {
      new FusionProg with ArithExp with LoopsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenLoops with ScalaGenPrint with Fusion { val IR: self.type = self }
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion1")
  }

  def testFusion2 = {
    withOutFile(prefix+"fusion2") {
      new FusionProg with ArithExp with LoopsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenLoopsFusionOpt with ScalaGenPrint with Fusion { val IR: self.type = self }
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion2")
  }
 
}