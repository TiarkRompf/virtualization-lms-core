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
      
      var loops = focusExactScope(result) { levelScope => 
        levelScope collect {
          case e @ TP(_, LoopArray(_,_,_)) => e
          case e @ TP(_, LoopReduce(_,_,_)) => e
        }
      }
      
      var Wloops: List[List[TP[_]]] = loops.map(List(_))
      
      if (loops.nonEmpty) {
      
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

          
          // find negative dependencies (those that can block fusion)
          
          val tableNeg = loops.flatMap { dx => // find non-simple dependencies
            getSchedule(currentScope)(getLoopRes(dx)) flatMap {
              case TP(_, ArrayIndex(a, i)) if i == getLoopVar(dx) => Nil
              case sc =>
                syms(sc.rhs).intersect(loopSyms).map { otherLoop => (otherLoop, dx.sym) }
            }
          }.distinct
        
          val tablePos = loops.flatMap { dx => // find simple dependencies
            getSchedule(currentScope)(getLoopRes(dx)) flatMap {
              case TP(_, ArrayIndex(a, i)) if i == getLoopVar(dx) && loopSyms.contains(a) => 
                List((a, dx.sym))
              case sc => Nil
            }
          }.distinct

          val WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies
            val otherLoopSyms = loopSyms diff (dx map (_.sym))
            getSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
              case e@TP(_, ArrayIndex(a, i)) if (WgetLoopVar(dx) contains i) => Nil              
              case sc =>
                syms(sc.rhs).intersect(otherLoopSyms).flatMap { otherLoop => dx map (d => (otherLoop, d.sym)) }
            }
          }.distinct
        
          val WtablePos = Wloops.flatMap { dx => // find simple dependencies
            getSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
              case TP(_, ArrayIndex(a, i)) if WgetLoopVar(dx).contains(i) && loopSyms.contains(a) => // not correct!
                dx map (d => (a, d.sym))
              case sc => Nil
            }
          }.distinct

          println("wtableneg: " + WtableNeg)
      
          
          // partitioning
          
          def canFuse(a: Sym[Any], b: Sym[Any]): Boolean = (WtableNeg intersect List((a,b),(b,a))).isEmpty
          def canFuse2(a: List[Sym[Any]], b: List[Sym[Any]]): Boolean = a.forall(x=>b.forall(y=>canFuse(x,y))) // TODO:opt
      

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
        
          // actually do the fusion: remove simple edges
        
          if ((partitionsOut intersect partitionsIn) != partitionsOut) {
            Wloops = partitionsOut.map(_ map (s => currentScope.find(_.sym == s).get))

            // within fused loops, remove accesses to outcomes of the fusion

            currentScope = currentScope.map { // a(i) where a = Loop { i => ... } (remove those!)
              case e@TP(_, ArrayIndex(a, i)) =>
                Wloops.find(_ exists (_.sym == a)) match {
                  case Some(fused) if WgetLoopVar(fused) contains i => 
                    val f = TP(e.sym, Copy(getLoopRes(fused.find(_.sym == a).get)))
                    println("replace " + e + " by " + f)
                    f
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
  
      
/*
      val allUses = (levelScope ::: innerScope) filter (d => syms(d.rhs).intersect(loopSyms) != Nil)
      val simpleUses = innerScope collect { 
        case e @ TP(_, ArrayIndex(a, i)) if loopSyms.contains(a) && loopVars.contains(i) => e
      }
    
    
      // used nontrivially --> must be created
      
    
      println("loops: " + loopSyms)
      println("tablePos: " + tablePos)
      println("tableNeg: " + tableNeg)
      println("all uses: " + allUses.map(_.sym))
      println("simple uses: " + simpleUses.map(_.sym))
*/      
      
      focusExactScope(result) { levelScope => 
        // do what super does
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
  
  def test(x: Rep[Unit]) = {
    
    val one = array(100) { i => 1 }

    val grow = array(100) { i => 2.0 * one.at(i)/*2.0*i*/ }

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