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
  val IR: LoopsExp with ArithExp with PrintExp // TODO: refactor
  import IR._  


  case class Copy(a: Exp[Any]) extends Def[Any]
  
  case class Combine(a: List[Exp[Any]]) extends Exp[Any]
  
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Copy(a) => emitValDef(sym, " <- " + quote(a))
    case _ => super.emitNode(sym, rhs)
  }
  
  
  
  case class ArrayElem(y: Exp[Double]) extends Def[Array[Double]]
  case class ReduceElem(y: Exp[Double]) extends Def[Double]
  

  class FatDef(val elems: List[Def[_]])

  case class TTP(val lhs: List[Sym[_]], val rhs: FatDef)
  
  case class ThinDef(rhs: Def[_]) extends FatDef(List(rhs))
  case class FatLoop(s: Rep[Int], x: List[Sym[Int]], rhs: List[Def[_]]) extends FatDef(rhs)
  
  import util.GraphUtil
  
  def getSchedule(scope: List[TP[_]])(result: Any): List[TP[_]] = {
    def deps(st: List[Sym[_]]): List[TP[_]] =
      scope.filter(st contains _.sym)

    GraphUtil.stronglyConnectedComponents[TP[_]](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatSchedule(scope: List[TTP])(result: Any): List[TTP] = {
    def deps(st: List[Sym[_]]): List[TTP] =
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[TTP](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatDependentStuff(scope: List[TTP])(st: List[Sym[_]]): List[TTP] = {
    st.flatMap(getFatDependentStuff0(scope)(_)).distinct
  }
    
  def getFatDependentStuff0(scope: List[TTP])(st: Sym[_]): List[TTP] = {
    def uses(s: Sym[_]): List[TTP] = {
      scope.filter { d =>
        (d.lhs contains s) || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    GraphUtil.stronglyConnectedComponents[TTP](uses(st), t => t.lhs flatMap uses).flatten
  }


  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case FatLoop(s, x, rhs) => syms(s):::syms(rhs)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case FatLoop(s, x, rhs) => x
    case _ => super.boundSyms(e)
  }

  
  def fatten(defs: List[TP[_]]) = defs map {
    case e @ TP(sym, LoopArray(s,x,y)) => TTP(List(sym), FatLoop(s,List(x),List(ArrayElem(y))))
    case e @ TP(sym, LoopReduce(s,x,y)) => TTP(List(sym), FatLoop(s,List(x),List(ReduceElem(y))))
    case TP(sym, rhs) => TTP(List(sym), ThinDef(rhs))
  }
  
  
  
  override def emitBlock(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    focusBlock(result) {
      
      // find loops at current top level
      var Wloops = focusExactScope(result) { levelScope => 
        fatten(levelScope) collect { case e @ TTP(_, FatLoop(_,_,_)) => e }
      }
      
      if (Wloops.nonEmpty) {
        var currentScope = fatten(innerScope)
        var done = false

        do {
          // utils
          def WgetLoopVar(e: TTP): List[Sym[Int]] = e.rhs match { case FatLoop(s,x,rhs) => x }
          def WgetLoopRes(e: TTP): List[Def[_]] = e.rhs match { case FatLoop(s,x,rhs) => rhs }

          val loopSyms = Wloops flatMap (_.lhs)
          val loopVars = Wloops flatMap WgetLoopVar

          val WloopSyms = Wloops map (_.lhs)
          val WloopVars = Wloops map WgetLoopVar

          
          // find negative dependencies (those that block fusion)
          
          val WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies
            val otherLoopSyms = loopSyms diff (dx.lhs)
            getFatSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
              case TTP(_, ThinDef(ArrayIndex(a, i))) if (WgetLoopVar(dx) contains i) =>
                Nil
              case sc =>
                syms(sc.rhs).intersect(otherLoopSyms).flatMap { otherLoop => dx.lhs map ((otherLoop, _)) }
            }
          }.distinct
        
          println("wtableneg: " + WtableNeg)
          
          
          // partitioning: build maximal sets of loops to be fused
          
          def canFuse(a: TTP, b: TTP): Boolean = {
            val cartesian = a.lhs.flatMap(x=>b.lhs.flatMap(y=>List((x,y),(y,x))))
            (WtableNeg intersect cartesian).isEmpty
          }


          var partitionsIn = Wloops
          var partitionsOut = Nil:List[TTP]
        
          for (b <- partitionsIn) {
            // try to add to an item in partitionsOut, if not possible add as-is
            partitionsOut.find(a => canFuse(a,b)) match {
              case Some(a) => 
                val shape = a.rhs match { case FatLoop(s,_,_) => s } // TODO!
                val fused = TTP(a.lhs:::b.lhs, FatLoop(shape, WgetLoopVar(a):::WgetLoopVar(b), WgetLoopRes(a):::WgetLoopRes(b)))
                partitionsOut = fused :: (partitionsOut diff List(a))
              case None => partitionsOut = b::partitionsOut
            }
          }
        
          println("partitions: " + partitionsOut)
        
        
          // actually do the fusion: 
        
          if ((partitionsOut intersect partitionsIn) != partitionsOut) {

            currentScope = currentScope diff Wloops
            Wloops = partitionsOut
            currentScope = currentScope ::: Wloops
            
            // equalize loop variables (TODO!)

            // within fused loops, remove accesses to outcomes of the fusion

            currentScope = currentScope.map { // a(i) where a = Loop { i => ... } (remove those!)
              case e@TTP(_, ThinDef(ArrayIndex(a, i))) =>
                Wloops.find(_.lhs contains a) match {
                  case Some(fused) if WgetLoopVar(fused) contains i => 
                    val index = fused.lhs.indexOf(a)
                    
                    println("replace " + e + " at " + index + " within " + fused)

                    val rhs = WgetLoopRes(fused)(index) match { case ArrayElem(y) => y }
                    
                    val f = TTP(e.lhs, ThinDef(Copy(rhs)))
                    // replace e.sym by getLoopRes(fused.find(_.sym == a))
                    println("replace " + //e.lhs + " by " + (f.rhs match { case Copy(a) => a }) + 
                      " / " + e + " by " + f)
                    f
                  case _ => e
                }
              case e => e
            }
            
            currentScope = getFatSchedule(currentScope)(result) // clean things up!

            println("try once more ...")
          } else {
            println("no changes, we're done")
            done = true
          }
        
        } while (!done)
       
        
       
        // do what super does, modula fat stuff
        focusExactScopeFat(currentScope)(result) { levelScope => 
          for (TTP(syms, rhs) <- levelScope) {
            emitFatNode(syms, rhs)
          }
        }

      } else {
        // do what super does
        focusExactScope(result) { levelScope => 
          for (TP(sym, rhs) <- levelScope) {
            emitNode(sym, rhs)
          }
        }

      }
    }
  }
  

  def emitFatNode(sym: List[Sym[_]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case FatLoop(s,x,rhs) => 
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println("var " + quote(l) + " = new Array[]("+quote(s)+")")
          case ReduceElem(y) =>
            stream.println("var " + quote(l) + " = 0")
        }
      }
      val ii = x(0)
//      stream.println("var " + quote(ii) + " = 0")
//      stream.println("while ("+quote(ii)+" < "+quote(s)+") {")
      stream.println("for ("+quote(ii)+" <- 0 until "+quote(s)+") {")
      for (jj <- x.drop(1)) {
        stream.println(quote(jj)+" = "+quote(ii))
      }
      emitFatBlock(syms(rhs))
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println(quote(l) + "("+quote(ii)+") = " + quote(getBlockResult(y)))
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
        }
      }
//      stream.println(quote(ii)+" += 1")
      stream.println("}")
    case ThinDef(a) => emitNode(sym(0), a)
    case _ => error("don't know how to generate code for: "+rhs)
  }

  def emitFatBlock(rhs: List[Exp[_]])(implicit stream: PrintWriter): Unit = {
    emitBlock(Combine(rhs))
  }

  def focusExactScopeFat[A](currentScope: List[TTP])(result: Exp[_])(body: List[TTP] => A): A = {
    
    val saveInner = innerScope
    
    val e1 = currentScope
    shallow = true
    val e2 = getFatSchedule(currentScope)(result) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false

    // shallow is 'must outside + should outside' <--- currently shallow == deep for lambdas, meaning everything 'should outside'
    // bound is 'must inside'

    // find transitive dependencies on bound syms, including their defs (in case of effects)
    val bound = for (TTP(syms, rhs) <- e1; s <- boundSyms(rhs)) yield s
    val g1 = getFatDependentStuff(currentScope)(bound)
    
    val levelScope = e1.filter(z => (e2 contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound

/*
    // sanity check to make sure all effects are accounted for
    result match {
      case Def(Reify(x, effects)) =>
        val actual = levelScope.filter(effects contains _.sym)
        assert(effects == actual.map(_.sym), "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
      case _ =>
    }
*/
    val innerScope2 = e1 diff levelScope // delay everything that remains

    innerScope = innerScope2 flatMap { 
      case TTP(List(sym), ThinDef(rhs)) => List(TP(sym, rhs))
      case e => 
      println("ignore: " + e)
      Nil
    }

    val rval = body(levelScope)
    
    innerScope = saveInner
    rval
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