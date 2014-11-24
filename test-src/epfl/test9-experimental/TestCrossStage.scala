package scala.virtualization.lms
package epfl
package test9

import common._
import internal.{ScalaCompile}
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ArrayLoopsFatExp,ScalaGenArrayLoops,ScalaGenFatArrayLoopsFusionOpt}

import org.scala_lang.virtualized.virtualize

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import collection.mutable.ArrayBuffer

class TestCrossStage extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test9-"
  
  @virtualize
  trait DSL extends Functions with ArrayBufferOps with Arith with OrderingOps with Variables with LiftVariables with IfThenElse with RangeOps with Print {
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
    def test(x: Rep[Int]): Rep[Unit]
    
    implicit def funToRep[T:Manifest,U:Manifest](x:T=>U): Rep[T=>U]
    implicit def abToRep[T:Manifest](x:ArrayBuffer[T]): Rep[ArrayBuffer[T]]
  }

  @virtualize
  trait Impl extends DSL with StaticDataExp with FunctionsExp with ArrayBufferOpsExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp with ScalaCompile { self => 

    def funToRep[T:Manifest,U:Manifest](x:T=>U): Rep[T=>U] = staticData(x)
    def abToRep[T:Manifest](x:ArrayBuffer[T]): Rep[ArrayBuffer[T]] = staticData(x)

    override val verbosity = 2
    val codegen = new ScalaGenStaticData with ScalaGenFunctions with ScalaGenArrayBufferOps with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { 
        val IR: self.type = self 
      }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
    println("-- running program")
    val f = compile(test)
    f(21)
  }

  // don't know sharing dependencies between static data in general -- for now assume there is no sharing
  
  def testCrossStage1 = {
    withOutFile(prefix+"csp1") {
      val f = (x: Int) => println("this is external non-DSL code: " + (2*x))
      
      @virtualize
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          
          doApply(f, x)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"csp1")
  }

  def testCrossStage2 = {
    withOutFile(prefix+"csp2") {
      val acc0 = new ArrayBuffer[Int]
      
      @virtualize
      trait Prog extends DSL {

        implicit def abToRep[T:Manifest](x:ArrayBuffer[T]): Rep[ArrayBuffer[T]]

        def test(x: Rep[Int]) = {
          val acc: Rep[ArrayBuffer[Int]] = acc0 // NOTE(trans) implicit doesn't kick in below
// Broke test compilation:
// array buffer switched to use implicis instead of
// infix methods and lifting did not kick in
          acc += x
          acc += x
          print("done")
        }
      }
      new Prog with Impl
      
      println("accumulated: " + acc0.mkString(","))
    }
    assertFileEqualsCheck(prefix+"csp2")
  }

}
