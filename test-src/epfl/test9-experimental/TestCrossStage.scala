package scala.virtualization.lms
package epfl
package test9

import common._
import internal.{ScalaCompile}
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ArrayLoopsFatExp,ScalaGenArrayLoops,ScalaGenFatArrayLoopsFusionOpt,TransformingStuff}

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import collection.mutable.ArrayBuffer

class TestCrossStage extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends Functions with ArrayBufferOps with Arith with OrderingOps with Variables with LiftVariables with IfThenElse with RangeOps with Print {
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
    def test(x: Rep[Int]): Rep[Any]
    
    /*abstract class StaticAccess[T]
    
    implicit def MM[T]: StaticAccess[T] = new StaticAccess[T] {}
    implicit def anyToRep[T:Manifest:StaticAccess](x:T): Rep[T] */
    
    implicit def funToRep[T:Manifest,U:Manifest](x:T=>U): Rep[T=>U]
    implicit def abToRep[T:Manifest](x:ArrayBuffer[T]): Rep[ArrayBuffer[T]]
  }

  trait Impl extends DSL with FunctionsExp with ArrayBufferOpsExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp with ScalaCompile { self => 

    def funToRep[T:Manifest,U:Manifest](x:T=>U): Rep[T=>U] = staticData(x)
    def abToRep[T:Manifest](x:ArrayBuffer[T]): Rep[ArrayBuffer[T]] = staticData(x)

    case class StaticData[T](x: T) extends Def[T]
    def staticData[T:Manifest](x: T): Exp[T] = StaticData(x)

    override def isWritableSym[A](w: Sym[A]): Boolean = findDefinition(w) match {
      case Some(TP(_, StaticData(_))) => true
      case _ => super.isWritableSym(w)
    }

    override val verbosity = 2
    val codegen = new ScalaGenFunctions with ScalaGenArrayBufferOps with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { 
        val IR: self.type = self 
        def getFreeDataExp[A](sym: Sym[A], rhs: Def[A]): List[(Sym[Any],Any)] = rhs match {
          case StaticData(x) => List((sym,x))
          case _ => Nil
        }
        override def getFreeDataBlock[A](start: Exp[A]): List[(Sym[Any],Any)] = {
          focusBlock(start) {
            focusExactScope(start) { levelScope =>
              levelScope flatMap { case TP(sym, rhs) =>
                getFreeDataExp(sym, rhs)
              }
            }
          }
        }
        override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
          case StaticData(x) => 
            emitValDef(sym, "p"+quote(sym) + " // static data: " + x)
          case _ => super.emitNode(sym, rhs)
        }
      }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
    println("-- running program")
    val f = compile(test)
    f(21)
  }


/*
  trait ImplFused extends DSL with ComplexStructExp with ArrayLoopsFatExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp with TransformingStuff { self => 
    override val verbosity = 2
    val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenStruct with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self;
        override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }
*/
  
  // don't know sharing dependencies between static data in general -- for now assume there is no sharing
  
  def testCrossStage1 = {
    withOutFile(prefix+"csp1") {
      val f = (x: Int) => println("this is external non-DSL code: " + (2*x))
      
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
      val acc = new ArrayBuffer[Int]
      
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          
          acc += x
          acc += x
        }
      }
      new Prog with Impl
      
      println("accumulated: " + acc.mkString(","))
    }
    assertFileEqualsCheck(prefix+"csp2")
  }

}