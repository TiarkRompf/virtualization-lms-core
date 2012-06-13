package scala.virtualization.lms
package epfl
package test11

import common._
import test1._
import test7._
import test8.{ArrayMutation,ArrayMutationExp,ScalaGenArrayMutation,OrderingOpsExpOpt}

import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext


class TestStencil extends FileDiffSuite {
  
  trait DSL extends LiftNumeric with NumericOps with ArrayOps with RangeOps with BooleanOps 
    with LiftVariables with IfThenElse with Print {
    def staticData[T:Manifest](x: T): Rep[T]
    def test(x: Rep[Array[Int]]): Rep[Array[Int]]
  }
  trait Impl extends DSL with Runner with ArrayOpsExpOpt with NumericOpsExpOpt with OrderingOpsExpOpt with BooleanOpsExp 
      with EqualExpOpt with VariablesExpOpt with RangeOpsExp with StaticDataExp
      with IfThenElseExpOpt with PrintExp 
      with CompileScala { self => 
    //override val verbosity = 1
    val codegen = new ScalaGenNumericOps with ScalaGenStaticData with ScalaGenOrderingOps with ScalaGenArrayOps with ScalaGenRangeOps
      with ScalaGenVariables with ScalaGenIfThenElse
      with ScalaGenPrint /*with LivenessOpt*/ { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
    run()
  }
  trait Runner extends Compile {
    def test(x: Rep[Array[Int]]): Rep[Array[Int]]
    def run() {
      val f = compile(test)
      val v0 = Array(3, 1, 5, -2, 4)
      val v1 = f(v0)
      v1 foreach println
    }
  }

/*
  trait Sliding extends DSL {
    
    def infix_sliding(n: Rep[Int], f: Rep[Int] => Rep[Double]): Rep[Array[Double]]
    
  }
  
  trait SlidingExp extends Impl with Sliding {
    
    def infix_sliding(n: Rep[Int], f: Rep[Int] => Rep[Double]): Rep[Array[Double]] = {
      val a = NewArray[Double](n)
      (0 until n) foreach { i =>
        a(i) = f(i)
      }
      a
    }
    
    
  }
*/


  val prefix = "test-out/epfl/test11-"
  
  def testStencil1 = {
    withOutFileChecked(prefix+"stencil1") {
      trait Prog extends DSL with ArrayOps with NumericOps {
        def test(v: Rep[Array[Int]]) = {

          /*
          .~(loop0 2 0 n .<a>. .<b>. (fun a ->
          let w1 j = a j *@ a (j+1) in 
          let wm j = a j -@ w1 j +@ w1 (j-1) in 
          let w2 j = wm j *@ wm (j+1) in 
          wm 0 -@ w2 0 +@ w2 (-1)))
          */
          
          /*
          
          def compute(b: Rep[Array[Int]], a: Rep[Array[Int]], n: Rep[Int]) = {
            def w(j: Rep[Int]) = a(j) * a(j+1)
            for (i <- 0 until n: Rep[Range]) {
              b(i) = a(i) - w(i) + w(i-1)
            }
          }
          */


          val A = scala.Array
          
          val a = A(A(1,0,0,1,0),
                    A(0,0,1,0,0),
                    A(0,1,0,0,0),
                    A(0,0,1,1,1),
                    A(0,0,1,0,1))

          val n = 5

          val v1 = NewArray[Int](n)
          v1
        }
      }
      new Prog with Impl
    }
  }


 
}