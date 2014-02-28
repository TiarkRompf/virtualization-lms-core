package scala.virtualization.lms
package epfl
package test1

import common._
import internal._

import scala.reflect.SourceContext


class TestNumeric extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test1-"
  

  // this seems to be a bug in Scala-Virtualized related to reified __new

  def testBugNumeric1 = {
    withOutFile(prefix+"numeric1") {
      trait Prog extends Base with NumericOps with PrimitiveOps with StructOps with LiftNumeric {
        def test(x: Rep[Int]) = {
          
          val a = unit(2.0)
          val f1 = (1.0 + a) // this one is ok

          val struct = new Record {  // FIXME: each of the statements below cause compiler errors ("erroneous or inaccessible type")
              //val f1 = (1.0 + a):Rep[Double]
              //val f1 = (1.0 + a)
              //val f1 = { val z = (1.0 + a); z }
              val dummy = 1.0
          }
        }
      }
    }
  }



  trait MinimalIntf {
    def infix_/(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]

    class Rep[T]
    abstract class Ops[T](x: Rep[T]) {
      def *(y: Rep[T]): Rep[T]
      def /(y: Rep[T]): Rep[T]
    }
    implicit def doubleToOps(x:Double): Ops[Double]
    implicit def doubleRepToOps(x:Rep[Double]): Ops[Double]

    def sqrt(x: Rep[Double]): Rep[Double]
    def acos(x: Rep[Double]): Rep[Double]

    def unit(x:Double): Rep[Double]
  }

  def testBugNumeric2 = {
    withOutFile(prefix+"numeric2") {
      trait Prog extends MinimalIntf {
        def test = {
          val r = unit(6.0)
          val q = unit(6.0)
          //val theta = r/sqrt(-1.0*q*q*q*q)   // FIXME: "erroneous or inaccessible type" on one of the *q trees
        }

      }
    }
  }

  // this one may be slightly simpler to debug

  def testBugNumeric2b = {
    withOutFile(prefix+"numeric2b") {
      trait Prog extends MinimalIntf {
        def test = {
          val numRows = unit(7)

          //val xstep = 25.0/numRows //FIXME: "erroneous or inaccessible type"
        }

      }
    }
  }


  def testBugNumeric3 = {
    withOutFile(prefix+"numeric3") {
      trait Prog extends Base with NumericOps with PrimitiveOps with LiftNumeric with StringOps {

        def test = {
          val numRows = unit(7)

          //val xstep = 25.0/numRows // could not find implicit Numeric[AnyVal]  (this is ok)
          //val msg = "step = " + xstep  //FIXME: "EMBEDDING: cannot resolve target method (sym=<none>): infix_$plus("res = ", xstep)"

        }

      }
    }
  }



}
