package scala.virtualization.lms
package epfl
package test4

import common._
import test1._
import test2._
import test3._


trait FacProg { this: Arith with Matching with Extractors =>

  def fac(n: Rep[Double]): Rep[Double] = n switch {
    case n if n guard(0) => unit(1.0) + unit(0.0)
  } orElse {
    case n => n * fac(n-1.0)
  }

}


class TestFac extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test4-"

  def testFac1 = {
    withOutFile(prefix+"fac1") {
      object FacProgExp extends FacProg with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import FacProgExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, prefix+"fac1-dot")
    }
    assertFileEqualsCheck(prefix+"fac1")
    assertFileEqualsCheck(prefix+"fac1-dot")
  }

  def testFac2 = {
    withOutFile(prefix+"fac2") {
      object FacProgExp extends FacProg with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with FunctionExpUnfoldAll with FunctionsExternalDef2
//        with FunctionsExternalDef01
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import FacProgExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, prefix+"fac2-dot")
    }
    assertFileEqualsCheck(prefix+"fac2")
    assertFileEqualsCheck(prefix+"fac2-dot")
  }

  def testFac3 = {
    withOutFile(prefix+"fac3") {
      object FacProgExp extends FacProg with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with FunctionExpUnfoldRecursion 
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import FacProgExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, prefix+"fac3-dot")
    }
    assertFileEqualsCheck(prefix+"fac3")
    assertFileEqualsCheck(prefix+"fac3-dot")
  }
}