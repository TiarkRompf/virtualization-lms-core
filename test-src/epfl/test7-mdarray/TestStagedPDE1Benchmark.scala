package scala.virtualization.lms
package epfl
package test7

import test7.original.MDArray
import common.{Base, IfThenElseExp, ExportGraph, IfThenElsePureExp}
import internal.GraphVizExport
import test2.{DisableDCE, DisableCSE}
import java.io.PrintWriter

/*
To run only this test use:
sbt 'test-only scala.virtualization.lms.epfl.test7.TestStagedPDE1Benchmark'
*/
class TestStagedPDE1Benchmark extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-staged-"
  var typeFunction: Int => String = (i:Int) => ""

  def testPower = {
    // Perform actual tests:
    val pde1 = new PDE1Benchmark with MDArrayBaseExp with IfThenElseExp
    import pde1._

    performExperiment(pde1, pde1.range1(pde1.knownOnlyAtRuntime("matrix-1"), 1), prefix + "range1-test")
    performExperiment(pde1, pde1.range2(pde1.knownOnlyAtRuntime("matrix-2"), 1), prefix + "range2-test")
    performExperiment(pde1, pde1.range3(pde1.knownOnlyAtRuntime("matrix-3"), 1), prefix + "range3-test")
    //performExperiment(pde1, pde1.range4(pde1.knownOnlyAtRuntime("matrix-4"), 1), prefix + "range4-test")
    performExperiment(pde1, pde1.range5(pde1.knownOnlyAtRuntime("matrix-5"), 1), prefix + "range5-test")
    performExperiment(pde1, pde1.vectorTest, prefix + "vector-test")
  }

  def performExperiment(pde1: PDE1Benchmark with MDArrayBaseExp with IfThenElseExp, expr: Any, fileName: String) {

    withOutFile(fileName + "-type-inference") {
      val typing = new MDArrayBaseTypingUnifier { val IR: pde1.type = pde1 }
      try {
        val fullSubst = typing.obtainSubstitutions(expr, true)._2
        val export = new MDArrayGraphExport {
          val IR: pde1.type = pde1
          override def emitTypingString(i: Int) = typing.getTypingString(i, fullSubst)
        }
        export.emitDepGraph(expr.asInstanceOf[pde1.Exp[_]], fileName + "-dot", false)
      } catch {
        case e: Exception => println(e.printStackTrace)
        val export = new MDArrayGraphExport {
          val IR: pde1.type = pde1
          override def emitTypingString(i: Int) = "<inference exception>"
        }
        export.emitDepGraph(expr.asInstanceOf[pde1.Exp[_]], fileName + "-dot", false)
      }
    }
  }
}