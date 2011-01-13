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

  def testPower = {
    // Perform actual tests:
    val pde1 = new PDE1Benchmark with MDArrayBaseExp with IfThenElseExp

    val pde1export = new GraphVizExport {
      val IR: pde1.type = pde1
    }

    val typing = new MDArrayBaseTyping {
      val IR: pde1.type = pde1
    }

    import pde1._

    val range1 = pde1.range1(pde1.knownOnlyAtRuntime(Nil), 1)
    val range2 = pde1.range2(pde1.knownOnlyAtRuntime(Nil), 1)
    val range3 = pde1.range3(pde1.knownOnlyAtRuntime(Nil), 1)
    //val range4 = pde1.range4(pde1.knownOnlyAtRuntime(Nil), 1)
    val range5 = pde1.range5(pde1.knownOnlyAtRuntime(Nil), 1)
    val vector = pde1.vectorTest

    pde1export.emitDepGraph(range1, prefix+"range1-dot", false)
    withOutFile(prefix+"range1-type")({typing.printTypingConstraints(range1)})
    pde1export.emitDepGraph(range2, prefix+"range2-dot", false)
    withOutFile(prefix+"range2-type")({typing.printTypingConstraints(range2)})
    pde1export.emitDepGraph(range3, prefix+"range3-dot", false)
    withOutFile(prefix+"range3-type")({typing.printTypingConstraints(range3)})
  //pde1export.emitDepGraph(range4, prefix+"range4-dot", false)
  //withOutFile(prefix+"range4-type")({typing.printTypingConstraints(range4)})
    pde1export.emitDepGraph(range5, prefix+"range5-dot", false)
    withOutFile(prefix+"range5-type")({typing.printTypingConstraints(range5)})
    pde1export.emitDepGraph(vector, prefix+"vector-test-dot", false)
    withOutFile(prefix+"vector-test-type")({typing.printTypingConstraints(vector)})
  }
}