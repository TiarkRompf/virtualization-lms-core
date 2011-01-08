package scala.virtualization.lms
package epfl
package test7

import test7.original.MDArray
import common.{Base, IfThenElseExp, ExportGraph, IfThenElsePureExp}
import internal.GraphVizExport
import test2.{DisableDCE, DisableCSE}

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
      val IR: pde1.type = pde1;
    }

    pde1export.emitDepGraph(pde1.range1(pde1.fresh, 1), prefix+"range1-dot", false)
    pde1export.emitDepGraph(pde1.range2(pde1.fresh, 1), prefix+"range2-dot", false)
    pde1export.emitDepGraph(pde1.range3(pde1.fresh, 1), prefix+"range3-dot", false)
  //pde1export.emitDepGraph(pde1.range4(pde1.fresh, 1), prefix+"range4-dot", false)
    pde1export.emitDepGraph(pde1.range5(pde1.fresh, 1), prefix+"range5-dot", false)

    withOutFile(prefix+"global-defs")(pde1.globalDefs.mkString("\n"))

    withOutFile(prefix+"range1")(println(pde1.range1(pde1.fresh[MDArray[Double]], 1).toString))
    withOutFile(prefix+"range2")(println(pde1.range2(pde1.fresh[MDArray[Double]], 1).toString))
    withOutFile(prefix+"range3")(println(pde1.range3(pde1.fresh[MDArray[Double]], 1).toString))
//  withOutFile(prefix+"range4")(println(pde1.range4(pde1.fresh[MDArray[Double]], 1).toString))
    withOutFile(prefix+"range5")(println(pde1.range5(pde1.fresh[MDArray[Double]], 1).toString))
  }
}