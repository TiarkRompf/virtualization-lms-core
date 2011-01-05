package scala.virtualization.lms
package epfl
package test7

import test7.original.MDArray
import common.{Base, IfThenElseExp, ExportGraph, IfThenElsePureExp}

/*
To run only this test use:
sbt 'test-only scala.virtualization.lms.epfl.test7.TestStagedPDE1Benchmark'
*/
class TestStagedPDE1Benchmark extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-staged-"

  def testPower = {
    // Perform actual tests:
    val pde1 = new PDE1Benchmark with MDArrayBaseExp with IfThenElseExp
    val pde1export = new ExportGraph { val IR: pde1.type = pde1 }

    pde1export.exportGraph(prefix+"range1-dot", false)(pde1.range1(pde1.fresh[MDArray[Double]], 1))
    pde1export.exportGraph(prefix+"range2-dot", false)(pde1.range2(pde1.fresh[MDArray[Double]], 1))
    pde1export.exportGraph(prefix+"range3-dot", false)(pde1.range3(pde1.fresh[MDArray[Double]], 1))
  //pde1export.exportGraph(prefix+"range4-dot", false)(pde1.range4(pde1.fresh[MDArray[Double]], 1))
    pde1export.exportGraph(prefix+"range5-dot", false)(pde1.range5(pde1.fresh[MDArray[Double]], 1))

//    withOutFile(prefix+"range1")(pde1.range1(pde1.fresh[MDArray[Double]], 1))
//    withOutFile(prefix+"range2")(pde1.range2(pde1.fresh[MDArray[Double]], 1))
//    withOutFile(prefix+"range3")(pde1.range3(pde1.fresh[MDArray[Double]], 1))
////  withOutFile(prefix+"range4")(pde1.range4(pde1.fresh[MDArray[Double]], 1))
//    withOutFile(prefix+"range5")(pde1.range5(pde1.fresh[MDArray[Double]], 1))
  }
}