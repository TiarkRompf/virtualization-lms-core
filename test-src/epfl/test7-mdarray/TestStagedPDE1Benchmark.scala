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
//    TODO: Remove at next cleanup. This code was used for debugging the KnownAtCompileTime bug.
//    {
//      override def findOrCreateDefinition[T](d: Def[T]): TP[T] = d match {
//        case that: KnownAtCompileTime[_] => {
//          println("known " + that + " found => " + findDefinition[T](d).toString)
//          try {
//              println(d.toString + " " + findDefinition[T](d).get.rhs + " => " + (d == findDefinition[T](d).get.rhs))
//          } catch {
//            case _ => println("not defined")
//          }
//          findDefinition[T](d).getOrElse {
//            createDefinition(fresh[T], d)
//          }
//        }
//        case _ => {
//          findDefinition[T](d).getOrElse {
//            createDefinition(fresh[T], d)
//          }
//        }
//      }
//    }
    val pde1export = new GraphVizExport {
      val IR: pde1.type = pde1
    }

    val typing = new MDArrayBaseTyping {
      val IR: pde1.type = pde1
    }

//    pde1export.emitDepGraph(pde1.range1(pde1.fresh, 1), prefix+"range1-dot", false)
//    pde1export.emitDepGraph(pde1.range2(pde1.fresh, 1), prefix+"range2-dot", false)
//    pde1export.emitDepGraph(pde1.range3(pde1.fresh, 1), prefix+"range3-dot", false)
//  //pde1export.emitDepGraph(pde1.range4(pde1.fresh, 1), prefix+"range4-dot", false)
//    pde1export.emitDepGraph(pde1.range5(pde1.fresh, 1), prefix+"range5-dot", false)
    pde1export.emitDepGraph(pde1.vectorTest, prefix+"vector-test-dot", false)
    withOutFile(prefix+"vector-test-type")({typing.printTypingConstraints(pde1.vectorTest)})
  }
}