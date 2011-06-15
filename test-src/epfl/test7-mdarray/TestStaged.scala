package scala.virtualization.lms
package epfl
package test7

import test7.original.MDArray
import internal.GraphVizExport
import test2.{DisableDCE, DisableCSE}
import common._
import test4.ScalaGenFunctionsExternal
import test1.ScalaGenArith
import java.io.{FileWriter, PrintWriter}

/*
To run only this test use:
sbt 'test-only scala.virtualization.lms.epfl.test7.TestStagedPDE1Benchmark'
*/
class TestStaged extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-staged-"
  var typeFunction: Int => String = (i:Int) => ""

  def testPower = {
    // Perform actual tests:
    val pde1 = new PDE1BenchmarkStaged with MDArrayBaseExp with IfThenElseExp
    val gol = new GameOfLifeStaged with MDArrayBaseExp with IfThenElseExp
    val scp = new ScopeTestStaged with MDArrayBaseExp with IfThenElse

    // TODO: Re-enable scope test when scopes are enabled
    //(new Experiment { val dsl: scp.type = scp }) ((a: scp.Rep[(MDArray[Boolean], MDArray[Int])]) => scp.testStaged(scp.pairToArgs(a)._1, scp.pairToArgs(a)._2), prefix + "scope-test")
    (new Experiment { val dsl: scp.type = scp }) ((a: scp.Rep[(MDArray[Int], MDArray[Int])]) => scp.testShapes(scp.pairToArgs(a)._1, scp.pairToArgs(a)._2), prefix + "shape-test")
    (new Experiment { val dsl: scp.type = scp }) ((a: scp.Rep[(MDArray[Int], MDArray[Int])]) => { import scp._; (a._1 + a._2 + (10::10::10::Nil)) }, prefix + "simple-test")

    // PDE1 experiments
    (new Experiment { val dsl: pde1.type = pde1 })((a: pde1.Rep[MDArray[Double]]) => pde1.range1(a, 1), prefix + "range1-test")
    (new Experiment { val dsl: pde1.type = pde1 })((a: pde1.Rep[MDArray[Double]]) => pde1.range2(a, 1), prefix + "range2-test")
    (new Experiment { val dsl: pde1.type = pde1 })((a: pde1.Rep[MDArray[Double]]) => pde1.range3(a, 1), prefix + "range3-test")
    // TODO: Include ranges to make this work
    //(new Experiment { val dsl: pde1.type = pde1 })((a: pde1.Rep[MDArray[Double]]) => pde1.range4(a, 1), prefix + "range4-test")
    (new Experiment { val dsl: pde1.type = pde1 })((a: pde1.Rep[MDArray[Double]]) => pde1.range5(a, 1), prefix + "range5-test")

    // Game of Life experiments
    (new Experiment { val dsl: gol.type = gol })((a: gol.Rep[MDArray[Int]]) => gol.gameOfLife(gol.reshape(gol.convertFromListRep(10::10::Nil), a)), prefix + "game-of-life-10-by-10")
    (new Experiment { val dsl: gol.type = gol })((a: gol.Rep[MDArray[Int]]) => gol.gameOfLife(a), prefix + "game-of-life-generic")
  }

  trait Experiment {
    val dsl: MDArrayBaseExp

    def apply[A: Manifest, B: Manifest](f: dsl.Exp[A] => dsl.Exp[B], fileName: String) = {
      val typing = new MDArrayTypingBubbleUp { val IR: dsl.type = dsl }

      /* Problem here: Generating the code creates a new fresh[A] and applies f to it. This leads to partial trashing
       * of the typing information gathered so far. What we need to do is retype the entire thing just after the
       * function application.
       *
       * Now to do this, we can do the following hack, which couldn't get any uglier:
       * TODO: Fix this thing, it needs a complete separate typing phase
       */
      def ff(arg: dsl.Exp[A]): dsl.Exp[B] = {
        val res = f(arg)

        System.err.println("Performing experiment: " + fileName)

        withOutFile(fileName + "-type-inference") {
          try {
            typing.doTyping(res, true)
            val export = new MDArrayGraphExport {
              val IR: dsl.type = dsl
              override val TY = typing
            }
            export.emitDepGraph(res, new PrintWriter(fileName + "-dot"), false)
          } catch {
            case e => throw e
          }
        }

        res
      }

      // Generate the corresponding code :)
      implicit val printWriter: PrintWriter = IndentWriter.getIndentPrintWriter(new FileWriter(fileName + "-code.scala"))
      val scalaGen = new ScalaGenMDArray with ScalaGenIfThenElse with ScalaGenArguments { val IR: dsl.type = dsl; override val TY = typing }
      scalaGen.emitSource(ff, "Experiment", printWriter)
      printWriter.close
    }
  }
}