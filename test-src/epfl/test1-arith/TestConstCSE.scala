package scala.lms
package epfl
package test1

import common._
import internal._
import java.io._

import scala.reflect.SourceContext


class TestConstCSE extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test1-"
  
  /**
   * This test targets checking resolved bug for equality check
   * on Const values. For more information, have a look at 
   * "equals" method implementation (and its comments) for Const
   * class inside Expressions trait.
   */
  def testBugConstCSE1 = {
    withOutFile(prefix+"constcse1") {
      trait Prog extends ScalaOpsPkg {
        def test1(test_param: Rep[Boolean], acc: Rep[Long]): Rep[Long] = {
          val dblVal = if(test_param) unit(1.0) else unit(0.0)
          val lngVal = if(test_param) unit(1L) else unit(0L)
          auxMethod(acc + lngVal, dblVal)
        }

        def auxMethod(val1: Rep[Long], val2: Rep[Double]): Rep[Long] = {
          val1 + unit(133L) + rep_asinstanceof[Double, Long](val2,manifest[Double],manifest[Long])
        }
      }

       new Prog with ScalaOpsPkgExp with ScalaCompile{ self =>

        val printWriter = new java.io.PrintWriter(System.out)

        //test1: first "loop"
        val codegen = new ScalaCodeGenPkg with ScalaCodegen{ val IR: self.type = self }

        codegen.emitSource2(test1 _ , "test1", printWriter)
        val source = new StringWriter
        val testc1 = compile2(test1)
        scala.Console.println(testc1(true,12))


      }
    }
    assertFileEqualsCheck(prefix+"constcse1")
  }

}
