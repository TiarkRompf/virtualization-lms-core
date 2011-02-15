package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait NestLambdaProg extends Arith with Functions with Print { // also used by TestLambdaLift
  
  def test(x: Rep[Unit]) = {
    val f = doLambda { x: Rep[Double] =>
      val g = doLambda { y: Rep[Double] =>
        print("yo")
        y + (unit(4.0) * unit(3.0))
      }
      g
    }
    f
  }
  
}


class TestCodemotion extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testCodemotion1 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion1") {
      new NestLambdaProg with ArithExp with FunctionsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion1")
  }

}