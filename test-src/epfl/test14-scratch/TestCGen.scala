package scala.virtualization.lms
package epfl
package test14

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



class TestCGen extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test14-"
  
  trait DSL extends ScalaOpsPkg with LiftPrimitives with LiftString with LiftVariables {
    def test(x: Rep[Int]): Rep[Int]
  }

  trait Impl extends DSL with ScalaOpsPkgExp { self => 
    override val verbosity = 2
    val codegen = new CCodeGenPkg with CLikeGenVariables { val IR: self.type = self }
    codegen.emitSource(test, "main", new PrintWriter(System.out))
  }
  
  def testCGen1 = {
    withOutFile(prefix+"cgen1") {
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {

          var i = 0
          while (i < 10) {
            printf("Hello, world! %d\n", i)
            i = i + 1
          }

          0
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"cgen1")
  }


}