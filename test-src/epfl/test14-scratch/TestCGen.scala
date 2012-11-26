package scala.virtualization.lms
package epfl
package test14

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



class TestCGen extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test14-"
  
  trait DSL extends ScalaOpsPkg with TupledFunctions with LiftPrimitives with LiftString with LiftVariables {
    def test(x: Rep[Int]): Rep[Int]
  }

  trait Impl extends DSL with ScalaOpsPkgExp with TupledFunctionsRecursiveExp { self => 
    val codegen = new CCodeGenPkg with CGenVariables with CGenTupledFunctions { val IR: self.type = self }
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


  // the generated code will contain nested functions; it needs to be
  // compiled with gcc -fnested-functions
  def testCGen2 = {
    withOutFile(prefix+"cgen2") {
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {

          def fac: Rep[((Int,Int))=>Int] = fun { (n, dummy) =>
            if (n == 0) 1 else n * fac(n - 1, dummy)
          }

          printf("Hello, world! %d\n", fac(4,0))

          0
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"cgen2")
  }


}