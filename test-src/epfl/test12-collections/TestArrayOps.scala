package scala.virtualization.lms
package epfl
package test12

import common._
import java.io.PrintWriter

class TestArrayOps extends FileDiffSuite {

  val prefix = "test-out/epfl/test12-"

  def testIntArrayCreation() {
    withOutFile(prefix+"array-seq-creation") {
      val prog = new ArrayOps with MiscOps with ArrayOpsExp with MiscOpsExp{
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit(1), unit(2), unit(3))
          println(a(unit(0)))
        }

        def g(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit('a'), unit('b'), unit('c'))
          println(a(unit(0)))
        }
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps{ val IR: prog.type = prog }
      codegen.emitSource(prog.f, "IntArrayCreation", new PrintWriter(System.out))
      codegen.emitSource(prog.g, "CharArrayCreation", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"array-seq-creation")
  }

}