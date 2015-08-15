package scala.lms
package epfl
package test12

import common._
import java.io.PrintWriter

class TestArrayOps extends FileDiffSuite {

  val prefix = home + "test-out/epfl/test12-"

  def testIntArrayCreation() {
    withOutFile(prefix+"array-seq-creation") {
      val prog = new LiftArrays with ArrayOps with MiscOps with ArrayOpsExp with SeqOpsExp with StringOpsExp with MiscOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit(1), unit(2), unit(3))
          println(a(unit(0)))
        }

        def g(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit('a'), unit('b'), unit('c'))
          println(a(unit(0)))
        }
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps { val IR: prog.type = prog }
      import prog.{intTyp,unitTyp}
      codegen.emitSource(prog.f, "IntArrayCreation", new PrintWriter(System.out))
      codegen.emitSource(prog.g, "CharArrayCreation", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"array-seq-creation")
  }

}