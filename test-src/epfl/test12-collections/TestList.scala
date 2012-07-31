package scala.virtualization.lms
package epfl
package test12

import common._
import java.io.PrintWriter

class TestList extends FileDiffSuite {

  trait MapAndFlatMap { this: ListOps with NumericOps =>
    def test(xs: Rep[List[Int]]): Rep[List[Int]] = {
      for {
        x <- xs
        y <- List(unit(1), unit(2), unit(3))
      } yield x * y
    }
  }

  trait Concat { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[List[Int]] =
      xs ++ List(unit(1), unit(2), unit(3))
  }

  trait MkString { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[String] =
      xs.mkString
  }

  val prefix = "test-out/epfl/test12-"

  def testMapAndFlatMap() {
    withOutFile(prefix+"map-flatmap") {
      val prog = new MapAndFlatMap with ListOpsExp with NumericOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps with ScalaGenNumericOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "MapAndFlatMap", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"map-flatmap")
  }

  def testConcat() {
    withOutFile(prefix+"concat") {
      val prog = new Concat with ListOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "Concat", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"concat")
  }

  def testMkString() {
    withOutFile(prefix+"mkstring") {
      val prog = new MkString with ListOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "MkString", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"mkstring")
  }

}