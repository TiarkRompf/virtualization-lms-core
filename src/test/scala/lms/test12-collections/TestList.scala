package scala.lms
package test12

import ops._
import java.io.PrintWriter

class TestList extends FileDiffSuite {

  trait MapFlatMapAndFilter { this: ListOps with NumericOps with OrderingOps =>
    def test(xs: Rep[List[Int]]): Rep[List[Int]] = {
      for {
        x <- xs
        y <- List(unit(1), unit(2), unit(3))
        if y < unit(3)
      } yield x * y
    }
  }

  trait Concat { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[List[Int]] =
      xs ++ List(unit(1), unit(2), unit(3))

    def emptyLeft(xs: Rep[List[Int]]): Rep[List[Int]] =
      List() ++ xs

    def emptyRight(xs: Rep[List[Int]]): Rep[List[Int]] =
      xs ++ List()
  }

  trait MkString { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[String] =
      xs.mkString
  }

  val prefix = "test-out/epfl/test12-"

  def testMapFlatMapAndFilter() {
    withOutFile(prefix+"map-flatmap-filter") {
      val prog = new MapFlatMapAndFilter with ListOpsExp with NumericOpsExp with OrderingOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps with ScalaGenNumericOps with ScalaGenOrderingOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "MapFlatMapAndFilter", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"map-flatmap-filter")
  }

  def testConcat() {
    withOutFile(prefix+"concat") {
      val prog = new Concat with ListOpsExpOpt
      val codegen = new ScalaGenEffect with ScalaGenListOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "Concat", new PrintWriter(System.out))
      codegen.emitSource(prog.emptyLeft, "ConcatEmptyLeft", new PrintWriter(System.out))
      codegen.emitSource(prog.emptyRight, "ConcatEmptyRight", new PrintWriter(System.out))
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