package scala.lms
package epfl
package test12

import common._
import java.io.PrintWriter

class TestList extends FileDiffSuite {

  trait MapFlatMapAndFilter { this: ListOps with NumericOps with PrimitiveOps with OrderingOps =>
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
      List[Int]() ++ xs // VIRT 2.11: need type annotation

    def emptyRight(xs: Rep[List[Int]]): Rep[List[Int]] =
      xs ++ List[Int]()
  }

  trait MkString { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[String] =
      xs.mkString
  }

  val prefix = home + "test-out/epfl/test12-"

  def testMapFlatMapAndFilter() {
    withOutFile(prefix+"map-flatmap-filter") {
      val prog = new MapFlatMapAndFilter with ListOpsExp with SeqOpsExp with NumericOpsExp with PrimitiveOpsExp with OrderingOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps with ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenOrderingOps { val IR: prog.type = prog }
      import prog.{intTyp,unitTyp,listTyp}
      codegen.emitSource(prog.test, "MapFlatMapAndFilter", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"map-flatmap-filter")
  }

  def testConcat() {
    withOutFile(prefix+"concat") {
      val prog = new Concat with ListOpsExpOpt with SeqOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps { val IR: prog.type = prog }
      import prog.{intTyp,unitTyp,listTyp}
      codegen.emitSource(prog.test, "Concat", new PrintWriter(System.out))
      codegen.emitSource(prog.emptyLeft, "ConcatEmptyLeft", new PrintWriter(System.out))
      codegen.emitSource(prog.emptyRight, "ConcatEmptyRight", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"concat")
  }

  def testMkString() {
    withOutFile(prefix+"mkstring") {
      val prog = new MkString with ListOpsExp with SeqOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps { val IR: prog.type = prog }
      import prog.{intTyp,unitTyp,listTyp,stringTyp}
      codegen.emitSource(prog.test, "MkString", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"mkstring")
  }

}
