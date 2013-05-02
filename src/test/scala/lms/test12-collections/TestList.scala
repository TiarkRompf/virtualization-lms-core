package scala.lms
package test12

import ops._

import java.io.PrintWriter

trait ListOptProg extends ListOps with MiscOps with LiftBoolean with LiftNumeric with Equal {
  def testList(op: Rep[List[Int]] => Rep[Any], i: Rep[Int], exp: Rep[List[Int]] => Rep[Any], expi: Rep[List[Int]] => Rep[Any], exp1: Rep[List[Int]] => Rep[Any], exp11: Rep[List[Int]] => Rep[Any], exp1i: Rep[List[Int]] => Rep[Any], expOld: Rep[List[Int]] => Rep[Any]) = {
    val list: Rep[List[Int]] = List()
    val listi: Rep[List[Int]] = List(i)
    val list1: Rep[List[Int]] = List(1)
    val list11: Rep[List[Int]] = List(1, 1)
    val list1i: Rep[List[Int]] = List(1, i)
    val listOld: Rep[List[Int]] = 1 :: List(1)
    
    println(op(list) == exp(list))
    println(op(listi) == expi(listi))
    println(op(list1) == exp1(list1))
    println(op(list11) == exp11(list11))
    println(op(list1i) == exp1i(list1i))
    println(op(listOld) == expOld(listOld))
  }
  def test1(i: Rep[Int]) = {
    testList({l => l.isEmpty}, i, {l => true}, {l => false}, {l => false}, {l => false}, {l => false}, {l => l.isEmpty})
    testList({l => l.head}, i, {l => l.head}, {l => i}, {l => 1}, {l => 1}, {l => 1}, {l => l.head})
    testList({l => l.tail}, i, {l => l.tail}, {l => List()}, {l => List()}, {l => List(1)}, {l => List(i)}, {l => l.tail})
  }
}

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
      val prog = new MapFlatMapAndFilter with ListOpsExp with NumericOpsExp with PrimitiveOpsExp with OrderingOpsExp
      val codegen = new ScalaGenEffect with ScalaGenListOps with ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenOrderingOps { val IR: prog.type = prog }
      import prog.typeRepFromManifest
      codegen.emitSource(prog.test, "MapFlatMapAndFilter", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"map-flatmap-filter")
  }

  def testConcat() {
    withOutFile(prefix+"concat") {
      val prog = new Concat with ListOpsExpOpt
      val codegen = new ScalaGenEffect with ScalaGenListOps { val IR: prog.type = prog }
      import prog.typeRepFromManifest
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
      import prog.typeRepFromManifest
      codegen.emitSource(prog.test, "MkString", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"mkstring")
  }
  
  def testListOpsExpOpt1 = {
    withOutFile(prefix+"listopt1"){
      new ListOptProg with ListOpsExp with ListOpsExpOpt with MiscOpsExp with EqualExp with EqualExpOpt { self =>
        val codegen = new ScalaGenEffect with ScalaGenListOps with ScalaGenMiscOps with ScalaGenEqual { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new java.io.PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"listopt1")
  }
}
