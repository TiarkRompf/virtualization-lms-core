package scala.virtualization.lms
package epfl
package test15

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream
import scala.reflect.SourceContext
import dbtoptimizer.lifters._
import org.dbtoaster.dbtoasterlib.K3Collection._

trait ExtendedGeneratorOps extends GeneratorOps with NumericOps
  with OrderingOps with PrimitiveOps with Equal with RangeOps
  with Structs with MiscOps with ArrayOps with BooleanOps {
  object Gen {
    def fSeq[A:Manifest](xs: Rep[A]*)(implicit pos: SourceContext) = fromSeq(xs)
  }

  def range(start: Rep[Int], end: Rep[Int]) : Generator[Int] = {
    val tmp = new Generator[Int]{
      def apply(f: Rep[Int] => Rep[Unit]) = {
        for(i <- start until end){
          f(i)
        }
      }
    }
    cond((start < end), tmp, emptyGen())
  }

  def fromSeq[A:Manifest](xs: Seq[Rep[A]]): Generator[A] =
    if(xs.isEmpty) emptyGen()
    else if(xs.length == 1) elGen(xs.head)
    else elGen(xs.head) ++ fromSeq(xs.tail)

  abstract class BoolGenerator[T:Manifest] extends ((Rep[T] => Rep[Unit]) => Rep[Boolean]) {self =>

    def map[U:Manifest](g: Rep[T] => Rep[U]) = new BoolGenerator[U]{
      def apply(f: Rep[U] => Rep[Unit]) = self.apply{
        x:Rep[T] => f(g(x))
      }
    }

    def filter(p: Rep[T] => Rep[Boolean]) = new BoolGenerator[T]{
      def apply(f: Rep[T] => Rep[Unit]) = self.apply{
        x:Rep[T] => if(p(x)) f(x)
      }
    }

    def ++(that: BoolGenerator[T]) = new BoolGenerator[T]{
      def apply(f: Rep[T] => Rep[Unit]) = {
        self.apply(f) && that.apply(f)
      }
    }


    def flatMap[U:Manifest](g: Rep[T] => BoolGenerator[U]) = new BoolGenerator[U]{
      def apply(f: Rep[U] => Rep[Unit]) = self.apply{ x:Rep[T] =>
        val tmp : BoolGenerator[U] = g(x)
        tmp.apply{u: Rep[U] => f(u)}
        unit(())
      }
    }
  }

  def rangeb(start: Rep[Int], end: Rep[Int]) : BoolGenerator[Int] = {
    val tmp = new BoolGenerator[Int]{
      def apply(f: Rep[Int] => Rep[Unit]) = {
        for(i <- start until end){
          f(i)
        }
        unit(true)
      }
    }
    condb((start < end), tmp, emptyGenb())
  }

  def condb[A:Manifest](cond: Rep[Boolean], a: BoolGenerator[A], b: BoolGenerator[A]) = new BoolGenerator[A]{
    def apply(f: Rep[A] => Rep[Unit]) = {
      if(cond) a(f) else b(f)
    }
  }

  def emptyGenb[A:Manifest]() = new BoolGenerator[A]{
    def apply(f: Rep[A] => Rep[Unit]) = unit(false)
  }
}

trait GeneratorProg extends ExtendedGeneratorOps with OverloadHack
  {

  type Complex = Record { val re: Double; val im: Double }
  def Complex(r: Rep[Double], i: Rep[Double]): Rep[Complex] = new Record { val re = r; val im = i }

  def infix_+(x: Rep[Complex], y: Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re + y.re, x.im + y.im)
  def infix_-(x: Rep[Complex], y: Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re - y.re, x.im - y.im)
  def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test1(start: Rep[Int], end: Rep[Int]) = {
    val g = range(start,end)

    var s = 0
    g{ x:Rep[Int] => s = x}
    s
  }

  def test2(start: Rep[Int], end: Rep[Int]) = {
    val g = range(start,end).map{x:Rep[Int] => x* unit(2)}

    var s  = 0
    g{ x:Rep[Int] => s = x }
    s
  }

  //sum
  def test3(start: Rep[Int], end: Rep[Int]) = {
    val g = range(start,end)

    var s = unit(0)
    g{ x:Rep[Int] => s = s+x }
    s
  }

  //sum of odds
  def test4(start: Rep[Int], end: Rep[Int]) = {
    val g = range(start,end).filter{x:Rep[Int] =>
      notequals(x%unit(2), unit(0))
    }

    var s = unit(0)
    g{ x:Rep[Int] => s = s+x }
    s
  }

  //concat sum ++ sum of odds
  def test5(start: Rep[Int], end: Rep[Int]) = {
    val f = range(start, end)
    val g = range(start,end).filter{x:Rep[Int] =>
      notequals(x%unit(2), unit(0))
    }

    var s = unit(0)
    (f++g){ x:Rep[Int] => s = s+x }
    s
  }

  //a flatMyap!!!
  def test6(start: Rep[Int], end: Rep[Int]) = {
    val f = range(start, end).flatMap{i:Rep[Int] =>
      range(start,i)
    }

    var s = unit(0)
    f{ x:Rep[Int] => s = s+x }
    s
  }


  //a flatMap!!!
  def test6b(start: Rep[Int], end: Rep[Int]) = {
    val f = rangeb(start, end).flatMap{i:Rep[Int] =>
      rangeb(start,i)
    }

    var s = unit(0)
    f{ x:Rep[Int] => s = s+x }
    s
  }

  //gen-ing a single elem from a list
  def test7(start: Rep[Int], end: Rep[Int]) = {
    val a : Rep[Array[Int]] = Array.u(1,2,3)
    val g = new Generator[Int]{
      def apply(f: Rep[Int] => Rep[Unit]) = {
        if(start + unit(1) == end) f(start)
      }
    }

    var s = unit(0)
    g{ x:Rep[Int] => s = a(x) }
    s
  }

  //fromSeq
  def test8(start : Rep[Int]) = {
    val g = Gen.fSeq(unit(1),unit(2),unit(3))

    var s = unit(0)
    g{ x:Rep[Int] => s = s + x }
    s
  }

  //complex example
  def test9(start: Rep[Int], end: Rep[Int]) = {
   val f = range(start, end).map{i:Rep[Int] =>
     i * unit(2)
   }.map{ i : Rep[Int] =>
     i + unit(1)
   }.map {i: Rep[Int] =>
     i * unit(3)
   }

   var s = unit(0)
   f{ x:Rep[Int] => s = s+x }
   s
  }

  //stream fusion intro example
  def test10(n: Rep[Int]) = {
    val f = for (k <- range(unit(1), n);
         m <- range(unit(1), k)
    ) yield (k * m)

    var s = unit(0)
    f{ x: Rep[Int] => s = s+x}
    s
  }

  //a map o flatten!!!
  def test11(start: Rep[Int], end: Rep[Int]) = {
    val f = range(start, end).map{i:Rep[Int] =>
      range(start,i)
    }.flatten

    var s = unit(0)
    f{ x:Rep[Int] => s = s+x }
    s
  }

}

trait ArrayProg extends ExtendedGeneratorOps {

  type Matres = Record { val rows: Int; val cols: Int; val mults: Int }
  def Matres(r: Rep[Int], c: Rep[Int], m: Rep[Int]): Rep[Matres] = new Record {
    val rows = r; val cols = c; val mults = m
  }

  type Alphabet = (Int,Int)
  type Input = Rep[Array[Alphabet]]


  def testMul(in: Input, n:Rep[Int]) = {

    def mult(l: Rep[Matres],r : Rep[Matres]) = {
      Matres(l.rows, r.cols, l.mults + r.mults +
        l.rows * l.cols * r.cols)
    }

    // a(i)(j) = a(i * (in.length + 1) + j) : this "lowering" done here to get
    // rid of some effect-related issues
    val costMatrix : Rep[Array[Matres]] = NewArray((in.length+unit(1)) * (in.length+ unit(1)))

    def el(i: Rep[Int], j: Rep[Int]) = new Generator[(Int,Int)]{
      def apply(f: Rep[Alphabet] => Rep[Unit]) =
        if(i + 1 == j) f(in(i))
    }

    def matrixEl(i: Rep[Int], j: Rep[Int]) = new Generator[Matres]{
      def apply(f: Rep[Matres] => Rep[Unit]) =
        if(i + 1 == j) {
          f(costMatrix(i * (in.length + unit(1)) + j))
        }
    }

    def single(i: Rep[Int], j: Rep[Int]): Generator[Matres] =
      el(i, j).map{x: Rep[(Int,Int)] =>
        Matres(x._1, x._2, unit(0))
      }

    def concat(i: Rep[Int], j: Rep[Int]): Generator[Matres] =
      range(i+1, j).map{k : Rep[Int] =>
        val x = costMatrix(i * (in.length + unit(1)) + k)
        val y = costMatrix(k * (in.length + unit(1)) + j)
        (x,y)
      }.map{x: Rep[(Matres,Matres)] =>
        mult(x._1,x._2)
      }


    (unit(1) until in.length + unit(1)).foreach{l =>
      (unit(0) until in.length + unit(1) -l).foreach{i =>
        val j = i+l

        val p = single(i,j) ++ concat(i,j)

        var s/*: Rep[Matres]*/ = Matres(unit(0), unit(0), unit(10000))
        p{  x: Rep[Matres] =>
          if(x.mults < readVar(s).mults){s = x}
        }
        costMatrix(i * (in.length + unit(1)) + j) = s
      }
    }

    println(costMatrix(0 * (in.length + unit(1)) + in.length))
  }
}

class TestGeneratorOps extends FileDiffSuite {

  val prefix = "test-out/epfl/test15-"

  def testgenerator1 = {
    withOutFile(prefix+"generator-simple"){
       new GeneratorProg with GeneratorOpsExp with NumericOpsExp
        with OrderingOpsExp with PrimitiveOpsExp with EqualExp with BooleanOpsExp
        with StructExp with StructExpOptCommon with ArrayOpsExp with RangeOpsExp
        with MiscOpsExp with ScalaCompile with K3PersistentCollectionExp{ self =>

        val printWriter = new java.io.PrintWriter(System.out)

        //test1: first "loop"
        val codegen = new ScalaGenGeneratorOps with ScalaGenNumericOps with ScalaGenOrderingOps
          with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenBooleanOps with ScalaGenRangeOps
          with ScalaGenArrayOps with ScalaGenStruct with ScalaGenMiscOps with ScalaGenK3PersistentCollection{ val IR: self.type = self }

        codegen.emitSource2(test1 _ , "test1", printWriter)
        codegen.emitDataStructures(printWriter)
        val source = new StringWriter
        codegen.emitDataStructures(new PrintWriter(source))
        //val testc1 = compile2s(test1, source)
        //scala.Console.println(testc1(1,11))

        //test2: a map
        codegen.emitSource2(test2 _ , "test2", printWriter)
        val testc2 = compile2(test2)
        scala.Console.println(testc2(1,11))

        //test3: a sum
        codegen.emitSource2(test3 _ , "test3", printWriter)
        val testc3 = compile2(test3)
        scala.Console.println(testc3(1,11))

        //test4: a filtersum
        codegen.emitSource2(test4 _ , "test4", printWriter)
        val testc4 = compile2(test4)
        scala.Console.println(testc4(1,11))

        //test5: a concat
        codegen.emitSource2(test5 _ , "test5", printWriter)
        val testc5 = compile2(test5)
        scala.Console.println(testc5(1,11))

        //test6: a flatMap
        codegen.emitSource2(test6 _ , "test6", printWriter)
        val testc6 = compile2(test6)
        scala.Console.println(testc6(1,6))

        //test7: single elem from Array
        // codegen.emitSource2(test7 _ , "test7", printWriter)
        // val testc7 = compile2(test7)
        // scala.Console.println(testc7(1,2))

        //test8: fromSeq
        codegen.emitSource1(test8 _ , "test8", printWriter)
        val testc8 = compile1(test8)
        scala.Console.println(testc8(1))

        //test6b: a flatMap with boolGenerators
        codegen.emitSource2(test6b _ , "test6b", printWriter)
        val testc6b = compile2(test6b)
        scala.Console.println(testc6b(1,6))

        //test9
        codegen.emitSource2(test9 _ , "test9", printWriter)
        val testc9 = compile2(test9)
        scala.Console.println(testc9(1,10))

        //test10: stream fusion intro example
        codegen.emitSource1(test10 _ , "test10", printWriter)
        val testc10 = compile1(test10)
        scala.Console.println(testc10(10))

        //test6: a map o flatten
        codegen.emitSource2(test11 _ , "test11", printWriter)
        val testc11 = compile2(test11)
        scala.Console.println(testc11(1,6))

      }
    }
    assertFileEqualsCheck(prefix+"generator-simple")
  }


  def testgenerator2 = {
    withOutFile(prefix+"generator-array"){
       new ArrayProg with GeneratorOpsExp with NumericOpsExp
        with OrderingOpsExp with PrimitiveOpsExp with EqualExp with BooleanOpsExp
        with StructExp with StructExpOptCommon with ArrayOpsExp with RangeOpsExp
        with MiscOpsExp with TupleOpsExp with ScalaCompile with K3PersistentCollectionExp{ self =>

        val printWriter = new java.io.PrintWriter(System.out)

        //test1: mat mult
        val codegen = new ScalaGenGeneratorOps with ScalaGenNumericOps
          with ScalaGenOrderingOps with ScalaGenPrimitiveOps with ScalaGenEqual
          with ScalaGenArrayOps with ScalaGenStruct with ScalaGenMiscOps with ScalaGenRangeOps
          with ScalaGenTupleOps with ScalaGenK3PersistentCollection{ val IR: self.type = self }

        codegen.emitSource2(testMul _ , "testMul", printWriter)
        codegen.emitDataStructures(printWriter)
        val source = new StringWriter
        codegen.emitDataStructures(new PrintWriter(source))

        //val testc1 = compile2s(testMul, source)
        //testc1(scala.Array((10,100),(100,5),(5,50)), 0)

      }
    }
    assertFileEqualsCheck(prefix+"generator-array")
  }
}