package scala.virtualization.lms
package epfl
package test7

import common._
import test1._
import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{ PrintWriter, StringWriter, FileOutputStream }
import scala.reflect.SourceContext

/**
 * Simplest concat example.
 */
trait ConcatProg extends Arith with ArrayLoops with Print {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {
    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    val constant = array(100) { i => 1 }
    val linear = array(100) { i => 2 * i }
    val constantLinear = concat(constant, linear)

    val l1 = map(constantLinear)(x => x + 1234)
    val res = map(l1)(x => x + 9876)
    print(res)
  }
}

// some nesting of concats
trait ConcatProg2 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]): Rep[Array[T]] =
      arrayFlat(x.length) { i => x.at(i) }

    def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]) =
      flatten(map(x)(x => f(x)))

    val arr: Rep[Array[Int]] = array(100) { i => i }

    // data structure
    val range: Rep[Array[Array[Array[Int]]]] = array(1000) { i => array(1000) { i => array(1000) { i => i } } }

    // template
    //    def flatIn(x: Rep[Array[Array[Int]]]) = flatMap(x)(y => y) 
    //    val res = flatMap(range)(x => concat(flatIn(x), arr))
    val res = concat(arr, flatMap(range)(x => flatMap(x) { x => x }))

    print(res)
  }
}

// some nesting of concats
trait ConcatProg3 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    val constant = array(100) { i => 1 }
    val linear = array(100) { i => 2 * i }
    val quadratic = array(100) { i => i * i }
    val clq = concat(concat(constant, linear), quadratic)

    val l1 = map(clq)(x => x + 1234)
    val res = map(l1)(x => x + 9876)
    print(res)
  }

}

trait SimpleFileOps extends Base with Variables with OverloadHack {

  class SimpleFile

  object SimpleFile {
    def apply(name: Rep[String], path: Rep[String], files: Rep[Array[SimpleFile]]) = simplefile_obj_new(name, path, files)
  }

  implicit def repSimpleFileToSimpleFileOps(x: Rep[SimpleFile]) = new simplefileOpsCls(x)
  class simplefileOpsCls(__x: Rep[SimpleFile]) {
    def name = simplefile_name(__x)
    def path = simplefile_path(__x)
    def files = simplefile_files(__x)
  }

  //object defs
  def simplefile_obj_new(name: Rep[String], path: Rep[String], files: Rep[Array[SimpleFile]]): Rep[SimpleFile]

  //class defs
  def simplefile_name(__x: Rep[SimpleFile]): Rep[String]
  def simplefile_path(__x: Rep[SimpleFile]): Rep[String]
  def simplefile_files(__x: Rep[SimpleFile]): Rep[Array[SimpleFile]]
}

trait SimpleFileOpsExp extends SimpleFileOps with StructExp with EffectExp with BaseFatExp {
  def simplefile_obj_new(name: Exp[String], path: Exp[String], files: Exp[Array[SimpleFile]]) = struct[SimpleFile](ClassTag[SimpleFile]("SimpleFile"), Map("name" -> name, "path" -> path, "files" -> files))
  def simplefile_name(__x: Rep[SimpleFile]) = field[String](__x, "name")
  def simplefile_path(__x: Rep[SimpleFile]) = field[String](__x, "path")
  def simplefile_files(__x: Rep[SimpleFile]) = field[Array[SimpleFile]](__x, "files")
}

trait SimpleFileOpsGen extends ScalaGenEffect {
  val IR: SimpleFileOpsExp 
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IR.Field(tuple, x, tp) => emitValDef(sym, "%s.%s".format(quote(tuple), x))
    case IR.SimpleStruct(IR.ClassTag(name), map) => {
      try {
        val args = map.map(x => x._1 + " = " + quote(x._2))
        emitValDef(sym, "new %s(%s)".format(name, args.mkString(", ")))
      } catch {
        case e =>
          emitValDef(sym, "Exception " + e + " when accessing  of " + name)
          e.printStackTrace
      }
    }

    case _ => super.emitNode(sym, rhs)
  }
}

// some nesting of concats
trait ConcatProg4 extends Arith with ArrayLoops with Print with OrderingOps with SimpleFileOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    val file = SimpleFile("bla", "bla", unit(null))

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) = array(x.length)(i => f(x.at(i)))

    def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]) = flatten(map(x)(x => f(x)))

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]): Rep[Array[T]] =
      arrayFlat(x.length) { i => x.at(i) }

    def f1(f: Rep[SimpleFile]) = flatMap(f.files)(f2(_))

    def f2(f: Rep[SimpleFile]) = flatMap(f.files)(f3(_))

    def f3(f: Rep[SimpleFile]) = flatMap(f.files)(f4(_))

    def f4(f: Rep[SimpleFile]) = flatMap(f.files)(f5(_))

    def f5(f: Rep[SimpleFile]) = flatMap(f.files)(f6(_))

    def f6(f: Rep[SimpleFile]) = Array(f.path)

    val res = f1(file)
    print(res)
  }
}

// some nesting of concats
trait ConcatProg5 extends Arith with ArrayLoops with Print with OrderingOps with SimpleFileOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    val file = SimpleFile("bla", "bla", unit(null))

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) = array(x.length)(i => f(x.at(i)))

    def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]) = flatten(map(x)(x => f(x)))

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]): Rep[Array[T]] =
      arrayFlat(x.length) { i => x.at(i) }

    def ArrayM[T: Manifest](v: Rep[T]) = {
      map(Array(v))(x => x)
    }

    def f1(f: Rep[SimpleFile]) = concat(Array("<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f2(_)))

    def f2(f: Rep[SimpleFile]) = concat(Array("-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f3(_)))

    def f3(f: Rep[SimpleFile]) = concat(Array("-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f4(_)))

    def f4(f: Rep[SimpleFile]) = concat(Array("-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f5(_)))

    def f5(f: Rep[SimpleFile]) = concat(Array("-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f6(_)))

    def f6(f: Rep[SimpleFile]) = Array("-&nbsp;-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>")

    val res = concat(Array("<html><body>"), f1(file), Array("</body></html>"))
    print(res)
  }

}

// some nesting of concats
trait ConcatBench extends Arith with ArrayLoops with Print with OrderingOps with SimpleFileOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) = array(x.length)(i => f(x.at(i)))

  def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]) = flatten(map(x)(x => f(x)))

  def flatten[T: Manifest](x: Rep[Array[Array[T]]]): Rep[Array[T]] =
      arrayFlat(x.length) { i => x.at(i) }
  
  def ArrayM[T: Manifest](v: Rep[T]*) = {
      map(array_obj_seq(v))(x => x)
    }

  def f1(f: Rep[SimpleFile]) = concat(ArrayM("<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f2(_)))

  def f2(f: Rep[SimpleFile]) = concat(Array("-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f3(_)))

  def f3(f: Rep[SimpleFile]) = concat(Array("-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f4(_)))

  def f4(f: Rep[SimpleFile]) = concat(Array("-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f5(_)))

  def f5(f: Rep[SimpleFile]) = concat(Array("-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>"), flatMap(f.files)(f6(_)))

  def f6(f: Rep[SimpleFile]) = Array("-&nbsp;-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>")

  
  def test1(x: Rep[Unit]) = {
    val file = SimpleFile("bla", "bla", unit(null))
    val res = concat(Array("<html><body>"), f1(file), Array("</body></html>"))
    print(res)
  }
  
  def test2(x: Rep[Unit]) = {
    val file = SimpleFile("bla", "bla", unit(null))
    val res = concat(Array("<html><body>"), f2(file), Array("</body></html>"))
    print(res)
  }
  
  def test3(x: Rep[Unit]) = {
    val file = SimpleFile("bla", "bla", unit(null))
    val res = concat(Array("<html><body>"), f3(file), Array("</body></html>"))
    print(res)
  }
  
  def test4(x: Rep[Unit]) = {
    val file = SimpleFile("bla", "bla", unit(null))
    val res = concat(Array("<html><body>"), f4(file), Array("</body></html>"))
    print(res)
  }
  
  def test5(x: Rep[Unit]) = {
    val file = SimpleFile("bla", "bla", unit(null))
    val res = concat(Array("<html><body>"), f5(file), Array("</body></html>"))
    print(res)
  }
}



class TestConcat extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-"

  private[this] def printExceptions(b: => Unit) = {
    try b catch {
      case e =>
        val writer = new PrintWriter(System.out)
        e.printStackTrace(writer)
        writer.flush
    }
  }

  def testConcat01 = {
    withOutFile(prefix + "concat01") {
      printExceptions {
        new ConcatProg with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with TransformingStuff { self =>
          override val verbosity = 1
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "concat01")
  }

  def testConcat02 = {
    withOutFile(prefix + "concat02") {
      printExceptions {
        new ConcatProg2 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 1
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "concat02")
  }

  def testConcat03 = {
    withOutFile(prefix + "concat03") {
      printExceptions {
        new ConcatProg3 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 1
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "concat03")
  }

  def testConcat04 = {
    withOutFile(prefix + "concat04") {
      printExceptions {
        new ConcatProg5 with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 1
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "concat04")
  }
  
  def testConcat05 = {
    withOutFile(prefix + "concat05") {
      printExceptions {
        new ConcatBench with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test1, "time1", new PrintWriter(System.out))
          codegen.emitSource(test2, "time2", new PrintWriter(System.out))
          codegen.emitSource(test3, "time3", new PrintWriter(System.out))
          codegen.emitSource(test4, "time4", new PrintWriter(System.out))
          codegen.emitSource(test5, "time5", new PrintWriter(System.out))
        }
        
        new ConcatBench with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
      		override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            fuseConcats = false
          }
          codegen.emitSource(test1, "time1orig", new PrintWriter(System.out))
          codegen.emitSource(test2, "time2orig", new PrintWriter(System.out))
          codegen.emitSource(test3, "time3orig", new PrintWriter(System.out))
          codegen.emitSource(test4, "time4orig", new PrintWriter(System.out))
          codegen.emitSource(test5, "time5orig", new PrintWriter(System.out))
        }

      }
    }
    assertFileEqualsCheck(prefix + "concat05")
  }
  

}