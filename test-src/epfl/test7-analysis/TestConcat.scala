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

trait IMDBOpsGen extends ScalaGenEffect {
  val IR: ItemOpsExp with Top250OpsExp
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

    val file = SimpleFile("beep", "boop", unit(null))

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

    val file = SimpleFile("beep", "boop", unit(null))

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
  
  def ArrayM[T: Manifest](v: Rep[T]*) = array_obj_seq(v) //map(array_obj_seq(v))(x => x)

  def f1(f: Rep[SimpleFile]) = ArrayM("<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f2(_))

  def f2(f: Rep[SimpleFile]) = ArrayM("-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f3(_))

  def f3(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f4(_))

  def f4(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f5(_))

  def f5(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f6(_))

  def f6(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>")

  
  def testL6(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = ArrayM("<html><body>") ++ f1(file) ++ ArrayM("</body></html>")
    print(res)
  }
  
  def testL5(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = Array("<html><body>") ++ f2(file) ++ Array("</body></html>")
    print(res)
  }
  
  def testL4(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = Array("<html><body>") ++ f3(file) ++ Array("</body></html>")
    print(res)
  }
  
  def testL3(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = Array("<html><body>") ++ f4(file) ++ Array("</body></html>")
    print(res)
  }
  
  def testL2(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = Array("<html><body>") ++ f5(file) ++ Array("</body></html>")
    print(res)
  }
}

trait IMDBOps extends ItemOps with Top250Ops
trait IMDBExp extends ItemOpsExp with Top250OpsExp

trait ItemOps extends Base with Variables with OverloadHack  {

 class Item 

 object Item {
 def apply(text: Rep[String], subitems: Rep[Array[Item]]) = item_obj_new(text, subitems)
 }

 implicit def repItemToItemOps(x: Rep[Item]) = new itemOpsCls(x)
 class itemOpsCls(__x: Rep[Item]) {
 def text = item_text(__x)
 def subitems = item_subitems(__x)
 }

 //object defs
 def item_obj_new(text: Rep[String], subitems: Rep[Array[Item]]): Rep[Item]

 //class defs
 def item_text(__x: Rep[Item]): Rep[String]
 def item_subitems(__x: Rep[Item]): Rep[Array[Item]]
}

trait ItemOpsExp extends ItemOps with StructExp with EffectExp with BaseFatExp {
 def item_obj_new(text: Exp[String], subitems: Exp[Array[Item]]) = struct[Item](ClassTag[Item]("Item"), Map( "text" -> text,  "subitems" -> subitems))
 def item_text(__x: Rep[Item]) = field[String](__x, "text")
 def item_subitems(__x: Rep[Item]) = field[Array[Item]](__x, "subitems")
}
trait Top250Ops extends Base with Variables with OverloadHack  with ItemOps {
class Top250 

 object Top250 {
 def apply(menu: Rep[Array[Item]], movies: Rep[Array[Array[String]]], sideBar: Rep[Array[Item]]) = top250_obj_new(menu, movies, sideBar)
 }

 implicit def repTop250ToTop250Ops(x: Rep[Top250]) = new top250OpsCls(x)
 class top250OpsCls(__x: Rep[Top250]) {
 def menu = top250_menu(__x)
 def movies = top250_movies(__x)
 def sideBar = top250_sideBar(__x)
 }

 //object defs
 def top250_obj_new(menu: Rep[Array[Item]], movies: Rep[Array[Array[String]]], sideBar: Rep[Array[Item]]): Rep[Top250]

 //class defs
 def top250_menu(__x: Rep[Top250]): Rep[Array[Item]]
 def top250_movies(__x: Rep[Top250]): Rep[Array[Array[String]]]
 def top250_sideBar(__x: Rep[Top250]): Rep[Array[Item]]
}

trait Top250OpsExp extends Top250Ops with StructExp with EffectExp with BaseFatExp with ItemOpsExp {
 def top250_obj_new(menu: Exp[Array[Item]], movies: Exp[Array[Array[String]]], sideBar: Exp[Array[Item]]) = struct[Top250](ClassTag[Top250]("Top250"), Map( "menu" -> menu,  "movies" -> movies,  "sideBar" -> sideBar))
 def top250_menu(__x: Rep[Top250]) = field[Array[Item]](__x, "menu")
 def top250_movies(__x: Rep[Top250]) = field[Array[Array[String]]](__x, "movies")
 def top250_sideBar(__x: Rep[Top250]) = field[Array[Item]](__x, "sideBar")
}




// some nesting of concats
trait IMDBBench extends Arith with ArrayLoops with Print with OrderingOps with ItemOps with Top250Ops {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) = array(x.length)(i => f(x.at(i)))

  def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]) = flatten(map(x)(x => f(x)))

  def flatten[T: Manifest](x: Rep[Array[Array[T]]]): Rep[Array[T]] =
      arrayFlat(x.length) { i => x.at(i) }
  
  def ArrayM[T: Manifest](v: Rep[T]*) = {
      map(array_obj_seq(v))(x => x)
    }

  def printMenu(items: Rep[Array[Item]]) = {
    Array("pre") ++ flatMap(items)(x => menuItem(x)) ++ Array("post")    
  }
  
  def menuItem(item: Rep[Item]) = {
    Array("pre1") ++ flatMap(item.subitems)(x => innerItem(x)) ++ Array("post1")
  }
  
  def innerItem(item: Rep[Item]) = {
    Array("pre2", item.text, "post2")
  }
  
  def printSideMenu(items: Rep[Array[Item]]) = {
    Array("pres") ++ flatMap(items)(x => sideMenuItem(x)) ++ Array("posts")    
  }
  
  def sideMenuItem(item: Rep[Item]) = {
    Array("pres1") ++ flatMap(item.subitems)(x => innerItem(x)) ++ Array("posts1")
  }
  
  def sideInnerItem(item: Rep[Item]) = {
    Array("pres2", item.text, "posts2")
  }
  
  def movies(movies: Rep[Array[Array[String]]]) = {
    Array("prem") ++ flatMap(movies)(x => movie(x)) ++ Array("postm")
  }
  
  def movie(movie: Rep[Array[String]]) = 
    Array("prem1") ++ movie ++ Array("postm1")
  
  def testIMDB(x: Rep[Unit]) = {
    val page = Top250(unit(null), unit(null), unit(null))
    val res = ArrayM("prep") ++ printMenu(page.menu) ++  ArrayM("inp") ++ printSideMenu(page.sideBar) ++  ArrayM("prem") ++ movies(page.movies)
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
          codegen.emitSource(testL6, "timeL6", new PrintWriter(System.out))
        }
        new ConcatBench with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL5, "timeL5", new PrintWriter(System.out))
        }
        new ConcatBench with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL4, "timeL4", new PrintWriter(System.out))
        }
         new ConcatBench with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL3, "timeL3", new PrintWriter(System.out))
         }
         new ConcatBench with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          } 
          codegen.emitSource(testL2, "timeL2", new PrintWriter(System.out))
         }
        }
        
        new ConcatBench with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
      		override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            fuseConcats = false
          }
          codegen.emitSource(testL6, "timeL6orig", new PrintWriter(System.out))         
          codegen.emitSource(testL5, "timeL5orig", new PrintWriter(System.out))                   
          codegen.emitSource(testL4, "timeL4orig", new PrintWriter(System.out))         
          codegen.emitSource(testL3, "timeL3orig", new PrintWriter(System.out))         
          codegen.emitSource(testL2, "timeL2orig", new PrintWriter(System.out))
        }      
    }
    assertFileEqualsCheck(prefix + "concat05")
  }
  
  def testConcat06 = {
    withOutFile(prefix + "concat06") {
      printExceptions {
        new IMDBBench with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testIMDB, "imdbf", new PrintWriter(System.out))
        }
        
        new IMDBBench with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen {
            val IR: self.type = self
      		override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            fuseConcats = false
          }
          codegen.emitSource(testIMDB, "imdborig", new PrintWriter(System.out))
        }

      }
    }
    assertFileEqualsCheck(prefix + "concat06")
  }


}