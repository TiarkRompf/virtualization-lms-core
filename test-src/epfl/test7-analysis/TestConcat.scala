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

trait CNNOpsGen extends ScalaGenEffect {
  val IR: ItemOpsExp with CNNOpsExp
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

trait DELLOpsGen extends ScalaGenEffect {
  val IR: DELLOpsExp with ItemOpsExp  
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

trait FlatMap extends Arith with ArrayLoops {
  def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]): Rep[Array[V]]
  
}
trait FlatMapO extends FlatMap with Arith with ArrayLoops with OrderingOps {
  def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]) = arrayFlat(x.length) { i => f(x.at(i)) }   
}

trait FlatMapF extends FlatMap with Arith with ArrayLoops with Print with OrderingOps {
  def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) = array(x.length)(i => f(x.at(i)))

  def flatMap[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[Array[V]]) = flatten(map(x)(x => f(x)))

  def flatten[T: Manifest](x: Rep[Array[Array[T]]]): Rep[Array[T]] =
      arrayFlat(x.length) { i => x.at(i) }   
}

// some nesting of concats
trait ConcatBench extends Arith with ArrayLoops with Print with OrderingOps with SimpleFileOps with FlatMap {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]  
 
  def ArrayM[T: Manifest](v: Rep[T]*) = array_obj_seq(v) //map(array_obj_seq(v))(x => x)

  def f01(f: Rep[SimpleFile]) = ArrayM("<a href='", f.path, "'>", f.name, "</a></br>") ++ flatMap(f.files)(f02(_))
  
  def f02(f: Rep[SimpleFile]) = ArrayM("<a href='", f.path, "'>", f.name, "</a></br>") ++ flatMap(f.files)(f03(_))
  
  def f03(f: Rep[SimpleFile]) = ArrayM("<a href='", f.path, "'>", f.name, "</a></br>") ++ flatMap(f.files)(f04(_))
  
  def f04(f: Rep[SimpleFile]) = ArrayM("<a href='", f.path, "'>", f.name, "</a></br>") ++ flatMap(f.files)(f1(_))
  
  def f1(f: Rep[SimpleFile]) = ArrayM("<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f2(_))

  def f2(f: Rep[SimpleFile]) = ArrayM("-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f3(_))

  def f3(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f4(_))

  def f4(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f5(_))

  def f5(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>") ++ flatMap(f.files)(f6(_))

  def f6(f: Rep[SimpleFile]) = ArrayM("-&nbsp;-&nbsp;-&nbsp;-&nbsp;-&nbsp;<a href='", f.path, "'>", f.name, "</a><br/>")

  def testL10(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = ArrayM("<html><body>") ++ f01(file) ++ ArrayM("</body></html>")
    print(res)
  }
  
  def testL9(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = ArrayM("<html><body>") ++ f02(file) ++ ArrayM("</body></html>")
    print(res)
  }
  def testL8(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = ArrayM("<html><body>") ++ f03(file) ++ ArrayM("</body></html>")
    print(res)
  }
  def testL7(x: Rep[Unit]) = {
    val file = SimpleFile("beep", "boop", unit(null))
    val res = ArrayM("<html><body>") ++ f04(file) ++ ArrayM("</body></html>")
    print(res)
  }
  
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
trait IMDBBench extends Arith with ArrayLoops with Print with OrderingOps with FlatMap with ItemOps with Top250Ops {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def ArrayM[T: Manifest](v: Rep[T]*) = {
      //map(array_obj_seq(v))(x => x)
      array_obj_seq(v)
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
    val res = Array("prep") ++ printMenu(page.menu) ++  Array("inp") ++ printSideMenu(page.sideBar) ++  Array("prem") ++ movies(page.movies)
    print(res)
  }
}

trait CNNOps extends Base with Variables with OverloadHack  with ItemOps {

 class CNN 

 object CNN {
 def apply(sideMenu: Rep[Array[Item]], central: Rep[Array[String]], centralBelow1: Rep[Item], centralBelow2: Rep[Item]) = cnn_obj_new(sideMenu, central, centralBelow1, centralBelow2)
 }

 implicit def repCNNToCNNOps(x: Rep[CNN]) = new cnnOpsCls(x)
 class cnnOpsCls(__x: Rep[CNN]) {
 def sideMenu = cnn_sideMenu(__x)
 def central = cnn_central(__x)
 def centralBelow1 = cnn_centralBelow1(__x)
 def centralBelow2 = cnn_centralBelow2(__x)
 }

 //object defs
 def cnn_obj_new(sideMenu: Rep[Array[Item]], central: Rep[Array[String]], centralBelow1: Rep[Item], centralBelow2: Rep[Item]): Rep[CNN]

 //class defs
 def cnn_sideMenu(__x: Rep[CNN]): Rep[Array[Item]]
 def cnn_central(__x: Rep[CNN]): Rep[Array[String]]
 def cnn_centralBelow1(__x: Rep[CNN]): Rep[Item]
 def cnn_centralBelow2(__x: Rep[CNN]): Rep[Item]
}

trait CNNOpsExp extends CNNOps with StructExp with EffectExp with BaseFatExp with  ItemOpsExp {
 def cnn_obj_new(sideMenu: Exp[Array[Item]], central: Exp[Array[String]], centralBelow1: Exp[Item], centralBelow2: Exp[Item]) = struct[CNN](ClassTag[CNN]("CNN"), Map( "sideMenu" -> sideMenu,  "central" -> central,  "centralBelow1" -> centralBelow1,  "centralBelow2" -> centralBelow2))
 def cnn_sideMenu(__x: Rep[CNN]) = field[Array[Item]](__x, "sideMenu")
 def cnn_central(__x: Rep[CNN]) = field[Array[String]](__x, "central")
 def cnn_centralBelow1(__x: Rep[CNN]) = field[Item](__x, "centralBelow1")
 def cnn_centralBelow2(__x: Rep[CNN]) = field[Item](__x, "centralBelow2")
}


trait CNNBench extends Arith with ArrayLoops with Print with OrderingOps with FlatMap with ItemOps with Top250Ops with CNNOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def ArrayM[T: Manifest](v: Rep[T]*) = {
      //map(array_obj_seq(v))(x => x)
      array_obj_seq(v)
    }

  def printSide(items: Rep[Array[Item]]) = {
    ArrayM("pre") ++ flatMap(items)(x => sideItem(x)) ++ ArrayM("post")
  }

  def sideItem(item: Rep[Item]): Rep[Array[String]] = {
    ArrayM("pre1", item.text) ++ flatMap(item.subitems)(x => ArrayM("post", x.text, "pre")) ++ ArrayM("post1")
  }

  def central(items: Rep[Array[String]]): Rep[Array[String]] = {    
    ArrayM("prec") ++ items ++ ArrayM("postc")
  }
  
  def centralBelow1(item: Rep[Item]): Rep[Array[String]] = {
    ArrayM("prec") ++ flatMap(item.subitems)(x => c1i(x)) ++ ArrayM("postc")
  }
  
  def c1i(item: Rep[Item]): Rep[Array[String]] = {
    ArrayM("prec",  item.text) ++ flatMap(item.subitems)(c1ii) ++ ArrayM("postc")
  }
  
  def c1ii(item: Rep[Item]) = {    
    ArrayM("prec",  item.text, "postc")
  }
  
  
  def centralBelow2(item: Rep[Item]) = {
    ArrayM("prec",  item.text) ++ flatMap(item.subitems)(c2i) ++ ArrayM("postc")
  }
    
  def c2i(item: Rep[Item]) = {    
    ArrayM("prec",  item.text) ++ flatMap(item.subitems)(c2ii) ++ ArrayM("postc")
  }
  
  def c2ii(item: Rep[Item]) = ArrayM("prec",  item.text, "postc")
  
  def testCNN(x: Rep[Unit]) = {
    val cnnPage = CNN(unit(null), unit(null), unit(null), unit(null))
    val res = ArrayM("prep") ++ printSide(cnnPage.sideMenu) ++ ArrayM("inp") ++ central(cnnPage.central) ++ ArrayM("prem") ++
    centralBelow1(cnnPage.centralBelow1) ++ ArrayM("postm", "pre") ++ centralBelow2(cnnPage.centralBelow2)
    print(res)
  }  
  
}

trait DELLOps extends Base with Variables with ItemOps with OverloadHack  {

 class DELL

 object DELL {
 def apply(menu: Rep[Array[Item]], center: Rep[Array[Item]], links: Rep[Array[Item]]) = dell_obj_new(menu, center, links)
 }

 implicit def repDELLToDELLOps(x: Rep[DELL]) = new dellOpsCls(x)
 class dellOpsCls(__x: Rep[DELL]) {
 def menu = dell_menu(__x)
 def center = dell_center(__x)
 def links = dell_links(__x)
 }

 //object defs
 def dell_obj_new(menu: Rep[Array[Item]], center: Rep[Array[Item]], links: Rep[Array[Item]]): Rep[DELL]

 //class defs
 def dell_menu(__x: Rep[DELL]): Rep[Array[Item]]
 def dell_center(__x: Rep[DELL]): Rep[Array[Item]]
 def dell_links(__x: Rep[DELL]): Rep[Array[Item]]
}

trait DELLOpsExp extends DELLOps with StructExp with EffectExp with BaseFatExp {
 def dell_obj_new(menu: Exp[Array[Item]], center: Exp[Array[Item]], links: Exp[Array[Item]]) = struct[DELL](ClassTag[DELL]("DELL"), Map( "menu" -> menu,  "center" -> center,  "links" -> links))
 def dell_menu(__x: Rep[DELL]) = field[Array[Item]](__x, "menu")
 def dell_center(__x: Rep[DELL]) = field[Array[Item]](__x, "center")
 def dell_links(__x: Rep[DELL]) = field[Array[Item]](__x, "links")
}


trait DELLBench extends Arith with ArrayLoops with Print with OrderingOps with FlatMap with ItemOps with Top250Ops  with DELLOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def ArrayM[T: Manifest](v: Rep[T]*) = {
      //map(array_obj_seq(v))(x => x)
      array_obj_seq(v)
    }

  def printMenu1(items: Rep[Array[Item]]) = {
    ArrayM("pre") ++ flatMap(items)(menuItem1) ++ ArrayM("post")
  }

  def menuItem1(item: Rep[Item]) = {
    ArrayM("pre1") ++ flatMap(item.subitems)(innerItem1) ++ ArrayM("post1")
  }

  def innerItem1(item: Rep[Item]) = {
    ArrayM("pre2", item.text, "post2")
  }

  def printCenter(items: Rep[Array[Item]]) = {
    ArrayM("pre") ++ flatMap(items)(centerItem) ++ ArrayM("post")
  }

  def centerItem(item: Rep[Item]) = {
    ArrayM("pre1") ++ flatMap(item.subitems)(innerCenterItem) ++ ArrayM("post1")
  }
  
  def innerCenterItem(item: Rep[Item]) = {    
    ArrayM("pre2", item.text, "post2")
  }

  def printBottomMenu(items: Rep[Array[Item]]) = {
    ArrayM("pre") ++ flatMap(items)(bottomItem) ++ ArrayM("post")
  }
  
  def bottomItem(item: Rep[Item]) = {
    ArrayM("pre1") ++ flatMap(item.subitems)(bottomInnerItem) ++ ArrayM("post1")
  }
  
  def bottomInnerItem(item: Rep[Item]) = {
    ArrayM("pre2", item.text, "post2")
  }
  
  def testDELL(x: Rep[Unit]) = {
    val dellPage = DELL(unit(null), unit(null), unit(null))
    val res = ArrayM("prep") ++ printMenu1(dellPage.menu) ++ ArrayM("inp") ++ printCenter(dellPage.center) ++ ArrayM("prem") ++ printBottomMenu(dellPage.links)
    res
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
        new ConcatProg4 with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
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
        new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL10, "timeL10C", new PrintWriter(System.out))
        }
        new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL9, "timeL9C", new PrintWriter(System.out))
        }
        new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL8, "timeL8C", new PrintWriter(System.out))
        }
        new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL7, "timeL7C", new PrintWriter(System.out))
        }
        
        new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL6, "timeL6C", new PrintWriter(System.out))
        }
        new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL5, "timeL5C", new PrintWriter(System.out))
        }
        new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL4, "timeL4C", new PrintWriter(System.out))
        }
         new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testL3, "timeL3C", new PrintWriter(System.out))
         }
         new ConcatBench with FlatMapF with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          } 
          codegen.emitSource(testL2, "timeL2C", new PrintWriter(System.out))
         }
        }
        
        new ConcatBench with FlatMapO with ArrayLoopsExp with SimpleFileOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with SimpleFileOpsGen {
            val IR: self.type = self
      		override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            fuseConcats = false
          }
          codegen.emitSource(testL10, "timeL10origC", new PrintWriter(System.out))
          codegen.emitSource(testL9, "timeL9origC", new PrintWriter(System.out))
          codegen.emitSource(testL8, "timeL8origC", new PrintWriter(System.out))
          codegen.emitSource(testL7, "timeL7origC", new PrintWriter(System.out))
          codegen.emitSource(testL6, "timeL6origC", new PrintWriter(System.out))         
          codegen.emitSource(testL5, "timeL5origC", new PrintWriter(System.out))                   
          codegen.emitSource(testL4, "timeL4origC", new PrintWriter(System.out))         
          codegen.emitSource(testL3, "timeL3origC", new PrintWriter(System.out))         
          codegen.emitSource(testL2, "timeL2origC", new PrintWriter(System.out))
        }      
    }
    assertFileEqualsCheck(prefix + "concat05")
  }
  
  def testConcat06 = {
    withOutFile(prefix + "concat06") {
      printExceptions {
        new IMDBBench with FlatMapF with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testIMDB, "imdbfC", new PrintWriter(System.out))
        }
        
        new IMDBBench with FlatMapO with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen {
            val IR: self.type = self
      		override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            fuseConcats = false
          }
          codegen.emitSource(testIMDB, "imdborigC", new PrintWriter(System.out))
        }

      }
    }
    assertFileEqualsCheck(prefix + "concat06")
  }

  def testConcat07 = {
    withOutFile(prefix + "concat07") {
      printExceptions {
        new CNNBench with FlatMapF with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with CNNOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen with CNNOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testCNN, "cnnC", new PrintWriter(System.out))
        }
        
        new CNNBench with FlatMapO with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with CNNOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen with CNNOpsGen {
            val IR: self.type = self
      		override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            fuseConcats = false
          }
          codegen.emitSource(testCNN, "cnnorigC", new PrintWriter(System.out))
        }

      }
    }
    assertFileEqualsCheck(prefix + "concat07")
  }
  
  def testConcat08 = {
    withOutFile(prefix + "concat08") {
      printExceptions {
        new DELLBench with FlatMapF with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with DELLOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen with DELLOpsGen {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(testDELL, "dellC", new PrintWriter(System.out))
        }
        
        new DELLBench with FlatMapO with ArrayLoopsExp with ItemOpsExp with Top250OpsExp with DELLOpsExp with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with IMDBOpsGen with DELLOpsGen {
            val IR: self.type = self
      		override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            override def shouldApplyConcatSink(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
            fuseConcats = false
          }
          codegen.emitSource(testDELL, "dellorigC", new PrintWriter(System.out))
        }

      }
    }
    assertFileEqualsCheck(prefix + "concat08")
  }


}