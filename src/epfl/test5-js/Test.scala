package scala.virtualization.lms
package epfl
package test5

import common._
import test1._

import java.io.PrintWriter
import java.io.FileOutputStream


trait JSGenEqual extends JSGenBase with EqualExp {
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, "" + quote(a) + "==" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}



trait Print extends Base {
  implicit def unit(s: String): Rep[String]
  def print(s: Rep[Any]): Rep[Unit]
}

trait PrintExp extends Print with EffectExp {
  implicit def unit(s: String): Rep[String] = Const(s)
  case class Print(s: Rep[Any]) extends Def[Unit]
  def print(s: Rep[Any]) = reflectEffect(Print(s))
}

trait ScalaGenPrint extends ScalaGenEffect with PrintExp {
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Print(s) =>  emitValDef(sym, "println(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenPrint extends JSGenEffect with PrintExp {
  // TODO: should have a function for this
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Print(s) =>  emitValDef(sym, "document.body.appendChild(document.createElement(\"div\"))"+
        ".appendChild(document.createTextNode("+quote(s)+"))")
    case _ => super.emitNode(sym, rhs)
  }
}



trait Dom extends Base {
  // not used yet...
  type DOMObjectInternal
  type DOMObject = Rep[DOMObjectInternal]
  def document: DOMObject
  def __ext__getElementById(s: Rep[String])
}





trait TestConditional { this: Arith with Equal with Print with IfThenElse =>
  
  def test(x: Rep[Double]): Rep[Double] = {
    
    print("yoyo")
    
    val z = if (x == x) {
      print("yoyo")
      print("xxx")
      print("yoyo")
      (x+4)
    } else {
      (x+6)
    }
    
    print("yyy")
    print("yoyo")
    
    z + (x + 4)
  }
  
}



object TestTestConditional {
  
  def main(args: Array[String]) = {

    println("-- begin")

    new TestConditional with ArithExpOpt with EqualExp with PrintExp
    with CompileScala with ScalaGenIfThenElse
    with ScalaGenArith with ScalaGenEqual with ScalaGenPrint
    {
      val f = (x: Rep[Double]) => test(x)
      emitScalaSource(f, "Test", new PrintWriter(System.out))
      val g = compile(f)
      println(g(7))
    }
    

    new TestConditional with ArithExpOpt with EqualExp with PrintExp
    with JSGenIfThenElse
    with JSGenArith with JSGenEqual with JSGenPrint
    {
      val f = (x: Rep[Double]) => test(x)
      emitJSSource(f, "main", new PrintWriter(System.out))
      emitHTMLPage(() => f(7), new PrintWriter(new FileOutputStream("test5.html")))
    }


    println("-- end")
  }
}