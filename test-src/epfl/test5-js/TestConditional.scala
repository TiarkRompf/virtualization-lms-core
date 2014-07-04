package scala.virtualization.lms
package epfl
package test5

import common._
import test1._

import java.io.PrintWriter
import java.io.FileOutputStream

trait JSGenEqual extends JSGenBase {
  val IR: EqualExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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

trait ScalaGenPrint extends ScalaGenEffect {
  val IR: PrintExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Print(s) =>  stream.println("println(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenPrint extends JSGenEffect {
  val IR: PrintExp
  import IR._
  
  // TODO: should have a function for this
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Print(s) =>  stream.println("document.body.appendChild(document.createElement(\"div\"))"+
        ".appendChild(document.createTextNode("+quote(s)+"))")
    case _ => super.emitNode(sym, rhs)
  }
}



trait Dom extends Base {
  // not used yet...
  type DOMObjectInternal
  type DOMObject = Rep[DOMObjectInternal]
  def document: DOMObject
  def infix_getElementById(s: Rep[String])
}





trait ConditionalProg { this: Arith with Equal with Print with IfThenElse =>
  
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



class TestConditional extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test5-"
  
  def testConditional = {
    withOutFile(prefix+"conditional") {
    
      println("-- begin")

      new ConditionalProg with ArithExpOpt with EqualExp with PrintExp
      with IfThenElseExp with CompileScala { self =>
        val codegen = new ScalaGenIfThenElse with ScalaGenArith 
        with ScalaGenEqual with ScalaGenPrint { val IR: self.type = self }
        
        val f = (x: Rep[Double]) => test(x)
        codegen.emitSource1(f, "Test", new PrintWriter(System.out))
        val g = compile1(f)
        println(g(7))
      }
    
      new ConditionalProg with IfThenElseExp with ArithExpOpt with EqualExp
      with PrintExp { self =>
        val codegen = new JSGenIfThenElse with JSGenArith 
        with JSGenEqual with JSGenPrint { val IR: self.type = self }
        
        val f = (x: Rep[Double]) => test(x)
        codegen.emitSource1(f, "main", new PrintWriter(System.out))
        codegen.emitHTMLPage(() => f(7), new PrintWriter(new FileOutputStream(prefix+"conditional.html")))
      }

      println("-- end")
    }
    assertFileEqualsCheck(prefix+"conditional")
    assertFileEqualsCheck(prefix+"conditional.html")
  }
}
