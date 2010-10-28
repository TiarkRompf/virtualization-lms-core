package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.PrintWriter
import java.io.FileOutputStream


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
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Print(s) =>  emitValDef(sym, "println(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait Arrays extends Base with OverloadHack {
  def zeroes(n: Rep[Int]): Rep[Array[Int]]
  def infix_update(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]): Rep[Array[Int]]
  def infix_+(a: Rep[Array[Int]], b: Rep[Array[Int]])(implicit o: Overloaded1): Rep[Array[Int]]
}

trait ArraysExp extends Arrays with EffectExp {
  case class ArrayZero(n: Rep[Int]) extends Def[Array[Int]]
  case class ArrayUpdate(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]) extends Def[Array[Int]]
  case class ArrayPlus(a: Rep[Array[Int]], b: Rep[Array[Int]]) extends Def[Array[Int]]
  def zeroes(n: Rep[Int]) = ArrayZero(n)
  def infix_update(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]) = ArrayUpdate(a,x,v)
  def infix_+(a: Rep[Array[Int]], b: Rep[Array[Int]])(implicit o: Overloaded1) = ArrayPlus(a,b)
}

trait ScalaGenArrays extends ScalaGenEffect {
  val IR: ArraysExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ArrayZero(n) =>  
      emitValDef(sym, "new Array[Int](" + quote(n) + ")")
    case ArrayUpdate(a,x,v) =>  
      emitValDef(sym, quote(a) +".clone()")
      stream.println(quote(sym) + "(" + quote(x) + ") = " + quote(v))
    case ArrayPlus(a,b) =>  
      emitValDef(sym, "new Array[Int](" + quote(a) + ".length)")
      stream.println("arrayPlus("+ quote(sym) + "," + quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


trait ScalaGenArraysOpt extends ScalaGenArrays {
  val IR: ArraysExp
  import IR._
  
  def canKill(e: Exp[_], u: Sym[_]) = {
    !defuse.exists(p => p._1 == e && p._2 != u)
  }

  def tryKill(e: Exp[_], u: Sym[_]) = {
    if (!defuse.exists(p => p._1 == e && p._2 != u)) {
      defuse = defuse.filterNot(p => p._1 == e)
      true
    } else false
  }
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ArrayZero(n) =>  
      emitValDef(sym, "new Array[Int](" + quote(n) + ")")
    case ArrayUpdate(a,x,v) =>  
      if (tryKill(a, sym))
        emitValDef(sym, quote(a))
      else
        emitValDef(sym, quote(a) +".clone()")
      stream.println(quote(sym) + "(" + quote(x) + ") = " + quote(v))
    case ArrayPlus(a,b) =>
      if (tryKill(a, sym))
        emitValDef(sym, quote(a))
      else if (canKill(b, sym))
        emitValDef(sym, quote(b))
      else 
        emitValDef(sym, "new Array[Int](" + quote(a) + ".length)")
      stream.println("arrayPlus("+ quote(sym) + "," + quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}






trait NestProg extends Arith with Functions with Print {
  
  def test(x: Rep[Unit]) = {
    val f = doLambda { x: Rep[Double] =>
      val g = doLambda { y: Rep[Double] =>
        print("yo")
        y + (unit(4.0) * unit(3.0))
      }
      g
    }
    f
  }
  
}

trait LiveProg extends Arith with Arrays with Print {
  
  def test(x: Rep[Unit]) = {
    val a = zeroes(100) // allocation

    val ab = a.update(7, 42) // in place, a not used below

    val abc = ab.update(2, 42) // not in place (will copy), b is used again

    val abd = ab.update(4, 42) // in place again

    val e = abc + abd   // in place addition, dealloc one

    print(e) // dealloc the other one
  }
  
}



class TestAnalysis extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testAnalysis1 = {
    // test loop hoisting (don't have no loops right now but lambdas will do)
    withOutFile(prefix+"analysis1") {
      new NestProg with ArithExp with FunctionsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { val IR: self.type = self }
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"analysis1")
  }
  
  def testAnalysis2 = {
    withOutFile(prefix+"analysis2") {
      new LiveProg with ArithExp with ArraysExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenArrays with ScalaGenPrint { val IR: self.type = self }
        codegen.computeLiveness = true
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"analysis2")
  }

  def testAnalysis3 = {
    withOutFile(prefix+"analysis3") {
      new LiveProg with ArithExp with ArraysExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenArraysOpt with ScalaGenPrint { val IR: self.type = self }
        codegen.computeLiveness = true
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"analysis3")
  }
}