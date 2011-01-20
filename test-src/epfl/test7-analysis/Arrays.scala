package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait ArrayLoops extends Loops with OverloadHack {
  def array(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Array[Double]]
  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] // TODO: make reduce operation configurable!

  def infix_at(a: Rep[Array[Double]], i: Rep[Int]): Rep[Double]
  def infix_length(a: Rep[Array[Double]]): Rep[Int]
}


trait ArrayLoopsExp extends LoopsExp {
  
  case class ArrayElem(y: Exp[Double]) extends Def[Array[Double]]
  case class ReduceElem(y: Exp[Double]) extends Def[Double]

  case class ArrayIndex(a: Rep[Array[Double]], i: Rep[Int]) extends Def[Double]  
  
  def array(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Array[Double]] = {
    val x = fresh[Int]
    val y = f(x)
    SimpleLoop(shape, x, ArrayElem(y))
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = f(x)
    SimpleLoop(shape, x, ReduceElem(y))
  }

  def infix_at(a: Rep[Array[Double]], i: Rep[Int]): Rep[Double] = ArrayIndex(a, i)

  def infix_length(a: Rep[Array[Double]]): Rep[Int] = a match {
    case Def(SimpleLoop(s, x, y)) => s
    // TODO!
  }

}

trait ArrayLoopsFatExp extends ArrayLoopsExp with LoopsFatExp




trait ScalaGenArrayLoops extends ScalaGenLoops {
  val IR: ArrayLoopsExp
  import IR._
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayElem(y) => effectSyms(y)
    case ReduceElem(y) => effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case SimpleLoop(s,x,ArrayElem(y)) =>  
      stream.println("val " + quote(sym) + " = LoopArray("+quote(s)+") {" + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case SimpleLoop(s,x,ReduceElem(y)) =>  
      stream.println("val " + quote(sym) + " = LoopReduce("+quote(s)+") {" + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case ArrayIndex(a,i) =>  
      emitValDef(sym, quote(a) + " at " + quote(i) + "")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenArrayLoopsFat extends ScalaGenLoopsFat {
  val IR: ArrayLoopsFatExp
  import IR._
  
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case SimpleFatLoop(s,x,rhs) => 
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println("var " + quote(l) + " = new Array[]("+quote(s)+")")
          case ReduceElem(y) =>
            stream.println("var " + quote(l) + " = 0")
        }
      }
      val ii = x // was: x(i)
//      stream.println("var " + quote(ii) + " = 0")
//      stream.println("while ("+quote(ii)+" < "+quote(s)+") {")
      stream.println("for ("+quote(ii)+" <- 0 until "+quote(s)+") {")
//      for (jj <- x.drop(1)) {
//        stream.println(quote(jj)+" = "+quote(ii))
//      }
      emitFatBlock(syms(rhs))
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println(quote(l) + "("+quote(ii)+") = " + quote(getBlockResult(y)))
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
        }
      }
//      stream.println(quote(ii)+" += 1")
      stream.println("}")
    case _ => super.emitFatNode(sym, rhs)
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
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
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

