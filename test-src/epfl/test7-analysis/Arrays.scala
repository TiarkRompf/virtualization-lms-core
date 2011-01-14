package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait Loops extends Base with OverloadHack {
  def array(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Array[Double]]
  def reduce(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] // TODO: make reduce operation configurable!

  def infix_at(a: Rep[Array[Double]], i: Rep[Int]): Rep[Double]
  def infix_length(a: Rep[Array[Double]]): Rep[Int]
}


trait LoopsExp extends Loops with EffectExp {
  
  case class LoopArray(s: Rep[Int], x: Sym[Int], y: Rep[Double]) extends Def[Array[Double]]
  case class LoopReduce(s: Rep[Int], x: Sym[Int], y: Rep[Double]) extends Def[Double]
  case class ArrayIndex(a: Rep[Array[Double]], i: Rep[Int]) extends Def[Double]  
  
  def array(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Array[Double]] = {
    val x = fresh[Int]
    val y = f(x)
    LoopArray(shape, x, y)
  }

  def reduce(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = f(x)
    LoopReduce(shape, x, y)
  }

  def infix_at(a: Rep[Array[Double]], i: Rep[Int]): Rep[Double] = ArrayIndex(a, i)

  def infix_length(a: Rep[Array[Double]]): Rep[Int] = a match {
    case Def(LoopArray(s, x, y)) => s
    // TODO!
  }


}

trait ScalaGenLoops extends ScalaGenEffect {
  val IR: LoopsExp
  import IR._
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case LoopArray(s, x, y) => syms(s):::syms(y)
    case LoopReduce(s, x, y) => syms(s):::syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match { // treat effects as bound symbols, just like the fun param
    case LoopArray(s, x, y) => x :: effectSyms(y)
    case LoopReduce(s, x, y) => x :: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case LoopArray(s,x,y) =>  
      stream.println("val " + quote(sym) + " = LoopArray("+quote(s)+") {" + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case LoopReduce(s,x,y) =>  
      stream.println("val " + quote(sym) + " = LoopReduce("+quote(s)+") {" + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case ArrayIndex(a,i) =>  
      emitValDef(sym, quote(a) + " at " + quote(i) + "")
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

