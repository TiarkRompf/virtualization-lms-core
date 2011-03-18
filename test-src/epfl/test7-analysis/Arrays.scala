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
  def arrayIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Array[Double]]
  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] // TODO: make reduce operation configurable!
  def flatten(shape: Rep[Int])(f: Rep[Int] => Rep[Array[Double]]): Rep[Array[Double]]

  def infix_at(a: Rep[Array[Double]], i: Rep[Int]): Rep[Double]
  def infix_length(a: Rep[Array[Double]]): Rep[Int]
}


trait ArrayLoopsExp extends LoopsExp {
  
  case class ArrayElem(y: Exp[Double]) extends Def[Array[Double]]
  case class ReduceElem(y: Exp[Double]) extends Def[Double]

  case class ArrayIfElem(c: Exp[Boolean], y: Exp[Double]) extends Def[Array[Double]]
  case class ReduceIfElem(c: Exp[Boolean], y: Exp[Double]) extends Def[Double]

  case class FlattenElem(y: Exp[Array[Double]]) extends Def[Array[Double]]

  case class ArrayIndex(a: Rep[Array[Double]], i: Rep[Int]) extends Def[Double]  
  case class ArrayLength(a: Rep[Array[Double]]) extends Def[Int]
  
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

  def arrayIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Array[Double]] = {
    val x = fresh[Int]
    val (c,y) = f(x)
    SimpleLoop(shape, x, ArrayIfElem(c,y)) // TODO: simplify for const true/false
  }

  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    val (c,y) = f(x)
    SimpleLoop(shape, x, ReduceIfElem(c,y)) // TODO: simplify for const true/false
  }

  def flatten(shape: Rep[Int])(f: Rep[Int] => Rep[Array[Double]]): Rep[Array[Double]] = {
    val x = fresh[Int]
    val y = f(x)
    SimpleLoop(shape, x, FlattenElem(y))
  }


  def infix_at(a: Rep[Array[Double]], i: Rep[Int]): Rep[Double] = ArrayIndex(a, i)

  def infix_length(a: Rep[Array[Double]]): Rep[Int] = a match {
    case Def(SimpleLoop(s, x, ArrayElem(y))) => s
    case _ => ArrayLength(a)
  }

}

trait ArrayLoopsFatExp extends ArrayLoopsExp with LoopsFatExp




trait ScalaGenArrayLoops extends ScalaGenLoops {
  val IR: ArrayLoopsExp
  import IR._
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayElem(y) => effectSyms(y)
    case ReduceElem(y) => effectSyms(y)
    case FlattenElem(y) => effectSyms(y)
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
    // TODO: conditional variants ...
    case SimpleLoop(s,x,FlattenElem(y)) =>  
      stream.println("val " + quote(sym) + " = LoopFlatten("+quote(s)+") {" + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case ArrayIndex(a,i) =>  
      emitValDef(sym, quote(a) + ".apply(" + quote(i) + ")")
    case ArrayLength(a) =>  
      emitValDef(sym, quote(a) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenArrayLoopsFat extends ScalaGenArrayLoops with ScalaGenLoopsFat {
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
          case ArrayIfElem(c,y) =>
            stream.println("var " + quote(l) + " = new ArrayBuilder[]")
          case ReduceIfElem(c,y) =>
            stream.println("var " + quote(l) + " = 0")
          case FlattenElem(y) =>
            stream.println("var " + quote(l) + " = new ArrayBuilder[]")
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
          case ArrayIfElem(c,y) =>
            stream.println("if ("+quote(getBlockResult(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case ReduceIfElem(c,y) =>
            stream.println("if ("+quote(getBlockResult(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case FlattenElem(y) =>
            stream.println(quote(l) + " ++= " + quote(getBlockResult(y)))
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

