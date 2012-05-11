package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

import scala.collection.immutable.HashMap


trait HashLoops extends ArrayLoops with OverloadHack {
  
//  def hashCount[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[HashMap[T,Int]]

  def hashArray[K:Manifest,T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[K],Rep[T])): Rep[HashMap[K,Array[T]]]

  def hashSum[K:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[K],Rep[Double])): Rep[HashMap[K,Double]]
  
/*  
  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]]
  
  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] // TODO: make reduce operation configurable!
  def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]]
  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] // TODO: make reduce operation configurable!
*/

  def infix_keyAt[K:Manifest,T:Manifest](a: Rep[HashMap[K,T]], i: Rep[Int]): Rep[K]
  def infix_valueAt[K:Manifest,T:Manifest](a: Rep[HashMap[K,T]], i: Rep[Int]): Rep[T]
  def infix_length[K:Manifest,T:Manifest](a: Rep[HashMap[K,T]]): Rep[Int]

  def infix_lookup[T:Manifest,U:Manifest](a: Rep[HashMap[T,U]], k: Rep[T]): Rep[U]

}



trait HashLoopsExp extends HashLoops with ArrayLoopsExp {
  
/*
  case class ArrayElem[T](y: Exp[T]) extends Def[Array[T]]        DeliteCollectElem
  case class ReduceElem(y: Exp[Double]) extends Def[Double]       DeliteReduceElem

  case class ArrayIfElem[T](c: Exp[Boolean], y: Exp[T]) extends Def[Array[T]]
  case class ReduceIfElem(c: Exp[Boolean], y: Exp[Double]) extends Def[Double]

  case class FlattenElem[T](y: Exp[Array[T]]) extends Def[Array[T]]

  case class ArrayIndex[T](a: Rep[Array[T]], i: Rep[Int]) extends Def[T]  
  case class ArrayLength[T](a: Rep[Array[T]]) extends Def[Int]
*/

  case class HashElem[K,T](k: Exp[K], y: Exp[T]) extends Def[HashMap[K,T]]                    //  DeliteHashCllectElem
  case class HashArrayElem[K,T](k: Exp[K], y: Exp[T]) extends Def[HashMap[K,Array[T]]]        //  DeliteBucketCollectElem
  case class HashReduceElem[K](k: Exp[K], y: Exp[Double]) extends Def[HashMap[K,Double]]      //  DeliteBucketReduceElem

  case class HashKeyIndex[K,T](a: Rep[HashMap[K,T]], i: Rep[Int]) extends Def[K]
  case class HashValueIndex[K,T](a: Rep[HashMap[K,T]], i: Rep[Int]) extends Def[T]
  case class HashLength[T,U](a: Rep[HashMap[T,U]]) extends Def[Int]

  case class HashLookup[K,T](a: Rep[HashMap[K,T]], k: Rep[K]) extends Def[T]
    

//    def hashCount[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[HashMap[T,Int]] = 

  def hashArray[K:Manifest,T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[K],Rep[T])): Rep[HashMap[K,Array[T]]] = {
    val x = fresh[Int]
    val (k,y) = f(x)
    SimpleLoop(shape, x, HashArrayElem(k,y))
  }

  def hashSum[K:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[K],Rep[Double])): Rep[HashMap[K,Double]] = {
    val x = fresh[Int]
    val (k,y) = f(x)
    SimpleLoop(shape, x, HashReduceElem(k,y))
  }

    
/*    
  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = f(x)
    SimpleLoop(shape, x, ArrayElem(y))
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = f(x)
    SimpleLoop(shape, x, ReduceElem(y))
  }

  def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]] = {
    val x = fresh[Int]
    val (c,y) = f(x)
    SimpleLoop(shape, x, ArrayIfElem(c,y)) // TODO: simplify for const true/false
  }

  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    val (c,y) = f(x)
    SimpleLoop(shape, x, ReduceIfElem(c,y)) // TODO: simplify for const true/false
  }

  def flatten[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = f(x)
    SimpleLoop(shape, x, FlattenElem(y))
  }

*/


  def infix_keyAt[K:Manifest,T:Manifest](a: Rep[HashMap[K,T]], i: Rep[Int]): Rep[K] = HashKeyIndex(a,i)
  def infix_valueAt[K:Manifest,T:Manifest](a: Rep[HashMap[K,T]], i: Rep[Int]): Rep[T] = HashValueIndex(a,i)

  def infix_lookup[K:Manifest,T:Manifest](a: Rep[HashMap[K,T]], k: Rep[K]): Rep[T] = HashLookup(a,k)

  def infix_length[K:Manifest,T:Manifest](a: Rep[HashMap[K,T]]): Rep[Int] = a match {
    //case Def(SimpleLoop(s, x, HashArrayElem(k,y))) => s //FIXME: yes or no???
    case _ => HashLength(a)
  }


  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case HashArrayElem(k,y) => effectSyms(y)
    case HashReduceElem(k,y) => effectSyms(y)
    case _ => super.boundSyms(e)
  }

}

trait HashLoopsFatExp extends HashLoopsExp with ArrayLoopsFatExp




trait ScalaGenHashLoops extends ScalaGenArrayLoops {
  val IR: HashLoopsExp
  import IR._
  case class Combine2(a:Any,b:Any)extends Exp[Any]
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SimpleLoop(s,x,HashArrayElem(k,y)) =>  
      stream.println("val " + quote(sym) + " = LoopHashArray("+quote(s)+") { " + quote(x) + " => ")
//      emitBlock(Block(Combine2(k,y)))
//      stream.println(quote(getBlockResult(k))+"->"+quote(getBlockResult(y)))
      stream.println("}")
    case SimpleLoop(s,x,HashReduceElem(k,y)) =>  
      stream.println("val " + quote(sym) + " = LoopHashReduce("+quote(s)+") { " + quote(x) + " => ")
//      emitBlock(Block(Combine2(k,y)))
//      stream.println(quote(getBlockResult(k))+"->"+quote(getBlockResult(y)))
      stream.println("}")
    // TODO: conditional variants ...
    case HashKeyIndex(a,i) =>  
      emitValDef(sym, quote(a) + ".keyAt(" + quote(i) + ")")
    case HashValueIndex(a,i) =>  
      emitValDef(sym, quote(a) + ".valueAt(" + quote(i) + ")")
    case HashLength(a) =>  
      emitValDef(sym, quote(a) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenHashLoopsFat extends ScalaGenHashLoops with ScalaGenArrayLoopsFat {
  val IR: HashLoopsFatExp
  import IR._
  
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(s,x,rhs) => 
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(g,y) =>
            stream.println("var " + quote(l) + " = new Array[]("+quote(s)+")")
          case ReduceElem(g,y) =>
            stream.println("var " + quote(l) + " = 0")
          //case ArrayIfElem(g,c,y) =>
          //  stream.println("var " + quote(l) + " = new ArrayBuilder[]")
          //case ReduceIfElem(g,c,y) =>
          //  stream.println("var " + quote(l) + " = 0")
          //case FlattenElem(g,y) =>
          //  stream.println("var " + quote(l) + " = new ArrayBuilder[]")
        }
      }
      val ii = x // was: x(i)
//      stream.println("var " + quote(ii) + " = 0")
//      stream.println("while ("+quote(ii)+" < "+quote(s)+") {")
      stream.println("for ("+quote(ii)+" <- 0 until "+quote(s)+") {")
//      for (jj <- x.drop(1)) {
//        stream.println(quote(jj)+" = "+quote(ii))
//      }
//      emitFatBlock(syms(rhs))
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(g,y) =>
            stream.println(quote(l) + "("+quote(ii)+") = " + quote(getBlockResult(y)))
          case ReduceElem(g,y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
          //case ArrayIfElem(g,c,y) =>
          //  stream.println("if ("+quote(getBlockResult(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          //case ReduceIfElem(g,c,y) =>
          //  stream.println("if ("+quote(getBlockResult(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          //case FlattenElem(g,y) =>
          //  stream.println(quote(l) + " ++= " + quote(getBlockResult(y)))
        }
      }
//      stream.println(quote(ii)+" += 1")
      stream.println("}")
    case _ => super.emitFatNode(sym, rhs)
  }
}






