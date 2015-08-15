package scala.lms
package epfl
package test7

import common._
import test1._

import internal.AbstractSubstTransformer


import util.OverloadHack
import scala.reflect.SourceContext
import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait ArrayLoops extends Loops with OverloadHack { this: PrimitiveOps =>
  implicit def arrayTyp[T:Typ]: Typ[Array[T]]

  def array[T:Typ](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] // TODO: make reduce operation configurable!
  def arrayIf[T:Typ](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]]
  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] // TODO: make reduce operation configurable!
  def flatten[T:Typ](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]]

  def infix_at[T:Typ](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def infix_length[T:Typ](a: Rep[Array[T]]): Rep[Int]
}


trait ArrayLoopsExp extends LoopsExp { this: PrimitiveOpsExp =>
  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp

  case class ArrayElem[T](y: Block[T]) extends Def[Array[T]]
  case class ReduceElem(y: Block[Double]) extends Def[Double]

  case class ArrayIfElem[T](c: Exp[Boolean], y: Block[T]) extends Def[Array[T]]
  case class ReduceIfElem(c: Exp[Boolean], y: Block[Double]) extends Def[Double]

  case class FlattenElem[T](y: Block[Array[T]]) extends Def[Array[T]]

  case class ArrayIndex[T](a: Rep[Array[T]], i: Rep[Int]) extends Def[T]  
  case class ArrayLen[T](a: Rep[Array[T]]) extends Def[Int]
  
  def array[T:Typ](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = reifyEffects(f(x))
    simpleLoop(shape, x, ArrayElem(y))
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = reifyEffects(f(x))
    simpleLoop(shape, x, ReduceElem(y))
  }

  def arrayIf[T:Typ](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]] = {
    val x = fresh[Int]
    //val (c,y) = f(x)
    var c: Rep[Boolean] = null
    val y = reifyEffects { val p = f(x); c = p._1; p._2 }
    simpleLoop(shape, x, ArrayIfElem(c,y)) // TODO: simplify for const true/false
  }

  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    //val (c,y) = f(x)
    var c: Rep[Boolean] = null
    val y = reifyEffects { val p = f(x); c = p._1; p._2 }
    simpleLoop(shape, x, ReduceIfElem(c,y)) // TODO: simplify for const true/false
  }

  def flatten[T:Typ](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = reifyEffects(f(x))
    simpleLoop(shape, x, FlattenElem(y))
  }


  def infix_at[T:Typ](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = ArrayIndex(a, i)

  def infix_length[T:Typ](a: Rep[Array[T]]): Rep[Int] = a match {
    case Def(SimpleLoop(s, x, ArrayElem(y))) => s
    case _ => ArrayLen(a)
  }


  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayElem(y) => effectSyms(y)
    case ReduceElem(y) => effectSyms(y)
    case FlattenElem(y) => effectSyms(y)
    case _ => super.boundSyms(e)
  }


  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s,i, ArrayElem(y)) if f.hasContext => 
      implicit def anyTyp: Typ[Any] = ??? // FIXME: wrong type
      array(f(s)) { j => 
        f.asInstanceOf[AbstractSubstTransformer{val IR:ArrayLoopsExp.this.type}].withSubstScope(i -> j) { 
          f.reflectBlock(y)
        } 
      }
    case ArrayIndex(a,i) => infix_at(f(a), f(i))(mtype(manifest[A]))
    case ArrayLen(a) => infix_length(f(a))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayElem(y) => ArrayElem(f(y))
    case ReduceElem(y) => ReduceElem(f(y))
    case ArrayIfElem(c,y) => ArrayIfElem(f(c),f(y))
    case ReduceIfElem(c,y) => ReduceIfElem(f(c),f(y))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]


}

trait ArrayLoopsFatExp extends ArrayLoopsExp with LoopsFatExp { this: PrimitiveOpsExp => }




trait ScalaGenArrayLoops extends ScalaGenLoops {
  val IR: ArrayLoopsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SimpleLoop(s,x,ArrayElem(y)) =>  
      stream.println("val " + quote(sym) + " = LoopArray("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case SimpleLoop(s,x,ReduceElem(y)) =>  
      stream.println("val " + quote(sym) + " = LoopReduce("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    // TODO: conditional variants ...
    case SimpleLoop(s,x,FlattenElem(y)) =>  
      stream.println("val " + quote(sym) + " = LoopFlatten("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case ArrayIndex(a,i) =>  
      emitValDef(sym, quote(a) + ".apply(" + quote(i) + ")")
    case ArrayLen(a) =>  
      emitValDef(sym, quote(a) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenArrayLoopsFat extends ScalaGenArrayLoops with ScalaGenLoopsFat {
  val IR: ArrayLoopsFatExp
  import IR._
  
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(s,x,rhs) => 
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println("var " + quote(l) + " = new Array[" + remap(getBlockResult(y).tp) + "]("+quote(s)+")")
          case ReduceElem(y) =>
            stream.println("var " + quote(l) + ": " + remap(getBlockResult(y).tp) + " = 0")
          case ArrayIfElem(c,y) =>
            stream.println("var " + quote(l) + " = new ArrayBuilder[" + remap(getBlockResult(y).tp) + "]")
          case ReduceIfElem(c,y) =>
            stream.println("var " + quote(l) + ": " + remap(getBlockResult(y).tp) + " = 0")
          case FlattenElem(y) =>
            stream.println("var " + quote(l) + " = new ArrayBuilder[" + remap(getBlockResult(y).tp) + "]")
        }
      }
      val ii = x // was: x(i)
//      stream.println("var " + quote(ii) + " = 0")
//      stream.println("while ("+quote(ii)+" < "+quote(s)+") {")
      stream.println("for ("+quote(ii)+" <- 0 until "+quote(s)+") {")
//      for (jj <- x.drop(1)) {
//        stream.println(quote(jj)+" = "+quote(ii))
//      }
      emitFatBlock(syms(rhs).map(Block(_))) // TODO: check this
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println(quote(l) + "("+quote(ii)+") = " + quote(getBlockResult(y)))
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
          case ArrayIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case ReduceIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case FlattenElem(y) =>
            stream.println(quote(l) + " ++= " + quote(getBlockResult(y)))
        }
      }
//      stream.println(quote(ii)+" += 1")
      stream.println("}")
    case _ => super.emitFatNode(sym, rhs)
  }
}








trait Arrays extends Base with PrimitiveOps with OverloadHack {
  implicit def arrayTyp[T:Typ]: Typ[Array[T]]
  def zeroes(n: Rep[Int]): Rep[Array[Int]]
  def infix_update(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]): Rep[Array[Int]]
  def infix_+(a: Rep[Array[Int]], b: Rep[Array[Int]])(implicit o: Overloaded1): Rep[Array[Int]]
}

trait ArraysExp extends Arrays with PrimitiveOpsExp with EffectExp {
  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp
  case class ArrayZero(n: Rep[Int]) extends Def[Array[Int]]
  case class ArrayWrite(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]) extends Def[Array[Int]]
  case class ArrayPlus(a: Rep[Array[Int]], b: Rep[Array[Int]]) extends Def[Array[Int]]
  def zeroes(n: Rep[Int]) = ArrayZero(n)
  def infix_update(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]) = ArrayWrite(a,x,v)
  def infix_+(a: Rep[Array[Int]], b: Rep[Array[Int]])(implicit o: Overloaded1) = ArrayPlus(a,b)
}

trait ScalaGenArrays extends ScalaGenEffect {
  val IR: ArraysExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayZero(n) =>  
      emitValDef(sym, "new Array[Int](" + quote(n) + ")")
    case ArrayWrite(a,x,v) =>  
      emitValDef(sym, quote(a) +".clone()")
      stream.println(quote(sym) + "(" + quote(x) + ") = " + quote(v))
    case ArrayPlus(a,b) =>  
      emitValDef(sym, "new Array[Int](" + quote(a) + ".length)")
      stream.println("arrayPlus("+ quote(sym) + "," + quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

