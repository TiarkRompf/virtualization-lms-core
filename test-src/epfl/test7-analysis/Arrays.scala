package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait ArrayLoops extends Loops with OverloadHack {
  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] // TODO: make reduce operation configurable!
  def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]]
  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] // TODO: make reduce operation configurable!
  def arrayFlat[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]]

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
}


trait ArrayLoopsExp extends LoopsExp with IfThenElseExp {

  case class ForeachElem[T](y: Block[Gen[T]]) extends Def[Gen[T]]

  case class ArrayElem[T](g: Exp[Gen[T]], y: Block[Gen[T]]) extends Def[Array[T]]
  case class ReduceElem(g: Exp[Gen[Double]], y: Block[Gen[Double]]) extends Def[Double]

  case class FlattenElem[T](g: Exp[Gen[Array[T]]], y: Block[Gen[Array[T]]]) extends Def[Array[T]]

  case class ArrayIndex[T](a: Rep[Array[T]], i: Rep[Int]) extends Def[T]  
  case class ArrayLength[T](a: Rep[Array[T]]) extends Def[Int]

/*
  example for flatMap fusion

  val as = ... // input

  val xs = for (i <- 0 until as.length) yield (i, f(as(i)))  // as map f
  
  val ys = for (i <- 0 until xs.length) {  // xs flatMap g
    val es = xs(i)
    for (j <- 0 until es.length) {
      yield(i, es(j))
    }
  }
  
  val zs = for (i <- 0 until ys.length) yield (i, h(ys(i)))  // ys map h
  
  // fused (assuming xs, ys not otherwise used)
  
  for (i <- 0 until as.length) 
  
    val xs_i = f(as(i))  // as map f
  
    val es = xs_i
    for (j <- 0 until es.length) {
      val ys_i = es(j))
      val zs_i = h(ys_i))
      
      yield(i, zs_i)
    }

  }

  
  // --- unrelated to above: loops over hashtables should be able to write to 'current' key
  
  for ((k,v) <- htable) {
    htable(k) = f(v)
  }
*/  
  
  // TODO: use simpleLoop instead of SimpleLoop

  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]

    val (g, y) = collectYields{ reifyEffects { 
        yields(List(x),f(x))
      }}
    simpleLoop(shape, x, ArrayElem(g, y))
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    //val g = fresh[Accu[Double]]
    val x = fresh[Int]
    val (g, y) = collectYields{ reifyEffects { 
       yields(List(x),f(x))
    }}
    
    simpleLoop(shape, x, ReduceElem(g,y))
  } 

  def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]] = {
    val x = fresh[Int]
    val (g, y) = collectYields{ reifyEffects { 
      val (c,z) = f(x)
      if (c) yields(List(x),z) else skip[T](List(x))
    }}
    reflectEffect(SimpleLoop(shape, x, ArrayElem(g,y)), summarizeEffects(y).star)
  }

  def arrayFlat[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]] = {
    val x = fresh[Int]
    val (g, y) = collectYields{ reifyEffects {
      val z = f(x)
      val shape2 = infix_length(z)
      val x2 = fresh[Int]

      simpleLoop(shape2, x2, ForeachElem(reifyEffects {yields(List(x2, x),infix_at(z,x2))}).asInstanceOf[Def[Gen[T]]])
    }}
    reflectEffect(SimpleLoop(shape, x, ArrayElem(g,y)), summarizeEffects(y).star)
  }


  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    val (g, y) = collectYields{ reifyEffects {
      val (c,z) = f(x)
      if (c) yields(List(x),z) else skip[Double](List(x))
    }}
    reflectEffect(SimpleLoop(shape, x, ReduceElem(g,y)), summarizeEffects(y).star)
  }

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = ArrayIndex(a, i)

  def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int] = a match {
//    case Def(SimpleLoop(s, x, ArrayElem(g1,Block(Def(Yield(x2,y)))))) if x == x2 => s // TODO: check condition
    case Def(SimpleLoop(s, x, ArrayElem(g,Block(y)))) if g == y => s // TODO: check condition
    case _ => ArrayLength(a)
  }


  override def syms(e: Any): List[Sym[Any]] = e match {
    case ForeachElem(y) => syms(y)
    case ArrayElem(g,y) => syms(y)
    case ReduceElem(g,y) => syms(y)
    case FlattenElem(g,y) => syms(y)
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any) = e match {
    case ForeachElem(y) => freqNormal(y)
    case ArrayElem(g,y) => freqNormal(y)
    case ReduceElem(g,y) => freqNormal(y)
    case FlattenElem(g,y) => freqNormal(y)
    case _ => super.symsFreq(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ForeachElem(y) => effectSyms(y)
    case ArrayElem(g,y) => effectSyms(y)
    case ReduceElem(g,y) => effectSyms(y)
    case FlattenElem(g,y) => effectSyms(y)
    case _ => super.boundSyms(e)
  }

}

trait ArrayLoopsFatExp extends ArrayLoopsExp with LoopsFatExp




trait ScalaGenArrayLoops extends ScalaGenLoops {
  val IR: ArrayLoopsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case SimpleLoop(s,x,ArrayElem(g,y)) =>  
      stream.println("val " + quote(sym) + " = LoopArray("+quote(s)+") { " + quote(x) + " => ")
      withGen(g, s=>stream.println(s)) {
        emitBlock(y)
      }
      stream.println("}")
    case SimpleLoop(s,x,ReduceElem(g,y)) =>  
      stream.println("val " + quote(sym) + " = LoopReduce("+quote(s)+") { " + quote(x) + " => ")
      withGen(g, s=>stream.println(s)) {
        emitBlock(y)
      }
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
          case ForeachElem(y) =>
            stream.println("val " + quote(l) + " = () // foreach")
          case ArrayElem(g,y) if g == getBlockResult(y) =>
            stream.println("val " + quote(l) + " = new Array[" + stripGen(getBlockResult(y).Type) + "]("+quote(s)+")")
          case ArrayElem(g,y) =>
            stream.println("val " + quote(l) + " = new ArrayBuilder[" + stripGen(getBlockResult(y).Type)  + "]")
          case ReduceElem(g,y) =>
            stream.println("var " + quote(l) + " = 0")
        }
      }
      val ii = x
      stream.println("for ("+quote(ii)+" <- 0 until "+quote(s)+") {")

      val gens = for ((l,r) <- sym zip rhs if !r.isInstanceOf[ForeachElem[_]]) yield r match {
        //case ForeachElem(y) =>
        case ArrayElem(g,Block(y)) if g == y => // g == y indicates selectivity 1.0
          (g, (s: List[String]) => {
            stream.println(quote(l) + "("+quote(ii)+") = " + s.head)
            stream.println("val " + quote(g) + " = ()")
          })
        case ArrayElem(g,Block(y)) =>
          (g, (s: List[String]) => {
            stream.println(quote(l) + " += " + s.head)
            stream.println("val " + quote(g) + " = ()")
          })
        case ReduceElem(g,y) =>
          (g, (s: List[String]) => { 
            stream.println(quote(l) + " += " + s.head)
            stream.println("val " + quote(g) + " = ()")
          })
      }

      withGens(gens) {
        emitFatBlock(syms(rhs).map(Block(_)))
      }

      stream.println("}")

      for ((l,r) <- sym zip rhs) r match {
        case ForeachElem(y) => 
        case ArrayElem(g,Block(y)) if g == y =>
        case ArrayElem(g,y) =>
          stream.println("val " + quote(l) + " = " + quote(g) + ".result")
        case ReduceElem(g,y) =>
      }

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

