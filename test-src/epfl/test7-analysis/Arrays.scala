package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import internal.AbstractSubstTransformer


import util.OverloadHack
import scala.reflect.SourceContext
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
  case class Dummy(a: Rep[Int]) extends Def[Unit]
  
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
    val d = reflectMutable(Dummy(Const(1)))
    
    val (g, y) = collectYields{ reifyEffects { 
        yields[T](d, List(x),f(x))
      }}
    simpleLoop(shape, x, ArrayElem(g, y))
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val d = reflectMutable(Dummy(Const(1)))
    
    val (g, y) = collectYields{ reifyEffects { 
       yields(d, List(x),f(x))
    }}
    
    reflectEffect(SimpleLoop(shape, x, ReduceElem(g,y)), summarizeEffects(y).star)
  }

  def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]] = {
    val x = fresh[Int]
    var builder = reflectMutable(Dummy(Const(1)))
    val (g, y) = collectYields{ reifyEffects { 
      val (c,z) = f(x)
      if (c) yields(builder, List(x), z) else skip[T](builder, List(x))
    }}
    SimpleLoop(shape, x, ArrayElem(g,y))
//    reflectEffect(SimpleLoop(shape, x, ArrayElem(g,y)), summarizeEffects(y).star)
  }

  def arrayFlat[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]] = {
    val x = fresh[Int]
    var builder = reflectMutableSym(fresh[Int])
    val (g, y) = collectYields{ reifyEffects {
      val z = f(x)
      val shape2 = infix_length(z)
      val x2 = fresh[Int]
      
      val innerBody = reifyEffects {yields(builder, List(x2, x),infix_at(z,x2))}
      reflectEffect(SimpleLoop(shape2, x2, ForeachElem(innerBody).asInstanceOf[Def[Gen[T]]]), summarizeEffects(innerBody).star)
    }}
    SimpleLoop(shape, x, ArrayElem(g,y))
//    reflectEffect(SimpleLoop(shape, x, ArrayElem(g,y)), summarizeEffects(y).star)
  }


  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    var builder = reflectMutableSym(fresh[Int])
    val (g, y) = collectYields{ reifyEffects {
      val (c,z) = f(x)
      if (c) yields(builder, List(x),z) else skip[Double](builder, List(x))
    }}
    SimpleLoop(shape, x, ReduceElem(g,y))
//    reflectEffect(SimpleLoop(shape, x, ReduceElem(g,y)), summarizeEffects(y).star)
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


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    // TODO (VJ) can we do this (just call the constructor with and ignore the old gen
    case SimpleLoop(s,i, ArrayElem(g,y)) if f.hasContext => 
      array(f(s)) { j => f.asInstanceOf[AbstractSubstTransformer{val IR:ArrayLoopsExp.this.type}].subst += (i -> j); f.reflectBlock(y) }
    case ArrayIndex(a,i) => infix_at(f(a), f(i))(mtype(manifest[A]))
    case ArrayLength(a) => infix_length(f(a))(mtype(manifest[A]))
    case Dummy(a) => Dummy(f(a))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayElem(g, y) => ArrayElem(g, f(y))
    case ReduceElem(g, y) => ReduceElem(g, f(y))
    case Dummy(a) => Dummy(f(a))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case SimpleLoop(s,i, ArrayElem(g,y)) if f.hasContext => 
      array(f(s)) { j => f.asInstanceOf[AbstractSubstTransformer{val IR:ArrayLoopsExp.this.type}].subst += (i -> j); f.reflectBlock(y) }
    case ArrayIndex(a,i) => infix_at(f(a), f(i))(mtype(manifest[A]))
    case ArrayLength(a) => infix_length(f(a))(mtype(manifest[A]))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]
}

trait ArrayLoopsFatExp extends ArrayLoopsExp with LoopsFatExp


trait ScalaGenArrayLoops extends ScalaGenLoops {
  val IR: ArrayLoopsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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
    case Dummy(_) => 
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
          case ForeachElem(y) =>
            stream.println("val " + quote(l) + " = () // foreach (this is perfectly fine)")
          case ArrayElem(g,Block(y)) if g == y =>
            stream.println("val " + quote(l) + " = new Array[" + stripGen(y.tp) + "]("+quote(s)+")")
          case ArrayElem(g,y) =>
            stream.println("val " + quote(l) + "_buff = new ArrayBuilder[" + stripGen(y.tp)  + "]")
          case ReduceElem(g,y) =>
            stream.println("var " + quote(l) + " = 0")
        }
      }
      val ii = x
      stream.println("for ("+quote(ii)+" <- 0 until "+quote(s)+") {")

      val gens = for ((l,r) <- sym zip rhs if !r.isInstanceOf[ForeachElem[_]]) yield r match {
        case ArrayElem(g,Block(y)) if g == y => // g == y should indicate selectivity 1.0 (which is not so general)
          (g, (s: List[String]) => {
            stream.println(quote(l) + "("+quote(ii)+") = " + s.head)
            stream.println("val " + quote(g) + " = ()")
          })
        case ArrayElem(g,Block(y)) =>
          (g, (s: List[String]) => {
            stream.println(quote(l) + "_buff += " + s.head)
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
          stream.println("val " + quote(l) + " = " + quote(l) + "_buff.result")
        case ReduceElem(g,y) =>
      }

    case _ => super.emitFatNode(sym, rhs)
  }
}


trait IteratorLoops extends ArrayLoops {
  def input[T: Manifest]: Rep[Array[T]]
  def sd[T: Manifest](dep: Rep[Array[T]]): Rep[Int]
}

trait IteratorLoopsExp extends ArrayLoopsExp {
  case class InputNode[T] extends Def[Array[T]]
  case class SD[T: Manifest](dep: Rep[Array[T]]) extends Def[Int]
  
  def input[T: Manifest]: Rep[Array[T]] = InputNode[T]()
  def sd[T: Manifest](dep: Rep[Array[T]]): Rep[Int] = SD(dep)
  
}

trait IteratorLoopsFatExp extends ArrayLoopsFatExp with IteratorLoopsExp with LoopsFatExp

trait ScalaGenIteratorLoopsFat extends ScalaGenArrayLoopsFat with ScalaGenLoopsFat {
  val IR: IteratorLoopsFatExp with IfThenElseFatExp
  import IR._
  
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(Def(SD(dep)),x,rhs) =>
      val ii = x
      // here we need to extract the value from the loop
      stream.println("val " + quote(sym.head) + " = " + quote(dep) +".mapPartitions(it => {")
      
      for ((l,r) <- sym zip rhs) {
        r match {
          case ForeachElem(y) =>
            stream.println("val " + quote(l) + " = () // foreach (this is perfectly fine)")
            stream.println("while(???) { // what to do with the iterator")
          case ArrayElem(g,y) =>
            // if not foreach elem
            stream.println("val inArray = it.toArray")
            stream.println("var " + quote(ii) + " = 0")
            stream.println("val " + quote(l) + "_buff = new Array[" + stripGen(y.tp)  + "](inArray.size)")
            stream.println("while ("+quote(ii)+" < inArray.size) {")
        }
      }
      
      val gens = for ((l,r) <- sym zip rhs if !r.isInstanceOf[ForeachElem[_]]) yield r match {
        case ArrayElem(g,Block(y)) =>
          (g, (s: List[String]) => {
            stream.println(quote(l) + "_buff("+quote(ii)+") = " + s.head)
            stream.println("val " + quote(g) + " = ()")
          })
      }

      withGens(gens) {
        emitFatBlock(syms(rhs).map(Block(_)))
      }
      stream.println(quote(ii) + " = " + quote(ii) + " + 1")
      stream.println("}")
      stream.println(quote(sym.head) + "_buff.iterator")
      stream.println("})")

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
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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