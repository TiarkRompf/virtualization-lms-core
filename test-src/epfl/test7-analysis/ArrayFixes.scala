package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.{OverloadHack, GraphUtil}
import scala.reflect.SourceContext

trait ArrayLoopsFixes extends ArrayLoops {
  def infix_foreach[T:Manifest](a: Rep[Array[T]], f: Rep[T] => Rep[Unit]): Rep[Unit]
}

trait ArrayLoopsExpFixes extends EffectExp with ArrayLoopsExp {

  // adding effects treatment to ArrayLoops.... Hum hum...
  // def infix_foreach[T:Manifest](a: Rep[Array[T]], f: Rep[T] => Rep[Unit]): Rep[Unit] = {
  //   val x = fresh[Int]
  //   val ax = infix_at(a, x)
  //   val y = reifyEffectsHere(f(ax))
  //   val yEff = summarizeEffects(y)
  //   reflectEffect(SimpleLoop(infix_length(a), x, ForeachElem(y)), yEff.star)
  // }

  override def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(f(x))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, ArrayElem(y)), yEff.star) 
  }

  override def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(f(x))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, ReduceElem(y)), yEff.star) 
  }

  override def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]] = {
    val x = fresh[Int]
    //val (c,y) = f(x)
    var c: Rep[Boolean] = null
    val y = reifyEffectsHere { val p = f(x); c = p._1; p._2 } // TODO check effects in cond
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, ArrayIfElem(c,y)), yEff.star) 
  }

  override def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    //val (c,y) = f(x)
    var c: Rep[Boolean] = null
    val y = reifyEffectsHere { val p = f(x); c = p._1; p._2 } // TODO check effects in cond
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, ReduceIfElem(c,y)), yEff.star) 
  }

  override def flatten[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(f(x))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, FlattenElem(y)), yEff.star) 
  }

  // mirrorring effects too

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(SimpleLoop(s, i, body), u, es) =>
      // First process body, otherwise shape or index could take onceSubst
      val newBody = mirrorFatDef(body, f)

      reflectMirrored(
        Reflect(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], newBody), 
        mapOver(f,u), f(es)))(mtype(manifest[A]))
      
    case SimpleLoop(s, i, body) => 
      // First process body, otherwise shape or index could take onceSubst
      val newBody = mirrorFatDef(body, f)

      simpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], newBody)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayElem(y) => ArrayElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case ReduceElem(y) => ReduceElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case ArrayIfElem(c,y) =>
      // Cheating, need to process body first, otherwise condition takes
      // onceSubst of body if it's a common subexpression
      val body = reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A]))
      ArrayIfElem(f.reflectBlock(Block(c)),body)
    case FlattenElem(y) => FlattenElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case ForeachElem(y) => ForeachElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]


  // new nodes

  case class EmptyArray[T]() extends Def[Array[T]]
  def emptyArray[T:Manifest](): Rep[Array[T]] = EmptyArray[T]()

  case class ForeachElem(y: Block[Any]) extends Def[Unit]
  def infix_foreach[T:Manifest](a: Rep[Array[T]], f: Rep[T] => Rep[Unit]): Rep[Unit] = {
    val x = fresh[Int]
    val ax = infix_at(a, x)
    val y = reifyEffectsHere(f(ax))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(infix_length(a), x, ForeachElem(y)), yEff.star)
  }
}

trait ScalaGenArrayLoopsFatFixes extends ScalaGenArrayLoopsFat {
  val IR: ArrayLoopsFatExp with ArrayLoopsExpFixes
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@EmptyArray() => 
      stream.println("var " + quote(sym) + " = new " + sym.tp + "(0)")
      // TODO why emitFatNode not sufficient?
    case Reflect(SimpleLoop(shape,index,body), _, _) => 
      emitFatNode(List(sym), SimpleFatLoop(shape, index, List(body)))
    // case SimpleLoop(shape,index,ForeachElem(block)) =>
    //   stream.println("var " + quote(sym) + " = for (" + quote(index) + " <- 0 until " + quote(shape) + ") {")
    //   emitBlock(block)
    //   stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }

  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(s,x,rhs2) => 
      val symRhs: List[(Sym[Any], Def[Any])] = sym zip rhs2
      symRhs.foreach({ t: (Sym[Any], Def[Any]) =>
        val l = t._1
        t._2 match {
          case ArrayElem(y) =>
            stream.println("var " + quote(l) + " = new Array[" + getBlockResult(y).tp + "]("+quote(s)+")")
          case ReduceElem(y) =>
            stream.println("var " + quote(l) + " = 0")
          case ArrayIfElem(c,y) =>
            stream.println("var " + quote(l) + " = new ArrayBuilder[" + getBlockResult(y).tp + "]")
          case ReduceIfElem(c,y) =>
            stream.println("var " + quote(l) + " = 0")
          case FlattenElem(y) =>
            stream.println("var " + quote(l) + " = new ArrayBuilder[" + getBlockResult(y).tp.typeArguments(0) + "]")
          case ForeachElem(_) =>
        }
      })
      stream.println("for ("+quote(x)+" <- 0 until "+quote(s)+") {")

      emitFatBlock(syms(rhs2).map(Block(_)))

      symRhs.foreach({ t: (Sym[Any], Def[Any]) =>
        val l = t._1
        t._2 match {
          case ArrayElem(y) =>
            stream.println(quote(l) + "("+quote(x)+") = " + quote(getBlockResult(y)))
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
          case ArrayIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case ReduceIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case FlattenElem(y) =>
            stream.println(quote(l) + " ++= " + quote(getBlockResult(y)))
          case ForeachElem(_) =>          
        }
      })
      stream.println("}")
    case _ => super.emitFatNode(sym, rhs)
  }


}

trait ArrayLoopFusionExtractors extends ArrayLoopsExpFixes with LoopFusionExtractors {

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }

  override def unapplySimpleDomain(e: Def[Any]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }


  override def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = e match {
    case SimpleLoop(s, _, ArrayElem(_)) => Some(s)
    // TODO is this input or output size? in math, domain is input, range is output
//    case SimpleLoop(s, _, ForeachElem(_)) => Some(s)
    case _ => super.unapplyFixedDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Block(a)) => Some(a) //TODO: block??
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,Block(a)) => Some((a,List(c))) //TODO: block?
    case _ => super.unapplySimpleCollectIf(e)
  }

  override def unapplyReduce[T](e: Def[T]) = e match {
    case ReduceElem(Block(a: Exp[Double])) => // ReduceElem is sum of doubles...
      Some((a.asInstanceOf[Exp[T]], Some(unit(0.0).asInstanceOf[Exp[T]]), Some(true)))
    // what about ReduceIfElem?
    case ForeachElem(Block(a: Exp[Unit @unchecked])) =>
      Some((a.asInstanceOf[Exp[T]], Some(unit(()).asInstanceOf[Exp[T]]), Some(false)))
    case _ => super.unapplyReduce(e)
  }

  override def unapplyMultiCollect[T](e: Def[T]) = e match {
    case FlattenElem(Block(a: Exp[T])) => Some((a.asInstanceOf[Exp[T]], Some((() => {
        emptyArray()(mtype(a.tp.typeArguments(0)))
      }).asInstanceOf[() => Exp[T]])))
    case _ => super.unapplyMultiCollect(e)
  }

  // override def unapplyForeach(e: Def[Any]) = e match {
  //   case ForeachElem(Block(a: Exp[Unit])) => Some(a)
  //   case _ => super.unapplyForeach(e)
  // }

}