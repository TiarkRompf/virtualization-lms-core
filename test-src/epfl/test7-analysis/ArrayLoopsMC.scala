package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import internal.AbstractSubstTransformer


import util.OverloadHack
import scala.reflect.SourceContext
import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait ArrayLoopsMC extends Loops with OverloadHack {
  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] // TODO: make reduce operation configurable!
  def arrayIf[T:Manifest](shape: Rep[Int])(cond: Rep[Int] => Rep[Boolean], f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] // TODO: make reduce operation configurable!
  def flatten[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]]

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
  def infix_foreach[T:Manifest](a: Rep[Array[T]], f: Rep[T] => Rep[Unit]): Rep[Unit]
}


trait ArrayLoopsMCExp extends LoopsExp with EffectExp with IfThenElseExp {

  // Keep generated arrays from being lifted out of loop when used in MultiCollect(IfThenElse(cond, thenp, empty))
  // TODO whether same problem with singleton that is indep of index
  object SuperEmptyArray {
    def unapply(e: Any) = e match {
      case EmptyArrayInLoop(_) => true
      case EmptyArray() => true
      case _ => false
    }
  }
  case class EmptyArrayInLoop[T](index: Exp[Int]) extends Def[Array[T]]
  case class EmptyArray[T]() extends Def[Array[T]]

  // block so that elem can be effectful computation
  case class Singleton[T](elem: Block[T]) extends Def[Array[T]]

  case class MultiArrayElem[T](y: Block[Array[T]]) extends Def[Array[T]]

  //------ old
  case class ReduceElem(y: Block[Double]) extends Def[Double]
  case class ReduceIfElem(c: Exp[Boolean], y: Block[Double]) extends Def[Double]

  case class ForeachElem(y: Block[Any]) extends Def[Unit]

  case class ArrayIndex[T](a: Rep[Array[T]], i: Rep[Int]) extends Def[T]  
  case class ArrayLength[T](a: Rep[Array[T]]) extends Def[Int]

  def emptyArray[T:Manifest](): Rep[Array[T]] = EmptyArray[T]()
  
  def singleton[T:Manifest](elem: => Rep[T]): Rep[Array[T]] = {
    val reifiedElem = reifyEffectsHere(elem)
    val elemEff = summarizeEffects(reifiedElem)
    reflectEffect(Singleton(reifiedElem), elemEff)
  }
  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]
    val m = reifyEffectsHere(singleton(f(x)))
    val mEff = summarizeEffects(m)
    reflectEffect(SimpleLoop(shape, x, MultiArrayElem(m)), mEff.star) 
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(f(x))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, ReduceElem(y)), yEff.star) 
  }

  // TODO test effects are routed correctly
  def arrayIf[T:Manifest](shape: Rep[Int])(cond: Rep[Int] => Rep[Boolean], f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]

    val ifthenelse = reifyEffectsHere(__ifThenElse(cond(x), singleton(f(x)), EmptyArrayInLoop[T](x)))
    val eff = summarizeEffects(ifthenelse)
    reflectEffect(SimpleLoop(shape, x, MultiArrayElem(ifthenelse)), eff.star) 
    // var condExp: Exp[Boolean] = null
    // val c = reifyEffectsHere({ condExp = cond(x); condExp })    
    // val ceff = summarizeEffects(c)
    // val ifthenelse = reifyEffectsHere(__ifThenElse(condExp, singleton(f(x)), EmptyArrayInLoop[T](x)))
    // val eff = summarizeEffects(ifthenelse)
    // reflectEffect(SimpleLoop(shape, x, MultiArrayElem(ifthenelse)), (ceff andThen eff).star) 
  }

  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    //val (c,y) = f(x)
    var c: Rep[Boolean] = null
    val y = reifyEffectsHere { val p = f(x); c = p._1; p._2 } // TODO check effects in cond
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, ReduceIfElem(c,y)), yEff.star) 
  }

  def flatten[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(f(x))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, MultiArrayElem(y)), yEff.star) 
  }

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = ArrayIndex(a, i)

  def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int] = a match {
//    case Def(SimpleLoop(s, x, ArrayElem(y))) => s
    case _ => ArrayLength(a)
  }
  
  def infix_foreach[T:Manifest](a: Rep[Array[T]], f: Rep[T] => Rep[Unit]): Rep[Unit] = {
    val x = fresh[Int]
    val ax = infix_at(a, x)
    val y = reifyEffectsHere(f(ax))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(infix_length(a), x, ForeachElem(y)), yEff.star)
  }

  // boundSyms won't be moved out of block
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MultiArrayElem(y) => effectSyms(y)
    case ReduceElem(y) => effectSyms(y)
    case Singleton(y) => effectSyms(y)
    case _ => super.boundSyms(e)
  }

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
    case ArrayIndex(a,i) => infix_at(f(a), f(i))(mtype(manifest[A]))
    case ArrayLength(a) => infix_length(f(a))(mtype(manifest[A]))
    case Singleton(block) => toAtom(Singleton(f(block)))(mtype(manifest[A]), mpos(e.pos))
//    case Singleton(block) => Singleton(reifyEffectsHere(f.reflectBlock(block))))
    case Reflect(Singleton(block), u, es) =>
      reflectMirrored(Reflect(Singleton(reifyEffectsHere(f.reflectBlock(block))(mtype(manifest[A].typeArguments(0)))), mapOver(f,u), f(es)))(mtype(manifest[A]))
    // case Reflect(Singleton(e), u, es) => 
    //   reflectMirrored(Reflect(Singleton(f(e)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case EmptyArray() => toAtom(e)(mtype(manifest[A]), mpos(e.pos))
    case EmptyArrayInLoop(x) => toAtom(EmptyArrayInLoop(f(x)))(mtype(manifest[A]), mpos(e.pos))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ReduceElem(y) => ReduceElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case ReduceIfElem(c,y) => ReduceIfElem(f(c),f(y))
    case ForeachElem(y) => ForeachElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case MultiArrayElem(y) => MultiArrayElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]

}

trait ArrayLoopsMCFatExp extends ArrayLoopsMCExp with LoopsFatExp


trait ScalaGenArrayLoopsMCFat extends ScalaGenLoopsFat {
  val IR: ArrayLoopsMCFatExp
  import IR._

  val replaceIfThenElseSingletonEmptyWithAppend = scala.collection.mutable.HashMap[Sym[Any], Sym[Any]]()

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EatReflect(IfThenElse(cond, Block(si@Def(Singleton(thenp))), Block(em@Def(SuperEmptyArray())))) =>
      replaceIfThenElseSingletonEmptyWithAppend.get(sym) match {
        case Some(arraySym) => 
          stream.println("if (" + quote(cond) + ") {")
          emitBlock(thenp)
          stream.println(quote(arraySym) + " += " + quote(getBlockResult(thenp)))
          stream.println("}")
        case None => super.emitNode(sym, rhs)
      } 
    case SuperEmptyArray() => 
      stream.println("val " + quote(sym) + ": " + sym.tp + " = Array.empty")
    case s@Singleton(elem) =>
      emitBlock(elem)
      stream.println("val " + quote(sym) + " = Array(" + quote(getBlockResult(elem)) + ")")
    case ArrayIndex(a,i) =>  
      emitValDef(sym, quote(a) + ".apply(" + quote(i) + ")")
    case ArrayLength(a) =>  
      emitValDef(sym, quote(a) + ".length")
    case Reflect(SimpleLoop(shape,index,body), _, _) => 
      emitFatNode(List(sym), SimpleFatLoop(shape, index, List(body)))
    case _ => super.emitNode(sym, rhs)
  }

  object EatReflect {
    def unapply(d: Any): Option[Any] = d match {
      case Reflect(inner, _, _) => Some(inner)
      case _ => Some(d)
    }
  }
  
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(s,x,rhs2) => 
      val symRhs: List[(Sym[Any], Def[Any])] = sym zip rhs2
      val blockSyms = symRhs.flatMap({ t: (Sym[Any], Def[Any]) =>
        val l = t._1
        t._2 match {
          case MultiArrayElem(y) => getBlockResult(y) match {
            case Def(EatReflect(Singleton(e))) => 
              // TODO what about effects before e etc?
              stream.println("val " + quote(l) + " = new Array[" + getBlockResult(e).tp + "]("+quote(s)+")")
              getBlockResult(e) :: effectSyms(e) // ::: getEffects(y) adds singleton node, but could need other effects?
            case ite@Def(EatReflect(IfThenElse(cond, Block(Def(Singleton(thenp))), Block(Def(SuperEmptyArray()))))) =>
              stream.println("var " + quote(l) + " = new ArrayBuilder[" + getBlockResult(y).tp.typeArguments(0) + "]")
              replaceIfThenElseSingletonEmptyWithAppend.put(ite.asInstanceOf[Sym[Any]], l)
              syms(t._2)
            case _ =>
              stream.println("var " + quote(l) + " = new ArrayBuilder[" + getBlockResult(y).tp.typeArguments(0) + "]")
              syms(t._2) 
          }
          case ReduceElem(y) =>
            stream.println("var " + quote(l) + " = 0")
            syms(t._2) 
          case ReduceIfElem(c,y) =>
            stream.println("var " + quote(l) + " = 0")
            syms(t._2) 
          case ForeachElem(_) => Nil
        }
      })
        
      stream.println("for ("+quote(x)+" <- 0 until "+quote(s)+") {")

      emitFatBlock(blockSyms.map(Block(_)))

      symRhs.foreach({ t: (Sym[Any], Def[Any]) =>
        val l = t._1
        t._2 match {
          case MultiArrayElem(y) => getBlockResult(y) match {
            case Def(EatReflect(Singleton(e))) => 
              stream.println(quote(l) + "("+quote(x)+") = " + quote(getBlockResult(e)))
            case ite@Def(EatReflect(IfThenElse(cond, Block(Def(Singleton(thenp))), Block(Def(SuperEmptyArray()))))) =>
              replaceIfThenElseSingletonEmptyWithAppend.remove(ite.asInstanceOf[Sym[Any]])
            case _ => 
              stream.println(quote(l) + " ++= " + quote(getBlockResult(y)))
          }

          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
          case ReduceIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case ForeachElem(_) =>          
        }
      })
      stream.println("}")
    case _ => super.emitFatNode(sym, rhs)
  }
}

trait ArrayLoopsMCFusionExtractors extends ArrayLoopsMCFatExp with LoopFusionExtractors {

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }

  override def unapplySimpleDomain(e: Def[Any]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }

  override def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = e match {
    case SimpleLoop(s, _, MultiArrayElem(Block(Def(SingletonColl(_))))) => Some(s)
    // TODO is this input or output size? in math, domain is input, range is output
//    case SimpleLoop(s, _, ForeachElem(_)) => Some(s)
    case _ => super.unapplyFixedDomain(e)
  }

  override def unapplyEmptyColl(a: Def[Any]): Boolean = a match {
    case SuperEmptyArray() => true
    case _ => super.unapplyEmptyColl(a)
  }
  override def unapplySingletonColl(a: Def[Any]): Option[Exp[Any]] = a match {
    case Singleton(Block(e)) => Some(e)
    case _ => super.unapplySingletonColl(a)
  }

  override def unapplyMultiCollect[T](e: Def[T]) = e match {
    // Some((a.asInstanceOf[Exp[T]], Some((() => {
    //     emptyArray()(mtype(a.tp.typeArguments(0)))
    //   }).asInstanceOf[() => Exp[T]])))
    case MultiArrayElem(Block(a: Exp[T])) => Some(a.asInstanceOf[Exp[T]])
    case _ => super.unapplyMultiCollect(e)
  }

  override def unapplyForlike[T](e: Def[T]) = e match {
    case ReduceElem(Block(a: Exp[Double])) => // ReduceElem is sum of doubles...
      Some(a.asInstanceOf[Exp[T]])
    // what about ReduceIfElem?
    case ForeachElem(Block(a: Exp[Unit @unchecked])) =>
      // Foreach is associative...
      Some(a.asInstanceOf[Exp[T]])
    case _ => super.unapplyForlike(e)
  }


  // override def unapplyForeach(e: Def[Any]) = e match {
  //   case ForeachElem(Block(a: Exp[Unit])) => Some(a)
  //   case _ => super.unapplyForeach(e)
  // }

}


