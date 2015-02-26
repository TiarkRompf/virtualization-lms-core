package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import internal.AbstractSubstTransformer


import util.OverloadHack
import scala.reflect.SourceContext
import scala.collection.mutable.HashMap
import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait ArrayLoopsMC extends Loops with OverloadHack {
  def emptyArray[T:Manifest](): Rep[Array[T]]
  def singleton[T:Manifest](elem: Rep[T]): Rep[Array[T]]
  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Int]): Rep[Int]
  def sumD(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double]
  def reduce[T:Manifest](shape: Rep[Int])(valFunc: Rep[Int] => Rep[Array[T]], 
    redFunc: (Rep[T], Rep[T]) => Rep[T], zero: Rep[T]): Rep[T]
  def arrayIf[T:Manifest](shape: Rep[Int])(cond: Rep[Int] => Rep[Boolean], f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def flatten[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]]
  def foreach2[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit]

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
  def infix_foreach[T:Manifest](a: Rep[Array[T]], f: Rep[T] => Rep[Unit]): Rep[Unit]
}


trait ArrayLoopsMCExp extends LoopsExp with EffectExp with IfThenElseExp with NumericOpsExp with PrimitiveOpsExp {

  // Keep generated arrays from being lifted out of loop when used in MultiCollect(IfThenElse(cond, thenp, empty))
  object SuperEmptyArray {
    def unapply(e: Any) = e match {
      case EmptyArrayInLoop(_, _) => true
      case EmptyArray(_) => true
      case _ => false
    }
  }
  case class EmptyArrayInLoop[T](index: Exp[Int], dummy: Sym[T]) extends Def[Array[T]] with CanBeFused
  // Without dummy empty arrays of different types are CSE'd and only
  // the first copy is retained, resulting in type errors
  case class EmptyArray[T](dummy: Sym[T]) extends Def[Array[T]] with CanBeFused

  // block so that elem can be effectful computation
  object SuperSingleton {
    def unapply(e: Any) = e match {
      case Singleton(elem) => Some(elem)
      case SingletonInLoop(elem, _) => Some(elem)
      case _ => None
    }
  }
  case class Singleton[T](elem: Exp[T]) extends Def[Array[T]] with CanBeFused
  case class SingletonInLoop[T](elem: Exp[T], index: Exp[Int]) extends Def[Array[T]] with CanBeFused

  case class MultiArrayElem[T](y: Block[Array[T]]) extends Def[Array[T]]

  case class MultiReduceElem[T](valFunc: Block[Array[T]], redFunc: Block[T],
    zero: Rep[T], accSym: Sym[T], valSym: Sym[T]) extends Def[T]

  case class ForeachElem(y: Block[Any]) extends Def[Unit]

  case class ArrayIndex[T](a: Rep[Array[T]], i: Rep[Int]) extends Def[T]  
  case class ArrayLength[T](a: Rep[Array[T]]) extends Def[Int]

  // use one symbol per type, like this arrays of same type are CSE'd
  val typeMap = new HashMap[String, Sym[Any]]()
  def getDummy[T:Manifest](): Sym[T] = { typeMap.getOrElseUpdate(manifest[T].toString, fresh[T]).asInstanceOf[Sym[T]] }

  def emptyArray[T:Manifest](): Rep[Array[T]] = EmptyArray(getDummy[T]())
  def emptyArrayInLoop[T:Manifest](index: Exp[Int]): Rep[Array[T]] = EmptyArrayInLoop(index, getDummy[T]())
  
  def singleton[T:Manifest](elem: Rep[T]): Rep[Array[T]] = Singleton(elem)
  def singletonInLoop[T:Manifest](elem: Rep[T], index: Exp[Int]): Rep[Array[T]] = elem match {
    case `index` => Singleton(elem)
    case _ => SingletonInLoop(elem, index)
  }

  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]
    val m = reifyEffectsHere(singletonInLoop(f(x), x))
    val mEff = summarizeEffects(m)
    reflectEffect(SimpleLoop(shape, x, MultiArrayElem(m)), mEff.star) 
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Int]): Rep[Int] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(singletonInLoop(f(x), x))
    val yEff = summarizeEffects(y)
    val accSym = fresh[Int]
    val valSym = fresh[Int]
    val reduceFun = reifyEffectsHere(numeric_plus(valSym, accSym))
    val rEff = summarizeEffects(reduceFun)
    reflectEffect(SimpleLoop(shape, x, MultiReduceElem[Int](y, reduceFun, unit(0), accSym, valSym)), (yEff.star andThen (rEff.star)).star)
  }
  def sumD(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(singletonInLoop(f(x), x))
    val yEff = summarizeEffects(y)
    val accSym = fresh[Double]
    val valSym = fresh[Double]
    val reduceFun = reifyEffectsHere(infix_+(valSym, accSym))
    val rEff = summarizeEffects(reduceFun)
    reflectEffect(SimpleLoop(shape, x, MultiReduceElem[Double](y, reduceFun, unit(0.0), accSym, valSym)), (yEff.star andThen (rEff.star)).star)
  }

  def reduce[T:Manifest](shape: Rep[Int])(valFunc: Rep[Int] => Rep[Array[T]], 
    redFunc: (Rep[T], Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(valFunc(x))
    val yEff = summarizeEffects(y)
    val accSym = fresh[T]
    val valSym = fresh[T]
    val reduceFun = reifyEffectsHere(redFunc(valSym, accSym))
    val rEff = summarizeEffects(reduceFun)
    reflectEffect(SimpleLoop(shape, x, MultiReduceElem[T](y, reduceFun, zero, accSym, valSym)), (yEff.star andThen (rEff.star)).star)
  }

  // TODO test effects are routed correctly
  def arrayIf[T:Manifest](shape: Rep[Int])(cond: Rep[Int] => Rep[Boolean], f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]

    val ifthenelse = reifyEffectsHere(__ifThenElse(cond(x), singletonInLoop(f(x), x), EmptyArrayInLoop[T](x, fresh[T])))
    val eff = summarizeEffects(ifthenelse)
    reflectEffect(SimpleLoop(shape, x, MultiArrayElem(ifthenelse)), eff.star) 
    // var condExp: Exp[Boolean] = null
    // val c = reifyEffectsHere({ condExp = cond(x); condExp })    
    // val ceff = summarizeEffects(c)
    // val ifthenelse = reifyEffectsHere(__ifThenElse(condExp, singleton(f(x)), EmptyArrayInLoop[T](x)))
    // val eff = summarizeEffects(ifthenelse)
    // reflectEffect(SimpleLoop(shape, x, MultiArrayElem(ifthenelse)), (ceff andThen eff).star) 
  }

  // def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
  //   val x = fresh[Int]
  //   //val (c,y) = f(x)
  //   var c: Rep[Boolean] = null
  //   val y = reifyEffectsHere { val p = f(x); c = p._1; p._2 } // TODO check effects in cond
  //   val yEff = summarizeEffects(y)
  //   reflectEffect(SimpleLoop(shape, x, ReduceIfElem(c,y)), yEff.star) 
  // }

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

  def foreach2[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit] = {
    val x = fresh[Int]
    val y = reifyEffectsHere(f(x))
    val yEff = summarizeEffects(y)
    reflectEffect(SimpleLoop(shape, x, ForeachElem(y)), yEff.star)
  }

  // boundSyms won't be moved out of block
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MultiArrayElem(y) => effectSyms(y)
    case MultiReduceElem(y, z, _, acc, vsym) => acc :: vsym :: effectSyms(y) ::: effectSyms(z)
//    case SuperSingleton(y) => effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(SimpleLoop(s, i, body), u, es) =>
      // First process body, otherwise shape or index could take onceSubst
      val newBody = mirrorFatDef(body, f)
      reflectMirrored(
        Reflect(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], newBody), 
        mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      
    case SimpleLoop(s, i, body) => 
      // First process body, otherwise shape or index could take onceSubst
      val newBody = mirrorFatDef(body, f)
      simpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], newBody)
    case ArrayIndex(a,i) => infix_at(f(a), f(i))(mtype(manifest[A]))
    case ArrayLength(a) => infix_length(f(a))(mtype(manifest[A]))
    case Singleton(elem) => singleton(f(elem))(mtype(manifest[A]))
    case SingletonInLoop(elem, index) => singletonInLoop(f(elem), f(index))(mtype(manifest[A]))
    case EmptyArray(dummy) => toAtom(e)(mtype(manifest[A]), mpos(e.pos))
    case EmptyArrayInLoop(x, dummy) => toAtom(EmptyArrayInLoop(f(x), dummy))(mtype(manifest[A]), mpos(e.pos))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ForeachElem(y) => ForeachElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    case MultiArrayElem(y) => MultiArrayElem(reifyEffectsHere(f.reflectBlock(y))(mtype(manifest[A])))
    
    case MultiReduceElem(valFunc, redFunc, zero, accSym, valSym) =>
      MultiReduceElem((reifyEffectsHere(f.reflectBlock(valFunc))(mtype(valFunc.tp))).asInstanceOf[Block[Array[A]]], 
        reifyEffectsHere(f.reflectBlock(redFunc))(mtype(manifest[A])), zero, 
        f(accSym).asInstanceOf[Sym[A]], f(valSym).asInstanceOf[Sym[A]])
    
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]

}

trait ArrayLoopsMCFatExp extends ArrayLoopsMCExp with LoopsFatExp with IfThenElseFatExp


trait ScalaGenArrayLoopsMCFat extends ScalaGenLoopsFat {
  val IR: ArrayLoopsMCFatExp
  import IR._

  val replaceIfThenElseSingletonEmptyWithAppend = scala.collection.mutable.HashMap[Sym[Any], Sym[Any]]()

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SuperEmptyArray() => 
      stream.println("val " + quote(sym) + ": " + sym.tp + " = Array.empty")
    case s@SuperSingleton(elem) =>
      stream.println("val " + quote(sym) + " = Array(" + quote(elem) + ")")
    case ArrayIndex(a,i) =>  
      emitValDef(sym, quote(a) + ".apply(" + quote(i) + ")")
    case ArrayLength(a) =>  
      emitValDef(sym, quote(a) + ".length")
    case Reflect(SimpleLoop(shape,index,body), _, _) => 
      emitFatNode(List(sym), SimpleFatLoop(shape, index, List(body)))
    case _ => super.emitNode(sym, rhs)
  }

  def arrBuilderType[T](tp: Manifest[T]) = 
    if (tp.typeArguments.isEmpty) {
      "ERROR: cannot build array of type " + tp
    } else if (isPrimitiveType(tp.typeArguments(0))) {
      "scala.collection.mutable.ArrayBuilder.of" + tp.typeArguments(0)
    } else "scala.collection.mutable.ArrayBuilder.ofRef"



  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case EatReflect(SimpleFatIfThenElse(cond, thenList, elseList)) =>
      val singleEmpty = sym.zip(thenList zip elseList).map({
        case (iteSym, (Block(Def(SuperSingleton(thenp))), Block(Def(SuperEmptyArray())))) =>
          replaceIfThenElseSingletonEmptyWithAppend.get(iteSym) match {
            case Some(arraySym) => Some((arraySym, thenp, iteSym))
            case _ => None
          }
        case _ => None
      })
      if (singleEmpty.forall(_.isDefined)) {
        stream.println("if (" + quote(cond) + ") {")
        singleEmpty.foreach({
          case Some((arraySym, thenp, iteSym)) =>
            emitBlock(Block(thenp))
            stream.println(quote(arraySym) + "_builder += " + quote(thenp))
            replaceIfThenElseSingletonEmptyWithAppend.remove(iteSym)
          case None => assert(false)
        })
        stream.println("}")
      } else {
        super.emitFatNode(sym, rhs)
      }

    case SimpleFatLoop(s,x,rhs2) => 
      val symRhs: List[(Sym[Any], Def[Any])] = sym zip rhs2

      val blockSyms = symRhs.flatMap({ t: (Sym[Any], Def[Any]) =>
        val l = t._1
        t._2 match {
          case MultiArrayElem(y) => (getBlockResult(y) match {
            case Def(SuperSingleton(e)) => 
              // TODO what about effects before e etc?
              stream.println("val " + quote(l) + " = new Array[" + e.tp + "]("+quote(s)+")")
              List(e) // :: getEffects(y) adds singleton node, but could need other effects?
            case ite@Def(EatReflect(IfThenElse(cond, Block(Def(SuperSingleton(thenp))), Block(Def(SuperEmptyArray()))))) =>
              stream.println("val " + quote(l) + "_builder = new " + arrBuilderType(getBlockResult(y).tp))
              replaceIfThenElseSingletonEmptyWithAppend.put(ite.asInstanceOf[Sym[Any]], l)
              syms(t._2)
            case _ =>
              stream.println("val " + quote(l) + "_builder = new " + arrBuilderType(getBlockResult(y).tp))
              syms(t._2) 
          }) ::: effectSyms(y)
          case MultiReduceElem(vFblock@Block(valFunc), redFunc, zero, accSym, valSym) => 
            stream.println("var " + quote(accSym) + " = " + quote(zero))
            (getBlockResult(vFblock) match {
              case Def(SuperSingleton(si)) => List(si) 
              case _ => List(valFunc)
            }) ::: effectSyms(vFblock) ::: effectSyms(redFunc)
          case ForeachElem(y) => syms(t._2) ::: effectSyms(y)
        }
      })
        
      stream.println("for ("+quote(x)+" <- 0 until "+quote(s)+") {")

      emitFatBlock((blockSyms).map(Block(_)))

      symRhs.foreach({ t: (Sym[Any], Def[Any]) =>
        val l = t._1
        t._2 match {
          case MultiArrayElem(y) => getBlockResult(y) match {
            case Def(SuperSingleton(e)) => 
              stream.println(quote(l) + "("+quote(x)+") = " + quote(e))
            case ite@Def(EatReflect(IfThenElse(cond, Block(Def(SuperSingleton(thenp))), Block(Def(SuperEmptyArray()))))) =>
              if (replaceIfThenElseSingletonEmptyWithAppend.contains(ite.asInstanceOf[Sym[Any]])) {
                // if-then-else wasn't replaced, probably because part of fat TTP
                stream.println(quote(l) + "_builder ++= " + quote(getBlockResult(y)))
              } 
            case _ => 
              stream.println(quote(l) + "_builder ++= " + quote(getBlockResult(y)))
          }
          case MultiReduceElem(valFunc, redFunc, zero, accSym, valSym) =>
            getBlockResult(valFunc) match {
              case Def(SuperSingleton(si)) =>
                stream.println("val " + quote(valSym) + " = " + quote(si))
                emitBlock(redFunc)
                stream.println(quote(accSym) + " = " + quote(getBlockResult(redFunc)))
              case _ =>  
                val index = fresh[Int]
                
                stream.println("for (" + quote(index) + " <- 0 until " + quote(getBlockResult(valFunc)) + ".length) {")
                stream.println("val " + quote(valSym) + " = " + quote(getBlockResult(valFunc)) + ".apply(" + quote(index) + ")")
                emitBlock(redFunc)
                stream.println(quote(accSym) + " = " + quote(getBlockResult(redFunc)))
                stream.println("}")
            }
          case ForeachElem(_) =>          
        }
      })

      stream.println("}")

      symRhs.foreach({ t: (Sym[Any], Def[Any]) =>
        val l = t._1
        t._2 match {
          case MultiArrayElem(y) => getBlockResult(y) match {
            case Def(EatReflect(SuperSingleton(e))) => 
            case _ => stream.println("val " + quote(l) + " = " + quote(l) + "_builder.result()")
          }
          case MultiReduceElem(valFunc, redFunc, zero, accSym, valSym) =>
            stream.println("val " + quote(l) + " = " + quote(accSym)) // better way to return result?

          // result of foreach is Unit, need to emit val because might be
          // quoted in if-then-else or elsewhere as block result
          case ForeachElem(_) => stream.println("val " + quote(l) + " = ()")         
        }
      })

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

  override def unapplyEmptyColl(a: Def[Any]): Boolean = a match {
    case SuperEmptyArray() => true
    case _ => super.unapplyEmptyColl(a)
  }
  override def unapplyEmptyCollNewEmpty[T:Manifest](a: (Def[Any], Exp[T], Option[Sym[Int]])): Option[Exp[T]] = a match {
    case (SuperEmptyArray(), b, None) => 
      Some(emptyArray()(mtype(b.tp.typeArguments(0))).asInstanceOf[Exp[T]])
    case (SuperEmptyArray(), b, Some(index)) => 
      Some(emptyArrayInLoop(index)(mtype(b.tp.typeArguments(0))).asInstanceOf[Exp[T]])
    case _ => super.unapplyEmptyCollNewEmpty[T](a)
  }
  override def unapplySingletonColl(a: Def[Any]): Option[Exp[Any]] = a match {
    case SuperSingleton(e) => Some(e)
    case _ => super.unapplySingletonColl(a)
  }

  override def unapplyMultiCollect[T](e: Def[T]) = e match {
    // Some((a.asInstanceOf[Exp[T]], Some((() => {
    //     emptyArray()(mtype(a.tp.typeArguments(0)))
    //   }).asInstanceOf[() => Exp[T]])))
    case MultiArrayElem(Block(a: Exp[T])) => Some(a.asInstanceOf[Exp[T]])
    case _ => super.unapplyMultiCollect(e)
  }

  override def unapplyFor(e: Def[Unit]) = e match {
    case ForeachElem(Block(a: Exp[Unit @unchecked])) =>
      Some(a.asInstanceOf[Exp[Unit]])
    case _ => super.unapplyFor(e)
  }

  override def unapplyReduce[T](e: Def[T]) = e match {
    case MultiReduceElem(Block(valFunc), _, _, _, _) =>
      Some(valFunc.asInstanceOf[Exp[T]])
    case _ => super.unapplyReduce(e)
  }

  override def ignoreIndex(e: Def[Any], index: Sym[Int]): Boolean = e match {
    case SingletonInLoop(_, `index`) => true
    case EmptyArrayInLoop(`index`,_) => true
    case _ => super.ignoreIndex(e, index)
  }

  override def shouldReuniquifyIndices = false

  // override def unapplyForeach(e: Def[Any]) = e match {
  //   case ForeachElem(Block(a: Exp[Unit])) => Some(a)
  //   case _ => super.unapplyForeach(e)
  // }

}


