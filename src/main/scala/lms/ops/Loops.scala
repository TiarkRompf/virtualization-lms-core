package scala.lms
package ops

import internal.{FatBlockTraversal, GenericNestedCodegen, GenericFatCodegen}

import java.io.PrintWriter
import scala.reflect.SourceContext

trait Loops extends Base { // no surface constructs for now

}

/** Loop expression with only one function as its body */
trait LoopsExp extends Loops with BaseExp with EffectExp {

  /** AbstractLoop is a representation of a loop consisting of:
    *   size: this loop will iterate from 0 to size-1
    *   v: the variable containing the current element of the loop
    *   body: a function that should be applied to v in each iteration of this loop
    */
  abstract class AbstractLoop[A] extends Def[A] {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: Def[A]
  }

  /** Loop object with unapply method as an extractor is used in pattern matching */
  object Loop {
    def unapply[A](l: AbstractLoop[A]): Option[(Exp[Int], Sym[Int], Def[A])] = Some((l.size, l.v, l.body))
  }

  /** A trivial implementation of AbstractLoop */
  case class SimpleLoop[A](val size: Exp[Int], val v: Sym[Int], val body: Def[A]) extends AbstractLoop[A]

  /** Function for creating SimpleLoop, having its parameter */
  def simpleLoop[A:TypeRep](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = SimpleLoop(size, v, body)


  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => syms(e.size) ::: syms(e.body) // should add super.syms(e) ?? not without a flag ...
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => readSyms(e.size) ::: readSyms(e.body)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => e.v :: boundSyms(e.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: AbstractLoop[_] => freqNormal(e.size) ::: freqHot(e.body) // should add super.symsFreq(e) ?? not without a flag ...
    case _ => super.symsFreq(e)
  }


  //////////////
  // mirroring

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s,v,body: Def[A]) => simpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => aliasSyms(e.body)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => containSyms(e.body)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => extractSyms(e.body)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => copySyms(e.body)
    case _ => super.copySyms(e)
  }
}

/** Loop expression with several functions as its body */
trait LoopsFatExp extends LoopsExp with BaseFatExp {

  /** AbstractFatLoop is a representation of a loop consisting of:
    *   size: this loop will iterate from 0 to size-1
    *   v: the variable containing the current element of the loop
    *   body: a list of functions that should be applied to v in each iteration of this loop
    */
  abstract class AbstractFatLoop extends FatDef {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: List[Def[Any]]
  }

  /** A trivial implementation of AbstractFatLoop */
  case class SimpleFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop


  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => syms(e.size) ::: syms(e.body)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => readSyms(e.size) ::: readSyms(e.body)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => e.v :: boundSyms(e.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: AbstractFatLoop => freqNormal(e.size) ::: freqHot(e.body)
    case _ => super.symsFreq(e)
  }

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => aliasSyms(e.body)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => containSyms(e.body)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => extractSyms(e.body)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => copySyms(e.body)
    case _ => super.copySyms(e)
  }
}


trait BaseLoopsTraversalFat extends FatBlockTraversal {
  val IR: LoopsFatExp
  import IR._

  override def fatten(e: Stm): Stm = e match {
    case TP(sym, op: AbstractLoop[_]) =>
      TTP(List(sym), List(op), SimpleFatLoop(op.size, op.v, List(op.body)))
    case TP(sym, p @ Reflect(op: AbstractLoop[_], u, es)) if !u.maySimple && !u.mayGlobal => // assume body will reflect, too. bring it on...
      printdbg("-- fatten effectful loop " + e)
      TTP(List(sym), List(p), SimpleFatLoop(op.size, op.v, List(op.body)))
    case _ => super.fatten(e)
  }

}

trait BaseGenLoops extends GenericNestedCodegen {
  val IR: LoopsExp
  import IR._

}

trait BaseGenLoopsFat extends BaseGenLoops with BaseLoopsTraversalFat with GenericFatCodegen {
  val IR: LoopsFatExp
  import IR._

}

trait ScalaGenLoops extends ScalaGenBase with BaseGenLoops {
  import IR._

  //TODO

}

trait ScalaGenLoopsFat extends ScalaGenLoops with ScalaGenFat with BaseGenLoopsFat {
  import IR._

  //TODO

}
