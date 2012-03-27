package scala.virtualization.lms
package internal

import scala.annotation.unchecked.uncheckedVariance
import java.lang.{StackTraceElement,Thread}

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def tp: Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    @transient var sourceInfo = Thread.currentThread.getStackTrace // until we can get useful info out of the manifest
  }

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  var nVars = 0
  def fresh[T:Manifest] = Sym[T] { nVars += 1; nVars -1 }

  abstract class Def[+T] // operations (composite)

  abstract class Stm // statement (links syms and definitions)
  
  def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TP(sym, rhs) => List(sym)
  }
  
  def infix_rhs(stm: Stm): Any = stm match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
    case TP(sym, rhs) => rhs
  }

  def infix_defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TP(`sym`, rhs: Def[A]) => Some(rhs)
    case _ => None
  }

  def infix_defines[A](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TP(sym: Sym[A], `rhs`) => Some(sym)
    case _ => None
  }
  
  case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm

  // graph construction state
  
  var globalDefs: List[Stm] = Nil
  var localDefs: List[Stm] = Nil

  def reifySubGraph[T](b: =>T): (T, List[Stm]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    localDefs = Nil
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    (r, defs)
  }

  def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    val existing = globalDefs filter (_.lhs exists (lhs contains _))
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
  }

  def findDefinition[T](s: Sym[T]): Option[Stm] =
    globalDefs.find(x => x.defines(s).nonEmpty)

  def findDefinition[T](d: Def[T]): Option[Stm] =
    globalDefs.find(x => x.defines(d).nonEmpty)

  def findOrCreateDefinition[T:Manifest](d: Def[T]): Stm =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T], d)
    }

  def findOrCreateDefinitionExp[T:Manifest](d: Def[T]): Exp[T] =
    findOrCreateDefinition(d).defines(d).get

  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    val f = TP(s, d)
    reflectSubGraph(List(f))
    f
  }
  

  protected implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    findOrCreateDefinitionExp(d) // TODO: return Const(()) if type is Unit??
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).flatMap(_.defines(s))
      case _ =>
        None
    }
  }


  // dependencies

  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }

  def effectSyms(x: Any): List[Sym[Any]] = x match {
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }

  def softSyms(e: Any): List[Sym[Any]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(softSyms(_))
    case _ => Nil
  }


  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
//    case _ => rsyms(e)(symsFreq)
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))



/*
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }
*/

/*
  def symsShare(e: Any): List[(Sym[Any], Int)] = {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(symsShare(_))
    case _ => Nil
  }
*/



  // bookkeeping

  def reset { // used by delite?
    nVars = 0
    globalDefs = Nil
  }

}
