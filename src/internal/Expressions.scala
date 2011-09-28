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
    def Type : Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceInfo = Thread.currentThread.getStackTrace // until we can get useful info out of the manifest
  }

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  case class External[A:Manifest](s: String, fmt_args: List[Exp[Any]] = List()) extends Exp[A]
      
  var nVars = 0
  def fresh[T:Manifest] = Sym[T] { nVars += 1; nVars -1 }

  abstract class Def[+T] // operations (compos  ite)

  case class TP[+T](sym: Sym[T], rhs: Def[T]) 

  var globalDefs: List[TP[Any]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] =
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]

  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]

  def findOrCreateDefinition[T:Manifest](d: Def[T]): TP[T] =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T], d)
    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  protected implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    findOrCreateDefinition(d).sym // TODO: return Const(()) if type is Unit??
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).map(_.rhs)
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
