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
trait Expressions {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def Type : Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceInfo = Thread.currentThread.getStackTrace // until we can get useful info out of the manifest
  }

  case class Variable[+T:Manifest](val e: Exp[T]) // TODO: decide whether it should stay here ...

  case class External[A:Manifest](s: String, fmt_args: List[Exp[Any]] = List()) extends Exp[A]
      
  var nVars = 0
  def fresh[T:Manifest] = Sym[T] { nVars += 1; nVars -1 }

  abstract class Def[+T] // operations (composite)

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
    findOrCreateDefinition(d).sym
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).map(_.rhs)
      case _ =>
        None
    }
  }

  def reset { // used anywhere?
    nVars = 0
    globalDefs = Nil
  }

  // Customizable type
  type Type = Any

  /**
   *   Rich Definition object. This can keep a type and a refcount, both of which are required for the MDArray
   *   optimizations. Moreover, it allows a more scalable dependency system compared to Def/Product.
   */
  class RichDef[+T](var used: List[Exp[Any]]) extends Def[T] {
    var users: List[RichDef[Any]] = Nil
    var assertions: List[Exp[Any]] = Nil
//    var shape: S

    def addUser(user: RichDef[Any]): Unit =
      users = user :: users

    def removeUser(user: RichDef[Any]): Unit =
      users = users.filterNot(t => t == user)

    def addAssertions(_assertions: List[Exp[Any]]): RichDef[T] = {
      assertions = assertions ::: _assertions
      this
    }

    for (u <- used.flatMap(t => Def.unapply(t)).filter(t => t.isInstanceOf[RichDef[_]]))
      u.asInstanceOf[RichDef[Any]].addUser(this)

//    // TODO: Implement removal logic
//    def used_=(newused: List[Exp[Any]]): Unit = {
//      for (u <- used.flatMap(t => Def.unapply(t)).filter(t => t.isInstanceOf[RichDef[_]]))
//        u.asInstanceOf[RichDef[Any]].removeUser(this)
//
//      for (u <- newused.flatMap(t => Def.unapply(t)).filter(t => t.isInstanceOf[RichDef[_]]))
//        u.asInstanceOf[RichDef[Any]].addUser(this)
//
//      used = newused
//    }

    override def equals(other: Any) = other match {
      case that: RichDef[_] => that.canEqual(this) && used == that.used
      case _ => false
    }

    def canEqual(other: Any) = other match {
      case that: RichDef[_] => true
      case _ => false
    }
  }

  object RichDef {
     def unapply[T](e: Exp[T]): Option[List[Exp[Any]]] = e match {
      case s @ Sym(_) =>
        val rhs = findDefinition(s).toList.map(_.rhs)

        if ((rhs.length != 0) && (rhs(0).isInstanceOf[RichDef[Any]])) {
          val d = rhs(0).asInstanceOf[RichDef[Any]]
          Some(d.used ::: d.assertions)
        } else
          None
      case _ =>
        None
    }
  }


/*
  // dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }
*/
}
