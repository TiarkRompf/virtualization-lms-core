package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{Base, BaseExp}
import scala.virtualization.lms.internal.ScalaGenBase

trait Equal extends Base with Variables with OverloadHack {
  // TODO: we need a better way of handling this, too many combinations
  // this occurs because Rep is an object, so it has an == and != method defined by default,
  // so there is no type error to force the implicit conversions
  def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1) : Rep[Boolean] = equals(a,b)
  def __equal[A,B](a: Rep[A], b: Var[B])(implicit o: Overloaded2) : Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: Var[A], b: Rep[B])(implicit o: Overloaded3) : Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded4): Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded5): Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: Var[A], b: B)(implicit o: Overloaded6): Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: A, b: Var[B])(implicit o: Overloaded7): Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: Var[A], b: Var[B])(implicit o: Overloaded8) : Rep[Boolean] = equals(a,b)
  // TODO: this will probably never get called due to original, equally specific ==
  def __equal[A,B](a: A, b: B) : Rep[Boolean] = equals(a,b)

  def infix_!=[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1) : Rep[Boolean] = notequals(a,b)
  def infix_!=[A,B](a: Rep[A], b: Var[B])(implicit o: Overloaded2) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Var[A], b: Rep[B])(implicit o: Overloaded3) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Rep[A], b: B)(implicit o: Overloaded4) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: A, b: Rep[B])(implicit o: Overloaded5) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Var[A], b: B)(implicit o: Overloaded6) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: A, b: Var[B])(implicit o: Overloaded7) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Var[A], b: Var[B])(implicit o: Overloaded8) : Rep[Boolean] = notequals(a,b)
  // TODO: this will probably never get called due to original, equally specific !=
  def infix_!=[A,B](a: A, b: B) : Rep[Boolean] = notequals(a,b)


  def equals[A,B](a: Rep[A], b: Rep[B]) : Rep[Boolean]
  def notequals[A,B](a: Rep[A], b: Rep[B]) : Rep[Boolean]
}

trait EqualExp extends Equal with VariablesExp {
  case class Equal[A,B](a: Exp[A], b: Exp[B]) extends Def[Boolean]
  case class NotEqual[A,B](a: Exp[A], b: Exp[B]) extends Def[Boolean]

  def equals[A,B](a: Rep[A], b: Rep[B]): Rep[Boolean] = Equal(a,b)
  def notequals[A,B](a: Rep[A], b: Rep[B]): Rep[Boolean] = NotEqual(a,b)
}

trait ScalaGenEqual extends ScalaGenBase {
  val IR: EqualExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
    case NotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
