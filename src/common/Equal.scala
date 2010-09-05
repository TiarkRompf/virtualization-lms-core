package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack

trait Equal extends Base with Variables with OverloadHack {
  def __equal(a: Rep[Any], b: Rep[Any]): Rep[Boolean]
  //todo this is required of things like if(a:Rep[T] == true) won't work
  def __equal(a: Rep[Any], b: Any)(implicit o: Overloaded1): Rep[Boolean] = __equal(a, unit(b))
  def __equal(a: Any, b: Rep[Any])(implicit o: Overloaded2): Rep[Boolean] = __equal(unit(a), b)

  def __ext__!=[A,B](a: Rep[A], b: Rep[B]) : Rep[Boolean] = notequals(a,b)
  def __ext__!=[A,B](a: Rep[A], b: Var[B])(implicit o: Overloaded1) : Rep[Boolean] = notequals(a, varToRep(b))
  def __ext__!=[A,B](a: Var[A], b: Rep[B])(implicit o: Overloaded2) : Rep[Boolean] = notequals(varToRep(a), b)
//  def __ext__!=[A,B](a: Rep[A], b: B)(implicit o: Overloaded3) : Rep[Boolean] = __ext__!=(a, unit(b))
//  def __ext__!=[A,B](a: A, b: Rep[B])(implicit o: Overloaded4) : Rep[Boolean] = __ext__!=(unit(a), b)

  def notequals[A,B](a: Rep[A], b: Rep[B]) : Rep[Boolean]
}

trait EqualExp extends Equal with BaseExp with VariablesExp {
  case class Equal(a: Exp[Any], b: Exp[Any]) extends Def[Boolean]
  case class NotEqual[A,B](a: Exp[A], b: Exp[B]) extends Def[Boolean]

  def __equal(a: Rep[Any], b: Rep[Any]): Rep[Boolean] = Equal(a,b)
  def notequals[A,B](a: Rep[A], b: Rep[B]): Rep[Boolean] = NotEqual(a,b)
}

trait ScalaGenEqual extends ScalaGenBase with EqualExp {
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
    case NotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
