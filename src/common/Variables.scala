package scala.virtualization.lms
package common

import util.OverloadHack
import java.io.PrintWriter

trait Variables extends Base with OverloadHack {
  
  type Var[T] = T // TODO: try not setting it not = T
  
  // TODO: fix -- this only resolves to the correct method in some cases
  def __newVar[T](init: Rep[T])(implicit o: Overloaded1): Var[Rep[T]]
  def __assign[T](lhs: Var[Rep[T]], rhs: Rep[T]) : Rep[Unit]
  
//  implicit def readVar[T](v: Var[Rep[T]]): Rep[T]

  // TODO: need only one of the two or both?

//  implicit def readVar[T](v: Var[T]): Rep[T]
//  implicit def chainReadVar[T,U](x: Var[T])(implicit f: Rep[T] => U): U = f(readVar(x))

}

trait VariablesExp extends Variables with EffectExp {
//  type Var[T] = T
  case class NewVar[T](init: Rep[T]) extends Def[T]
  case class Assign[T](lhs: Rep[T], rhs: Rep[T]) extends Def[T]

  def __newVar[T](init: Rep[T])(implicit o: Overloaded1): Rep[T] = {
    reflectEffect(NewVar(init))
  }

  def __assign[T](lhs: Rep[T], rhs: Rep[T]): Rep[Unit] = {
    reflectEffect(Assign(lhs, rhs))
    Const()
  }  
}


trait ScalaGenVariables extends ScalaGenEffect with VariablesExp {
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case NewVar(init) => emitVarDef(sym, quote(init))
    case Assign(a, b) => emitAssignment(quote(a), quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}