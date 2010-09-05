package scala.virtualization.lms
package common

import util.OverloadHack
import java.io.PrintWriter

trait Variables extends Base with OverloadHack {
  type Var[T]

  implicit def varToRep[T](v: Var[T]) : Rep[T]

  def __newVar[T](init: Rep[T])(implicit o: Overloaded1): Var[T]
  def __assign[T](lhs: Var[T], rhs: Rep[T]) : Rep[Unit]

//  implicit def readVar[T](v: Var[Rep[T]]): Rep[T]
}

trait VariablesExp extends Variables with EffectExp {
  type Var[+T] = Variable[T]
  // TODO: make a design decision here.
  // defining Var[T] as Sym[T] is dangerous. If someone forgets to define a more-specific implicit conversion from
  // Var[T] to Ops, e.g. implicit def varToRepStrOps(s: Var[String]) = new RepStrOpsCls(varToRep(s))
  // then the existing implicit from Rep to Ops will be used, and the ReadVar operation will be lost.
  // Defining Vars as separate from Exps will always cause a compile-time error if the implicit is missing.
  //type Var[T] = Sym[T]

  // read operation
  implicit def varToRep[T](v: Var[T]) : Exp[T] = reflectEffect(ReadVar(v))

  case class ReadVar[T](v: Var[T]) extends Def[T]
  case class NewVar[T](init: Exp[T]) extends Def[T]
  case class Assign[T](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]

  def __newVar[T](init: Exp[T])(implicit o: Overloaded1): Var[T] = {
    //reflectEffect(NewVar(init)).asInstanceOf[Var[T]]
    Variable(reflectEffect(NewVar(init)))
  }

  def __assign[T](lhs: Var[T], rhs: Exp[T]): Exp[Unit] = {
    reflectEffect(Assign(lhs, rhs))
    Const()
  }  
}


trait ScalaGenVariables extends ScalaGenEffect with VariablesExp {
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym, quote(init))
    case Assign(Variable(a), b) => emitAssignment(quote(a), quote(b))
    //case Assign(a, b) => emitAssignment(quote(a), quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}