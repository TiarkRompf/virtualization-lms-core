package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenEffect, ScalaGenEffect}

trait IfThenElse extends Base {
  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]
}

// TODO: it would be nice if IfThenElseExp would extend IfThenElsePureExp
// but then we would need to give it a different name.

trait IfThenElsePureExp extends IfThenElse with BaseExp {

  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = IfThenElse(cond, thenp, elsep)
}


trait IfThenElseExp extends IfThenElse with EffectExp {

  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyEffects(thenp)
    val b = reifyEffects(elsep)
    (a,b) match {
      case (Def(Reify(_,_)), _) | (_, Def(Reify(_,_))) => reflectEffect(IfThenElse(cond,a,b))
      case _ => IfThenElse(cond, thenp, elsep)
    }
  }

}


trait ScalaGenIfThenElse extends ScalaGenEffect {
  val IR: IfThenElseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenIfThenElse extends CudaGenEffect {
  val IR: IfThenElseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {	
    case IfThenElse(c, t, e) => getFreeVarBlock(c,Nil) ::: getFreeVarBlock(t,Nil) ::: getFreeVarBlock(e,Nil)
    case _ => super.getFreeVarNode(rhs)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case IfThenElse(c,a,b) =>

      stream.println(addTab()+"%s %s;".format(CudaType(a.Type.toString),quote(sym)))
      stream.println(addTab() + "if (" + quote(c) + ") {")
      tabWidth += 1
      emitBlock(a)
      stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(a))))
      tabWidth -= 1
      stream.println(addTab() + "} else {")
      tabWidth += 1
      emitBlock(b)
      stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(b))))
      tabWidth -= 1
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}