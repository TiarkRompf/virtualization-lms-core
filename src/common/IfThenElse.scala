package scala.virtualization.lms
package common



import java.io.PrintWriter

trait IfThenElse extends Base {
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]
}

// TODO: it would be nice if IfThenElseExp would extend IfThenElsePureExp
// but then we would need to give it a different name.

trait IfThenElsePureExp extends IfThenElse with BaseExp {
  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]
  
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = IfThenElse(cond, thenp, elsep)
}


trait IfThenElseExp extends IfThenElse with EffectExp {
  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]
  override def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyEffects(thenp)
    val b = reifyEffects(elsep)
    (a,b) match {
      case (Def(Reify(_,_)), _) | (_, Def(Reify(_,_))) => reflectEffect(IfThenElse(cond,a,b))
      case _ => IfThenElse(cond, thenp, elsep)
    }
  }
}


trait ScalaGenIfThenElse extends ScalaGenEffect with IfThenElseExp {
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