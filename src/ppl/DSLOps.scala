package scala.virtualization.lms
package ppl

import java.io.PrintWriter
import scala.virtualization.lms.common.{ScalaGenEffect, FunctionsExp}

/**
 * This entire trait could (and probably should) be moved to the Delite framework.
 */
trait DSLOpsExp extends FunctionsExp {

  case class DSLOp[A,B](task: Exp[A] => Exp[B], arg: Exp[A]) extends Def[B] {
    lazy val representation = reifyEffects(task(arg))
  }

}

trait ScalaGenDSL extends ScalaGenEffect with DSLOpsExp  {
  override def syms(e: Any): List[Sym[Any]] = e match {
    case DSLOp(func, arg) if shallow => syms(arg) // in shallow mode, don't count deps from nested blocks
    // this is needed to prevent potential forward reference errors; not clear to me why this is different then super.syms(e) 
    case op@DSLOp(func, arg) => syms(arg) ++ syms(op.representation)
    case _ => super.syms(e)
  }

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {    
    case op@DSLOp(func, arg) =>
      val b = op.representation      
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
