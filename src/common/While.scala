package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CGenEffect, CudaGenEffect, GenericNestedCodegen, ScalaGenEffect}

trait While extends Base {
  def __whileDo(cond: => Rep[Boolean], body: => Rep[Unit])
}


trait WhileExp extends While with FunctionsExp { 
  case class While(cond: Exp[Boolean], body: Exp[Unit]) extends Def[Unit]

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {
    val c = reifyEffects(cond)
    val a = reifyEffects(body)
    reflectEffect(While(c, a))
  }
}


trait BaseGenWhile extends GenericNestedCodegen {
  val IR: WhileExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    // we want to hoist things out of the loop if possible, so we count nested free deps as well
    //case While(c, b) if shallow => getFreeVarBlock(b,Nil).asInstanceOf[List[Sym[Any]]]
    case While(c, b) if shallow => Nil
    case _ => super.syms(e)
  }

  // TODO: What about condition node?
  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case While(c,b) => getFreeVarBlock(c,Nil) ::: getFreeVarBlock(b,Nil)
    case _ => super.getFreeVarNode(rhs)
  }
  
}

trait ScalaGenWhile extends ScalaGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case While(c,b) =>
      stream.print("val " + quote(sym) + " = while ({")
      emitBlock(c)
      stream.print(quote(getBlockResult(c)))
      stream.println("}) {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenWhile extends CudaGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case While(c,b) =>
            // Get free variables list
            val freeVars = getFreeVarBlock(c,Nil)
            val argListStr = freeVars.map(quote(_)).mkString(", ") 

            // emit function for the condition evaluation
            val condFunc = emitDevFunc(c, getBlockResult(c).Type, freeVars)

            // Emit while loop (only the result variable of condition)
            stream.print(addTab() + "while (")
            stream.print("%s(%s)".format(condFunc,argListStr))
            stream.println(") {")
            tabWidth += 1
            emitBlock(b)
            tabWidth -= 1
            //stream.println(quote(getBlockResult(b)))   //TODO: Is this needed?
            stream.println(addTab() + "}")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CGenWhile extends CGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case While(c,b) =>
        // calculate condition
        emitBlock(c)
        stream.println("bool cond_%s = %s;".format(quote(sym),quote(getBlockResult(c))))
        // Emit while loop
        stream.print("while (cond_%s) {".format(quote(sym)))
        emitBlock(b)
        stream.println("}")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
