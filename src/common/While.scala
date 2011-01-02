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
      stream.print("while ({")
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
            val argListStr = if(freeVars.length == 0) freeVars.map(quote(_)).mkString(", ") else ""
            val paramListStr = if(freeVars.length == 0) freeVars.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ") else ""

            // emit function for the condition evaluation
            stream.println("__device__ __host__ %s %s(%s) {".format("bool", "cond_"+quote(sym), paramListStr))
            tabWidth += 1
            emitBlock(c)
            stream.println(addTab()+"return %s;".format(quote(getBlockResult(c))))
            stream.println("}")
            tabWidth -= 1

            // Emit while loop (only the result variable of condition)
            stream.print(addTab() + "while (")
            stream.print("cond_"+quote(sym)+"(%s)".format(argListStr))
            stream.println(") {")
            tabWidth += 1
            emitBlock(b)
            tabWidth -= 1
            //stream.println(quote(getBlockResult(b)))   //TODO: Is this needed?
            stream.println("}")
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
