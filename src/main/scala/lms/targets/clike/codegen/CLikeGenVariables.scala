package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import ops.VariablesExp


trait CLikeGenVariables extends CLikeGenBase {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ReadVar(Variable(a)) =>
          emitValDef(sym, quote(a))
        case NewVar(init) =>
          emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
        case Assign(Variable(a), b) =>
          emitAssignment(quote(a), quote(b))
        case VarPlusEquals(Variable(a), b) =>
          emitAssignment(quote(a), quote(a) + " + " + quote(b))
        case VarMinusEquals(Variable(a), b) =>
          emitAssignment(quote(a), quote(a) + " - " + quote(b))
        case VarTimesEquals(Variable(a), b) =>
          emitAssignment(quote(a), quote(a) + " * " + quote(b))
        case VarDivideEquals(Variable(a), b) =>
          emitAssignment(quote(a), quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenVariables extends CudaGenEffect with CLikeGenVariables
trait OpenCLGenVariables extends OpenCLGenEffect with CLikeGenVariables
trait CGenVariables extends CGenEffect with CLikeGenVariables
