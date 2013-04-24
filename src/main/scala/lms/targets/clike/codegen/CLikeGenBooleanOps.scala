package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.reflect.SourceContext
import ops.BooleanOpsExp


trait CLikeGenBooleanOps extends CLikeGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
      case BooleanAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
      case BooleanOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " || " + quote(rhs))
      case _ => super.emitNode(sym,rhs)
    }
  }
}

trait CudaGenBooleanOps extends CudaGenBase with CLikeGenBooleanOps
trait OpenCLGenBooleanOps extends OpenCLGenBase with CLikeGenBooleanOps
trait CGenBooleanOps extends CGenBase with CLikeGenBooleanOps
