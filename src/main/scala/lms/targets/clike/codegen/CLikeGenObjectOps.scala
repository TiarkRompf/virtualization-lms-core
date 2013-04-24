package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.lms.internal.{GenerationFailedException}
import ops.ObjectOpsExp


trait CLikeGenObjectOps extends CLikeGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(lhs) => emitValDef(sym, "(" + quote(lhs) + ").toString()")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, quote(x) + "; // unsafe immutable")
    case ObjectUnsafeMutable(x) => emitValDef(sym, quote(x) + "; // unsafe mutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenObjectOps extends CudaGenBase with CLikeGenObjectOps
trait OpenCLGenObjectOps extends OpenCLGenBase with CLikeGenObjectOps
trait CGenObjectOps extends CGenBase with CLikeGenObjectOps
