package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import ops.BaseGenDSLOps


trait CLikeGenDSLOps extends BaseGenDSLOps with CLikeGenBase {
  val IR: DSLOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: DSLOp[_] => throw new GenerationFailedException("CLikeGenDSLOps: DSLOp is not supported")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDSLOps extends CudaGenEffect with CLikeGenDSLOps
trait OpenCLGenDSLOps extends OpenCLGenEffect with CLikeGenDSLOps
trait CGenDSLOps extends CGenEffect with CLikeGenDSLOps

