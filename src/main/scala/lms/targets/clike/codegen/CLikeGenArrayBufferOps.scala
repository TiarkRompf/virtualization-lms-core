package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.lms.ops.BaseGenArrayBufferOps


trait CLikeGenArrayBufferOps extends BaseGenArrayBufferOps with CLikeGenBase {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayBufferOps extends CudaGenEffect with CLikeGenArrayBufferOps
trait OpenCLGenArrayBufferOps extends OpenCLGenEffect with CLikeGenArrayBufferOps
trait CGenArrayBufferOps extends CGenEffect with CLikeGenArrayBufferOps

