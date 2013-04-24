package scala.lms
package targets.clike.codegen

import internal.GenericNestedCodegen
import ops.{BaseGenSynchronizedArrayBufferOps, SynchronizedArrayBufferOpsExp}

import java.io.PrintWriter

trait CLikeGenSynchronizedArrayBufferOps extends BaseGenSynchronizedArrayBufferOps with CLikeGenArrayBufferOps {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenSynchronizedArrayBufferOps extends CudaGenEffect with CLikeGenSynchronizedArrayBufferOps
trait OpenCLGenSynchronizedArrayBufferOps extends OpenCLGenEffect with CLikeGenSynchronizedArrayBufferOps
trait CGenSynchronizedArrayBufferOps extends CGenEffect with CLikeGenSynchronizedArrayBufferOps

