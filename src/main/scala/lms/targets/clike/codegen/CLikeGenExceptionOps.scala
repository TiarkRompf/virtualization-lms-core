package scala.lms
package targets.clike.codegen

import ops.ExceptionOpsExp

trait CLikeGenExceptionOps extends CLikeGenBase {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ThrowException(m) => 
      stream.println("printf(" + quote(m) + ");")
      stream.println("assert(false);")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenExceptionOps extends CGenBase with CLikeGenExceptionOps
trait CudaGenExceptionOps extends CudaGenBase with CLikeGenExceptionOps
//OpenCL does not support printf within a kernel
//trait OpenCLGenExceptionOps extends OpenCLGenBase with CLikeGenExceptionOps
