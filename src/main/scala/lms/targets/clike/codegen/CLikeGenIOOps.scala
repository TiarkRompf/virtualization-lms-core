package scala.lms
package targets.clike.codegen

import internal.{GenerationFailedException}
import ops.IOOpsExp

import java.io.{File, FileReader, FileWriter, BufferedReader, BufferedWriter, PrintWriter}

trait CLikeGenIOOps extends CLikeGenBase {
  val IR: IOOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjBrApply(f) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case ObjFrApply(s) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case BrReadline(b) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case BrClose(b) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CudaGenIOOps extends CudaGenBase with CLikeGenIOOps
trait OpenCLGenIOOps extends OpenCLGenBase with CLikeGenIOOps
trait CGenIOOps extends CGenBase with CLikeGenIOOps


