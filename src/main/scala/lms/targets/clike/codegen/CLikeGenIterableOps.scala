package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import internal._
import scala.reflect.SourceContext
import ops.{BaseGenIterableOps, IterableOpsExp}

trait CLikeGenIterableOps extends BaseGenIterableOps with CLikeGenBase {
  val IR: IterableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenIterableOps extends CudaGenBase with CLikeGenIterableOps
trait OpenCLGenIterableOps extends OpenCLGenBase with CLikeGenIterableOps
trait CGenIterableOps extends CGenBase with CLikeGenIterableOps

