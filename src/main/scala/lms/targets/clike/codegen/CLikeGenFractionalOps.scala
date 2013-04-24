package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.reflect.SourceContext
import ops.FractionalOpsExp

trait CLikeGenFractionalOps extends CLikeGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case FractionalDivide(a,b) =>
          emitValDef(sym, quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
     }
    }
}

trait CudaGenFractionalOps extends CudaGenBase with CLikeGenFractionalOps
trait OpenCLGenFractionalOps extends OpenCLGenBase with CLikeGenFractionalOps
trait CGenFractionalOps extends CGenBase with CLikeGenFractionalOps
