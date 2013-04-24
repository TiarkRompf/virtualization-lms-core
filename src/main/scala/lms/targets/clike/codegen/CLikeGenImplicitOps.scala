package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.reflect.SourceContext
import ops.ImplicitOpsExp

trait CLikeGenImplicitOps extends CLikeGenBase {
  val IR: ImplicitOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case im@ImplicitConvert(x) =>
          stream.println("%s %s = (%s)%s;".format(remap(im.mY), quote(sym), remap(im.mY), quote(x)))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenImplicitOps extends CudaGenBase with CLikeGenImplicitOps
trait OpenCLGenImplicitOps extends OpenCLGenBase with CLikeGenImplicitOps
trait CGenImplicitOps extends CGenBase with CLikeGenImplicitOps
