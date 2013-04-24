package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import ops.EqualExp

trait CLikeGenEqual extends CLikeGenBase {
  val IR: EqualExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case Equal(a,b) =>
          emitValDef(sym, quote(a) + " == " + quote(b))
        case NotEqual(a,b) =>
          emitValDef(sym, quote(a) + " != " + quote(b))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenEqual extends CudaGenBase with CLikeGenEqual
trait OpenCLGenEqual extends OpenCLGenBase with CLikeGenEqual
trait CGenEqual extends CGenBase with CLikeGenEqual