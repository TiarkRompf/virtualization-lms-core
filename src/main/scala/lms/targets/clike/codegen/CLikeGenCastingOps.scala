package scala.lms
package targets.clike.codegen

import ops.CastingOpsExp

import java.io.PrintWriter

trait CLikeGenCastingOps extends CLikeGenBase { 
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        //case RepIsInstanceOf(x,mA,mB) => //TODO: How?
        case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, "(%s) %s".format(remap(mB),quote(x)))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenCastingOps extends CudaGenBase with CLikeGenCastingOps 
trait OpenCLGenCastingOps extends OpenCLGenBase with CLikeGenCastingOps 
trait CGenCastingOps extends CGenBase with CLikeGenCastingOps 
