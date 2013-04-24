package scala.lms
package targets.clike.codegen

import ops.OrderingOpsExp

import java.io.PrintWriter

trait CLikeGenOrderingOps extends CLikeGenBase {
  val IR: OrderingOpsExp
  import IR._
  
  // TODO: Add MIN/MAX macro needs to C-like header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case OrderingLT(a,b) =>
          emitValDef(sym, quote(a) + " < " + quote(b))
        case OrderingLTEQ(a,b) =>
          emitValDef(sym, quote(a) + " <= " + quote(b))
        case OrderingGT(a,b) =>
          emitValDef(sym, quote(a) + " > " + quote(b))
        case OrderingGTEQ(a,b) =>
          emitValDef(sym, quote(a) + " >= " + quote(b))
        case OrderingEquiv(a,b) =>
          emitValDef(sym, quote(a) + " == " + quote(b))
        case OrderingMax(a,b) =>
          emitValDef(sym, "MAX(" + quote(a) + ", " + quote(b) + ")")
        case OrderingMin(a,b) =>
          emitValDef(sym, "MIN(" + quote(a) + ", " + quote(b) + ")")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenOrderingOps extends CudaGenBase with CLikeGenOrderingOps
trait OpenCLGenOrderingOps extends OpenCLGenBase with CLikeGenOrderingOps
trait CGenOrderingOps extends CGenBase with CLikeGenOrderingOps

