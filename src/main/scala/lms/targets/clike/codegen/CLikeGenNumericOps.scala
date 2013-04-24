package scala.lms
package targets.clike.codegen

import ops.NumericOpsExp

import java.io.PrintWriter

trait CLikeGenNumericOps extends CLikeGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case NumericPlus(a,b) =>
          emitValDef(sym, quote(a) + " + " + quote(b))
        case NumericMinus(a,b) =>
          emitValDef(sym, quote(a) + " - " + quote(b))
        case NumericTimes(a,b) =>
          emitValDef(sym, quote(a) + " * " + quote(b))
        case NumericDivide(a,b) =>
          emitValDef(sym, quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenNumericOps extends CudaGenBase with CLikeGenNumericOps
trait OpenCLGenNumericOps extends OpenCLGenBase with CLikeGenNumericOps
trait CGenNumericOps extends CGenBase with CLikeGenNumericOps

