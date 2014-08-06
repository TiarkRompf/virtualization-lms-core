/*TODO DISABLED
package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}

//TODO: is this used at all? should it be merge with DeliteOps?

//TODO rename this to something more meaningful

trait DSLOpsExp extends EffectExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).
  class DSLOp[A](val representation: Block[A]) extends Def[A]
}

trait BaseGenDSLOps extends GenericNestedCodegen {
  val IR: DSLOpsExp
  import IR._
}

trait ScalaGenDSLOps extends ScalaGenEffect with BaseGenDSLOps {
  val IR: DSLOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: DSLOp[_] =>
      val b = op.representation
      gen"""val $sym = {
           |${nestedBlock(b)}
           |$b
           |}"""

    case _ => super.emitNode(sym, rhs)
  }

}

trait CLikeGenDSLOps extends BaseGenDSLOps with CLikeGenBase {
  val IR: DSLOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: DSLOp[_] => throw new GenerationFailedException("CLikeGenDSLOps: DSLOp is not supported")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDSLOps extends CudaGenEffect with CLikeGenDSLOps
trait OpenCLGenDSLOps extends OpenCLGenEffect with CLikeGenDSLOps
trait CGenDSLOps extends CGenEffect with CLikeGenDSLOps

*/
