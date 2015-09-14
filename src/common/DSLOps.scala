package scala.lms
package common

import java.io.PrintWriter

import scala.lms.codegen.GenericCodegen
import scala.lms.internal._

//TODO: Is this used at all? should it be merge with DeliteOps?
//TODO: Rename this to something more meaningful
trait DSLOpsExp extends BaseExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).
  class DSLOp[A](val representation: Block[A]) extends Def[A]
}

trait BaseGenDSLOps extends GenericCodegen {
  val IR: DSLOpsExp
  import IR._
}

trait ScalaGenDSLOps extends ScalaGenBase with BaseGenDSLOps {
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

trait CudaGenDSLOps extends CudaGenBase with CLikeGenDSLOps
trait OpenCLGenDSLOps extends OpenCLGenBase with CLikeGenDSLOps
trait CGenDSLOps extends CGenBase with CLikeGenDSLOps
