package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}

//TODO rename this to something more meaningful
trait DSLOpsExp extends EffectExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).
  class DSLOp[A](val representation: Exp[A]) extends Def[A]

  case class DSLMap[A,B,C[_]](val in: Exp[C[A]], out: Exp[C[B]], range: Exp[Range], func:Exp[A=>B]) extends Def[C[B]]
  case class DSLZipwith[A1,A2,B,C[_]](val in1: Exp[C[A1]], in2: Exp[C[A2]], out: Exp[C[B]], range: Exp[Range], func:Exp[(A1,A2)=>B]) extends Def[C[B]]
}

trait BaseGenDSLOps extends GenericNestedCodegen {
  val IR: DSLOpsExp
  import IR._

  // TODO: think about whether this should override syms for DSLOps or not
  override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = rhs match {
    case op: DSLOp[_] => getFreeVarBlock(op.representation,Nil)
    case _ => super.getFreeVarNode(rhs)
  }

}
trait ScalaGenDSLOps extends ScalaGenEffect with BaseGenDSLOps {
  val IR: DSLOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case op: DSLOp[_] =>
      val b = op.representation
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }

}

trait CLikeGenDSLOps extends BaseGenDSLOps with CLikeGenBase {
  val IR: DSLOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case op: DSLOp[_] => throw new GenerationFailedException("CLikeGenDSLOps: DSLOp is not supported")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDSLOps extends CudaGenEffect with CLikeGenDSLOps 
trait CGenDSLOps extends CGenEffect with CLikeGenDSLOps

