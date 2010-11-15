package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenEffect, ScalaGenEffect}


trait DSLOpsExp extends EffectExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).
  class DSLOp[A](val representation: Exp[A]) extends Def[A]

  case class DSLMap[A,B,C[_]](val in: Exp[C[A]], out: Exp[C[B]], range: Exp[Range], func:Exp[A=>B]) extends Def[C[B]]
}

trait ScalaGenDSLOps extends ScalaGenEffect {
  val IR: DSLOpsExp
  import IR._
  
  // TODO: think about whether this should override syms for DSLOps or not
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case op: DSLOp[_] =>
      val b = op.representation
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }

}

trait CudaGenDSLOps extends CudaGenEffect {
  val IR: DSLOpsExp
  import IR._

  // TODO: think about whether this should override syms for DSLOps or not

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case op: DSLOp[_] =>
      val b = op.representation
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    // Currently the generator inlines the __device__ function inside the __global__ function.
    // Later it will be changed to have a separate device function.
    // TODO: Need a flag to tell the function generator to determine it.
    // TODO: How to tell the task graph generator / runtime about the output data structure generation
     case DSLMap(x,y,range,func) =>
       // Get free variables of this __global__ GPU function
       var freeVars = (buildScheduleForResult(x):::buildScheduleForResult(y):::buildScheduleForResult(range):::buildScheduleForResult(func)).filter(scope.contains(_)).map(_.sym)
       val paramList = (x.asInstanceOf[Sym[_]]::y.asInstanceOf[Sym[_]]::freeVars).distinct
       stream.println("__global__ gpuKernel_%s(%s) {".format(quote(sym),paramList.map(quote(_)).mkString(",")))
       stream.println("\tint %s = blockIdx.x*blockDim.x + threadIdx.x;".format("index"))
       stream.println("\tif(%s < %s) {".format("index", quote(x)+".length"))
       // Print the device function (inlined)
       // put parameters
       stream.println("%s %s = %s;".format("no_type", quote(func)+"_1", quote(x)+".apply(index)"))
       emitBlock(func)
       stream.println("%s.update(%s, %s);".format(quote(y),"index",quote(func)))
       stream.println("\t}")
       stream.println("}")

       //The version having separate device function
       //stream.println("\t\t%s.update(%s, %s(%s.apply(%s)));".format(quote(y),"index",quote(func),quote(x),"index"))

    case _ => super.emitNode(sym, rhs)
  }
}
