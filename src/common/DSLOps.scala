package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenEffect, ScalaGenEffect}
import collection.mutable.HashSet

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

     case DSLMap(x,y,range,func) =>
      // Get function argument list
      val argsSet = HashSet[Sym[_]]()
      //getUsedIdents(func,argsSet)
      // Print function
      emitBlock(func)
      // Print range
      //emitBlock(range)(prtWriter)
      //val length = strWriter.toString.split("<.+;")(1)
      argsSet.add(findDefinition(x.asInstanceOf[Sym[_]]).get.sym)
      argsSet.add(findDefinition(y.asInstanceOf[Sym[_]]).get.sym)
      //var list = argsSet.toList ++ findDefinition(x).get +
      var str = ""
      stream.println("__global__ gpuKernel_%s(%s) {".format(quote(sym),argsSet.toList.map(quote(_)).mkString(",")))
      stream.println("\tint %s = blockIdx.x*blockDim.x + threadIdx.x;".format("index"))
      stream.println("\tif(%s < %s) {".format("index", "length"))
      stream.println("\t\t%s.get(%s) = %s(%s.get(%s));".format(quote(x),"index",quote(func),quote(y),"index"))
      stream.println("\t}")
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
