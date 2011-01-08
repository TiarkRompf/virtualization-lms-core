package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen

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
  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case op: DSLOp[_] => getFreeVarBlock(op.representation,Nil)
    case _ => super.getFreeVarNode(rhs)
  }

}
trait ScalaGenDSLOps extends ScalaGenEffect with BaseGenDSLOps {
  val IR: DSLOpsExp
  import IR._
  
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

trait CudaGenDSLOps extends CudaGenEffect with BaseGenDSLOps {
  val IR: DSLOpsExp
  import IR._

  // TODO: think about whether this should override syms for DSLOps or not

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case op: DSLOp[_] =>
          val b = op.representation
          //stream.println("val " + quote(sym) + " = { ")
          emitBlock(b)
          //stream.println(quote(getBlockResult(b)))
          //stream.println("}")
          //isGPUable = false

        // Currently the generator inlines the __device__ function inside the __global__ function.
        // Later it will be changed to have a separate device function.
        // TODO: Need a flag to tell the function generator to determine it.
        // TODO: How to tell the task graph generator / runtime about the output data structure generation
         case op@DSLMap(x,y,range,func) =>
           tabWidth = 0
           // Get free variables of this __global__ GPU function
           val freeVars = getFreeVarBlock(func,Nil)
           //var freeVars = (buildScheduleForResult(x):::buildScheduleForResult(y):::buildScheduleForResult(range):::buildScheduleForResult(func)).filter(scope.contains(_)).map(_.sym)
           val paramList = (x.asInstanceOf[Sym[_]]::y.asInstanceOf[Sym[_]]::freeVars).distinct
           val paramListStr = paramList.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
           stream.println("__global__ gpuKernel_%s(%s) {".format(quote(sym),paramListStr))
           tabWidth += 1
           stream.println(addTab()+"int %s = blockIdx.x*blockDim.x + threadIdx.x;".format("index"))
           stream.println(addTab()+"if(%s < %s) {".format("index", quote(x)+".length"))
           // Print the device function (inlined)
           // put parameters
           tabWidth += 1
           stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)), quote(func)+"_1", quote(x)+".apply(index)"))
           emitBlock(func)
           stream.println(addTab()+"%s.update(%s, %s);".format(quote(y),"index",quote(func)))
           tabWidth -= 1
           stream.println("\t}")
           tabWidth -= 1
           stream.println("}")

           //The version having separate device function
           //stream.println("\t\t%s.update(%s, %s(%s.apply(%s)));".format(quote(y),"index",quote(func),quote(x),"index"))

        case op@DSLZipwith(x1,x2,y,range,func) =>
           tabWidth = 0
           // Get free variables of this __global__ GPU function
           val freeVars = getFreeVarBlock(func,Nil)
           //var freeVars = (buildScheduleForResult(x1):::buildScheduleForResult(x2):::buildScheduleForResult(y):::buildScheduleForResult(range):::buildScheduleForResult(func)).filter(scope.contains(_)).map(_.sym)
           val paramList = (x1.asInstanceOf[Sym[_]]::x2.asInstanceOf[Sym[_]]::y.asInstanceOf[Sym[_]]::freeVars).distinct
           val paramListStr = paramList.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
           stream.println("__global__ gpuKernel_%s(%s) {".format(quote(sym),paramListStr))
           tabWidth += 1
           stream.println(addTab()+"int %s = blockIdx.x*blockDim.x + threadIdx.x;".format("index"))
           stream.println(addTab()+"if(%s < %s) {".format("index", quote(x1)+".length"))
           // Print the device function (inlined)
           // put parameters
           tabWidth += 1
           stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)), quote(func)+"_1", quote(x1)+".apply(index)"))
           stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)), quote(func)+"_2", quote(x1)+".apply(index)"))
           emitBlock(func)
           stream.println(addTab()+"%s.update(%s, %s);".format(quote(y),"index",quote(func)))
           tabWidth -= 1
           stream.println(addTab()+"}")
           tabWidth -= 1
           stream.println(addTab()+"}")

        case _ => super.emitNode(sym, rhs)
      }
    }
}
