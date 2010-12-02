package scala.virtualization.lms
package internal

import java.io.{PrintWriter,File}
import collection.immutable.HashMap


trait CudaCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cu"

  override def toString = "cuda"

  var isGPUable = false
  var parallelFor = true
  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth

  /*
  override def quote(x: Exp[_]) : String = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "null" // why is null getting lifted now? something to do with Equal
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case External(s: String, args: List[Exp[Any]]) => throw new RuntimeException("could not quote " + x)
    case null => "null"
    case _ => throw new RuntimeException("could not quote " + x)
  }
  */
  
  // HashMap for Type Conversions
  val TypeTable = HashMap[String,String](
          "ppl.dsl.optiml.Matrix[Int]" -> "Matrix<int>",
          "ppl.dsl.optiml.Matrix[Long]" -> "Matrix<long>",
          "ppl.dsl.optiml.Matrix[Float]" -> "Matrix<float>",
          "ppl.dsl.optiml.Matrix[Double]" -> "Matrix<bool>",
          "ppl.dsl.optiml.Vector[Int]" -> "Vector<int>",
          "ppl.dsl.optiml.Vector[Long]" -> "Vector<long>",
          "ppl.dsl.optiml.Vector[Float]" -> "Vector<float>",
          "ppl.dsl.optiml.Vector[Double]" -> "Vector<double>",
          "ppl.dsl.optiml.Vector[Boolean]" -> "Vector<bool>",
          "Int" -> "int",
          "Long" -> "long",
          "Float" -> "float",
          "Double" -> "double",
          "Boolean" -> "bool"
  )

  def CudaType(scalaType: String): String = TypeTable.get(scalaType) match {
    case Some(cudaType) => cudaType
    case None => throw new Exception(scalaType + " is not valid type for Cuda.")
  }

  def CudaInnerType(scalaType: String): String = {
    val start = scalaType.indexOf('[')
    val end = scalaType.lastIndexOf(']')
    if((start == -1) || (end == -1))
      throw new Exception(scalaType + " does not have inner type.")

    val innerType = scalaType.substring(start+1,end)
    TypeTable.get(innerType) match {
      case Some(cudaType) => cudaType
      case None => throw new Exception("Innertype of " + scalaType + "(" + innerType + ")" + " is not valid type for Cuda")
    }
  }
  
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    val x = fresh[A]
    val y = f(x)

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting Cuda Generated Code                  \n"+
                   "*******************************************/\n" +
                   "#include <stdio.h>\n" +
                   "#include <stdlib.h>"
    )

    //stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("int main(int argc, char** argv) {")

    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))

    //stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Cuda Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }  

  def emitConstDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }

  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+"no_type" + " " + quote(sym) + " = " + rhs + ";")
  }
  
  def emitValDef(tpe: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+tpe + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+"no_type" + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(tpe: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+tpe + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }

  /*
  def emitAssignment(tpe: String, lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+tpe + " " + lhs + " = " + rhs + ";")
  }
  */
  
  override def emitKernelHeader(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    parallelFor = true

    if((vars.length > 0)  || (resultIsVar)){
      //println("ERROR: CUDA cannot have var for the input or the result!")
      throw new RuntimeException("CudaGen: Not GPUable")
    }

    stream.println("#include <cuda.h>")
    stream.println("#include \"VectorImpl.h\"")
    stream.println("#include \"MatrixImpl.h\"")
    stream.println("")

    val paramListStr = vals.map(ele=>CudaType(ele.Type.toString) + " " + quote(ele)).mkString(", ")
    stream.println("__global__ gpuKernel_%s(%s) {".format(quote(sym), paramListStr))


  }

  override def emitKernelFooter(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    stream.println("}")

    isGPUable = false
  }

  override def exceptionHandler(filename: String, kstream:PrintWriter): Unit = {
     super.exceptionHandler(filename, kstream)
     isGPUable = false
  }

}

// TODO: do we need this for each target?
trait CudaNestedCodegen extends GenericNestedCodegen with CudaCodegen {
  val IR: Expressions with Effects
  import IR._
  
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    super.emitSource[A,B](x => reifyEffects(f(x)), className, stream)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait CudaGenBase extends CudaCodegen {
  import IR._

}

trait CudaGenEffect extends CudaNestedCodegen with CudaGenBase {

}