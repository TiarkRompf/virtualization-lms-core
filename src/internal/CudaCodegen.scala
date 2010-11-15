package scala.virtualization.lms
package internal

import java.io.PrintWriter
import collection.immutable.HashMap


trait CudaCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._


  var parallelFor = true
  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth
  
  // HashMap for Type Conversions
  val TypeTable = HashMap[String,String](
          "ppl.dsl.optiml.Matrix[Int]" -> "Matrix<Int>",
          "ppl.dsl.optiml.Matrix[Long]" -> "Matrix<Long>",
          "ppl.dsl.optiml.Matrix[Float]" -> "Matrix<Float>",
          "ppl.dsl.optiml.Matrix[Double]" -> "Matrix<Double>",
          "ppl.dsl.optiml.Vector[Boolean]" -> "Vector<Bool>",
          "ppl.dsl.optiml.Vector[Int]" -> "Vector<Int>",
          "ppl.dsl.optiml.Vector[Long]" -> "Vector<Long>",
          "ppl.dsl.optiml.Vector[Float]" -> "Vector<Float>",
          "ppl.dsl.optiml.Vector[Double]" -> "Vector<Double>",
          "ppl.dsl.optiml.Vector[Boolean]" -> "Vector<Bool>",
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