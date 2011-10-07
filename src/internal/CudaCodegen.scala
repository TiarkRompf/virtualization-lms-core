package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap, ListMap, HashSet}
import collection.immutable.List._


trait CudaCodegen extends GPUCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cu"
  override def toString = "cuda"

  override def initializeGenerator(buildDir:String): Unit = {

    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.cu"))
    devStream = new PrintWriter(new FileWriter(buildDir+"devFuncs.cu"))
    headerStream = new PrintWriter(new FileWriter(buildDir + "dsl.h"))
    headerStream.println("#include \"CudaArrayList.h\"")
    headerStream.println("#include \"helperFuncs.cu\"")
    headerStream.println("#include \"devFuncs.cu\"")

    //TODO: Put all the DELITE APIs declarations somewhere
    hstream.print(getDSLHeaders)
    hstream.print("#include <iostream>\n")
    hstream.print("#include <limits>\n")
    hstream.print("#include <jni.h>\n\n")
    hstream.print("//Delite Runtime APIs\n")
    hstream.print("extern void DeliteCudaMallocHost(void **ptr, size_t size);\n")
    hstream.print("extern void DeliteCudaMalloc(void **ptr, size_t size);\n")
    hstream.print("extern void DeliteCudaMemcpyHtoDAsync(void *dptr, void *sptr, size_t size);\n")
    hstream.print("extern void DeliteCudaMemcpyDtoHAsync(void *dptr, void *sptr, size_t size);\n")
    hstream.print("typedef jboolean jbool;\n")              // TODO: Fix this
    hstream.print("typedef jbooleanArray jboolArray;\n\n")  // TODO: Fix this
  }

  /*
  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    // Set kernel input and output symbols
    setKernelInputs(vals)
    setKernelOutputs(syms)

    // Conditions for not generating CUDA kernels (may be relaxed later)
    for (sym <- syms) {
      if((!isObjectType(sym.Type)) && (remap(sym.Type)!="void")) throw new GenerationFailedException("CudaGen: Not GPUable output type : %s".format(remap(sym.Type)))
    }
    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("CudaGen: Not GPUable input/output types: Variable")

    // Initialize global variables
    //useLocalVar = false
    //cudaVarMap.clear
    optimizer = new CudaOptimizer

    currDim = 0
    xDimList.clear
    yDimList.clear
    multDimInputs.clear

    helperFuncString.clear
    metaData = new CudaMetaData
    //MetaData.init
    tabWidth = 1
    devFuncString = new StringBuilder

    forceParallel = false
  }
  */

  /****************************************
   *  Methods for managing GPUable Types
   *  **************************************/
  /*
  // Map a scala primitive type to JNI type descriptor
  def JNITypeDescriptor[A](m: Manifest[A]) : String = m.toString match {
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case "Boolean" => "Z"
    case _ => throw new GenerationFailedException("Undefined CUDA type")
  }

  def isObjectType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "scala.collection.immutable.List[Int]" => true
      case _ => false
    }
  }

  def isPrimitiveType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Int" | "Long" | "Float" | "Double" | "Boolean"  => true
      case _ => false
    }
  }

  def isVoidType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Unit" => true
      case _ => false
    }
  }

  def isVariableType[A](m: Manifest[A]) : Boolean = {
    if(m.erasure == classOf[Variable[AnyVal]]) true
    else false
  }

  // Check the type and generate Exception if the type is not GPUable
  def checkGPUableType[A](m: Manifest[A]) : Unit = {
    if(!isGPUableType(m))
      throw new GenerationFailedException("CudaGen: Type %s is not a GPUable Type.".format(m.toString))
  }

  // All the types supported by CUDA Generation
  def isGPUableType[A](m : Manifest[A]) : Boolean = {
    if(!isObjectType(m) && !isPrimitiveType(m) && !isVoidType(m) && !isVariableType(m))
      false
    else
      true
  }
  */

  override def remap[A](m: Manifest[A]) : String = {
    checkGPUableType(m)
    if (m.erasure == classOf[Variable[AnyVal]])
      remap(m.typeArguments.head)
    else {
      m.toString match {
          case "Int" => "int"
          case "Long" => "long"
          case "Float" => "float"
          case "Double" => "double"
          case "Boolean" => "bool"
          case "Unit" => "void"
          case "scala.collection.immutable.List[Int]" => "CudaArrayList<int>"  //TODO: Use C++ list
          case _ => throw new Exception("CudaGen: remap(m) : GPUable Type %s does not have mapping table.".format(m.toString))
      }
    }
  }

  // TODO: Handle general C datastructure
  def copyInputHtoD(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "CudaArrayList<int>" => {
        val out = new StringBuilder
        out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.toString
      }
      case _ => throw new Exception("CudaGen: copyInputHtoD(sym) : Cannot copy to GPU device (%s)".format(remap(sym.Type)))
    }
  }

  def copyOutputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "CudaArrayList<int>" => "\t//TODO: Implement this!\n"
      case _ => throw new Exception("CudaGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    }
  }

  def copyMutableInputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "CudaArrayList<int>" => "\t//TODO: Implement this!\n"
      case _ => throw new Exception("CudaGen: copyMutableInputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    }
  }

  //TODO: Remove below methods
  def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = {
    throw new GenerationFailedException("CudaGen: allocOutput(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.Type)))
  }
  def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = {
    throw new GenerationFailedException("CudaGen: allocReference(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.Type)))
  }

  def positionMultDimInputs(sym: Sym[Any]) : String = {
    throw new GenerationFailedException("CudaGen: positionMultDimInputs(sym) : Cannot reposition GPU memory (%s)".format(remap(sym.Type)))
  }

  def cloneObject(sym: Sym[Any], src: Sym[Any]) : String = {
    throw new GenerationFailedException("CudaGen: cloneObject(sym)")
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

    stream.println("int main(int argc, char** argv) {")

    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))

    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Cuda Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }  

/*
  //TODO: is sym of type Any or Variable[Any] ?
  def emitConstDef(sym: Sym[Any], rhs: emitK)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }
*/

  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+ remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }
  
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    if (external) {
      // CUDA library ops use a C wrapper, so should be generated as a C kernel
      stream.println(getDSLHeaders)
      super.emitKernelHeader(syms, getKernelOutputs ::: vals, vars, resultType, resultIsVar, external)
      return
    }

    val out = new StringBuilder

    out.append("#include <cuda.h>\n\n")
    out.append(getDSLHeaders)

    val paramStr = (getKernelOutputs++getKernelInputs++getKernelTemps).filterNot(e=>isVoidType(e.Type)).map(e=>remap(e.Type) + " " + quote(e)).mkString(", ")

    out.append("__global__ void kernel_%s(%s) {\n".format(syms.map(quote(_)).mkString(""), paramStr))
    out.append(addTab()+"int idxX = blockIdx.x*blockDim.x + threadIdx.x;\n")
    out.append(addTab()+"int idxY = blockIdx.y*blockDim.y + threadIdx.y;\n")
    for(in <- multDimInputs) {
      out.append(addTab()+positionMultDimInputs(in))
    }
    stream.print(out.toString)
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    if (external) {
      super.emitKernelFooter(syms, vals, vars, resultType, resultIsVar, external)
      //return
    }
    else {
      stream.println("}")
    }

    // aks TODO: the rest of this stuff adds to metadata and seems necessary even if we are external.
    // should probably be refactored...
    tabWidth -= 1

    // Emit input copy helper functions for object type inputs
    for(v <- vals if isObjectType(v.Type)) {
      helperFuncString.append(emitCopyInputHtoD(v, syms, copyInputHtoD(v)))
      helperFuncString.append(emitCopyMutableInputDtoH(v, syms, copyMutableInputDtoH(v)))
    }

    // Emit kernel size calculation helper functions
    if (!external) {
      helperFuncString.append(emitSizeFuncs(syms,external))
    }

    // Print helper functions to file stream
    hstream.print(helperFuncString)
    hstream.flush

    // Print out dsl.h file
    if(kernelsList.intersect(syms).isEmpty) {
      headerStream.println("#include \"%s.cu\"".format(syms.map(quote).mkString("")))
      kernelsList ++= syms
    }
    headerStream.flush

    // Print out device function
    devStream.println(devFuncString)
    devStream.flush
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


  def CudaConsts(x:Exp[Any], s:String): String = {
    s match {
      case "Infinity" => "std::numeric_limits<%s>::max()".format(remap(x.Type))
      case _ => s
    }
  }
  
  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "NULL"
    case Const(z) => CudaConsts(x, z.toString)
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait CudaFatCodegen extends GenericFatCodegen with CudaCodegen {
  val IR: Expressions with Effects with FatExpressions
}
