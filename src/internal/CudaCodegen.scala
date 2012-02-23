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
    helperFuncHdrStream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.h"))
    //devStream = new PrintWriter(new FileWriter(buildDir+"devFuncs.cu"))
    headerStream = new PrintWriter(new FileWriter(buildDir + "dsl.h"))
    //headerStream.println("#include \"CudaArrayList.h\"")
    //headerStream.println("#include \"helperFuncs.cu\"")
    //headerStream.println("#include \"devFuncs.cu\"")

    //TODO: Put all the DELITE APIs declarations somewhere
    hstream.print("#include \"helperFuncs.h\"\n")
    helperFuncHdrStream.print(getDSLHeaders)
    helperFuncHdrStream.print("#include <iostream>\n")
    helperFuncHdrStream.print("#include <limits>\n")
    helperFuncHdrStream.print("#include <jni.h>\n\n")
    helperFuncHdrStream.print("#include \"DeliteCuda.h\"\n")
    helperFuncHdrStream.print("typedef jboolean jbool;\n")              // TODO: Fix this
    helperFuncHdrStream.print("typedef jbooleanArray jboolArray;\n\n")  // TODO: Fix this
  }

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
    if (isPrimitiveType(sym.Type)) {
      val out = new StringBuilder
      out.append("\t%s *ptr;\n".format(remap(sym.Type)))
      out.append("\tDeliteCudaMallocHost((void**)&ptr,sizeof(%s));\n".format(remap(sym.Type)))
      out.append("\tDeliteCudaMemcpyDtoHAsync(ptr, %s, sizeof(%s));\n".format(quote(sym),remap(sym.Type)))
      out.append("\treturn *ptr;\n")
      out.toString
    }
    else throw new Exception("CudaGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    /*
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "CudaArrayList<int>" => "\t//TODO: Implement this!\n"
      case _ => throw new Exception("CudaGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    }
    */
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

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
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
    Nil
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

    out.append(getDSLHeaders)
    stream.print(out.toString)
  }

  def registerKernel(syms: List[Sym[Any]]) {
    // Print out dsl.h file
    isGPUable = true
    if(kernelsList.intersect(syms).isEmpty) {
      headerStream.println("#include \"%s.cu\"".format(syms.map(quote).mkString("")))
      kernelsList ++= syms
    }
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    if (!isGPUable) throw new GenerationFailedException("This kernel is not GPUable")

    if (external) {
      super.emitKernelFooter(syms, vals, vars, resultType, resultIsVar, external)
      //return
    }

    // aks TODO: the rest of this stuff adds to metadata and seems necessary even if we are external.
    // should probably be refactored...
    tabWidth -= 1

    // Emit input copy helper functions for object type inputs
    for(v <- vals if isObjectType(v.Type)) {
      helperFuncString.append(emitCopyInputHtoD(v, syms, copyInputHtoD(v)))
      helperFuncString.append(emitCopyMutableInputDtoH(v, syms, copyMutableInputDtoH(v)))
    }

    // Print helper functions to file stream
    hstream.print(helperFuncString)
    hstream.flush
    helperFuncHdrStream.flush
    headerStream.flush

    // Print out device function
    //devStream.println(devFuncString)
    //devStream.flush
  }

}

// TODO: do we need this for each target?
trait CudaNestedCodegen extends GenericNestedCodegen with CudaCodegen {
  val IR: Expressions with Effects
  import IR._
  
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
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
  import IR._

  def emitMultiLoopCond(sym: Sym[Any], funcs:List[Exp[Any]], idx: Sym[Int], postfix: String="", stream:PrintWriter):(String,List[Exp[Any]]) = {
    isNestedNode = true
    devFuncIdx += 1
    val currIdx = devFuncIdx
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString, true)
    val header = new StringWriter
    val footer = new StringWriter

    val currentTab = tabWidth
    tabWidth = 1
    emitFatBlock(funcs)(tempStream)
    tabWidth = currentTab

    val inputs = getFreeVarBlock(Combine(funcs),Nil).filterNot(quote(_)==quote(idx)).distinct
    val paramStr = (inputs++List(idx)).map(ele=>remap(ele.Type)+" "+quote(ele)).mkString(",")
    header.append("__device__ bool dev_%s(%s) {\n".format(postfix,paramStr))
    footer.append("\treturn %s;\n".format(funcs.map(f=>quote(getBlockResult(f))).mkString("&&")))
    footer.append("}\n")
    stream.print(header)
    stream.print(tempString)
    stream.print(footer)

    //Register Metadata for loop function
    val lf = metaData.loopFuncs.getOrElse(sym,new LoopFunc)
    lf.hasCond = true
    lf.loopCondInputs = inputs.map(quote)
    metaData.loopFuncs.put(sym,lf)
    isNestedNode = false

    ("dev_"+currIdx,inputs)
  }

}
