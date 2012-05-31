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
  override def devFuncPrefix = "__device__"

  override def initializeGenerator(buildDir:String): Unit = {

    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.cu"))
    helperFuncHdrStream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.h"))
    //headerStream = new PrintWriter(new FileWriter(buildDir + "dsl.h"))

    //TODO: Put all the DELITE APIs declarations somewhere
    hstream.print("#include \"helperFuncs.h\"\n")
    helperFuncHdrStream.print(getDSLHeaders)
    helperFuncHdrStream.print("#include <iostream>\n")
    helperFuncHdrStream.print("#include <limits>\n")
    helperFuncHdrStream.print("#include <jni.h>\n\n")
    helperFuncHdrStream.print("#define CHAR short\n")
    helperFuncHdrStream.print("#define jCHAR jshort\n")
    helperFuncHdrStream.print("#include \"DeliteCuda.h\"\n")
    helperFuncHdrStream.print("#include \"DeliteArray.h\"\n")
  }

  // TODO: Move to Delite?
  def copyInputHtoD(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
        val out = new StringBuilder
        val typeArg = sym.Type.typeArguments.head
        val numBytesStr = "length * sizeof(%s)".format(remap(typeArg))
        out.append("\tint length = env->GetArrayLength((j%sArray)obj);\n".format(remapToJNI(typeArg).toLowerCase))
        out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
        out.append("\t%s *%s = new %s(length);\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))
        out.append("\tmemcpy(hostPtr, dataPtr, %s);\n".format(numBytesStr))
        out.append("\tDeliteCudaMemcpyHtoDAsync(%s->data, hostPtr, %s);\n".format(quote(sym),numBytesStr))
        out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.toString
      case _ => throw new Exception("CudaGen: copyInputHtoD(sym) : Cannot copy to GPU device (%s)".format(remap(sym.Type)))
    }
  }

  def copyOutputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    if (isPrimitiveType(sym.Type)) {
      val out = new StringBuilder
      out.append("\t%s *ptr;\n".format(remap(sym.Type)))
      out.append("\tDeliteCudaMallocHost((void**)&ptr,sizeof(%s));\n".format(remap(sym.Type)))
      out.append("\tDeliteCudaMemcpyDtoHAsync(ptr, %s, sizeof(%s));\n".format(quote(sym),remap(sym.Type)))
      out.append("\treturn *ptr;\n")
      out.toString
    }
    else {
      remap(sym.Type) match {
        case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
          val out = new StringBuilder
          val typeArg = sym.Type.typeArguments.head
          val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(typeArg))
          out.append("\tj%sArray arr = env->New%sArray(%s.length);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg),quote(sym)))
          out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)arr,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
          out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
          out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))
          out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
          out.append("\tmemcpy(dataPtr, hostPtr, %s);\n".format(numBytesStr))
          out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)arr, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
          out.append("\treturn arr;\n")
          out.toString
        case _ => throw new Exception("CudaGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
      }
    }
  }

  def copyMutableInputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "DeliteArray<bool>" | "DeliteArray<char>" | "DeliteArray<CHAR>" | "DeliteArray<short>" | "DeliteArray<int>" | "DeiteArray<long>" | "DeliteArray<float>" | "DeliteArray<double>" =>
        val out = new StringBuilder
        val typeArg = sym.Type.typeArguments.head
        val numBytesStr = "length * sizeof(%s)".format(remap(typeArg))
        out.append("\tint length = %s.length;\n".format(quote(sym)))
        out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
        out.append("\t%s *hostPtr;\n".format(remap(typeArg)))
        out.append("\tDeliteCudaMallocHost((void**)&hostPtr,%s);\n".format(numBytesStr))
        out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
        out.append("\tmemcpy(dataPtr, hostPtr, %s);\n".format(numBytesStr))
        out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
        out.toString
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
    val y = reifyBlock(f(x))

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

}

// TODO: do we need this for each target?
trait CudaNestedCodegen extends CLikeNestedCodegen with CudaCodegen {
  val IR: Expressions with Effects
  import IR._

  def CudaConsts(x:Exp[Any], s:String): String = {
    s match {
      case "Infinity" => "std::numeric_limits<%s>::max()".format(remap(x.Type))
      case _ => s
    }
  }
  
  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Const(s: String) => "\""+s+"\""
    case Const(s: Char) => "'"+s+"'"
    case Const(null) => "NULL"
    case Const(z) => CudaConsts(x, z.toString)
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait CudaFatCodegen extends CLikeFatCodegen with CudaCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._

  def emitMultiLoopCond(sym: Sym[Any], funcs:List[Block[Any]], idx: Sym[Int], postfix: String="", stream:PrintWriter):(String,List[Exp[Any]]) = {
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

    val inputs = getFreeVarBlock(Block(Combine(funcs.map(getBlockResultFull))),Nil).filterNot(quote(_)==quote(idx)).distinct
    val paramStr = (inputs++List(idx)).map(ele=>remap(ele.Type)+" "+quote(ele)).mkString(",")
    header.append(devFuncPrefix + " bool dev_%s(%s) {\n".format(postfix,paramStr))
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
