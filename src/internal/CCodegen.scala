package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap, ListMap, HashSet, Map => MMap}
import collection.immutable.List._


trait CCodegen extends CLikeCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cpp"
  override def toString = "c"

  var helperFuncIdx = 0
  var helperFuncString:StringBuilder = null
  var hstream: PrintWriter = null
  var headerStream: PrintWriter = null
  var kernelsList = ListBuffer[Exp[Any]]()
  
  override def hasMetaData: Boolean = true
  override def getMetaData: String = metaData.toString
  var metaData: CMetaData = null

  final class TransferFunc {
    var funcHtoD:String = _
    var argsFuncHtoD:List[Sym[Any]] = _
    var funcDtoH:String = _
    var argsFuncDtoH:List[Sym[Any]] = _
  }
  final class CMetaData {
    val inputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val outputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    //val temps: ListMap[Sym[Any],TransferFunc] = ListMap()
    //val sizeFuncs: ListMap[String,SizeFunc] = ListMap()
    //var gpuLibCall: String = ""
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")

      out.append("\"cppInputs\":["+inputs.toList.reverse.map(in=>"{\""+quote(in._1)+"\":[\""+remap(in._1.Type)+"\",\""+in._2.funcHtoD+"\",\""+in._2.funcDtoH+"\"]}").mkString(",")+"],")
      out.append("\"cppOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.Type)+"\",\""+out._2.funcDtoH+"\"]}").mkString(",")+"]")
      out.append("}")
      out.toString
    }
  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    // Set kernel input and output symbols
    //setKernelInputs(vals)
    //setKernelOutputs(syms)

      /*
    // Conditions for not generating GPU kernels (may be relaxed later)
    for (sym <- syms) {
      if((!isObjectType(sym.Type)) && (remap(sym.Type)!="void")) throw new GenerationFailedException("GPUGen: Not GPUable output type : %s".format(remap(sym.Type)))
    }
    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("GPUGen: Not GPUable input/output types: Variable")
*/
    helperFuncString.clear
    metaData = new CMetaData
  }

  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.cpp"))
    headerStream = new PrintWriter(new FileWriter(buildDir + "dsl.hpp"))
    headerStream.println("#include \"helperFuncs.cpp\"")

    /*
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
    */

    super.initializeGenerator(buildDir, args, _analysisResults)
  }

  def copyInputHtoD(sym: Sym[Any]) : String = {
    remap(sym.Type) match {
      case _ => throw new GenerationFailedException("CGen: copyInputHtoD(sym) : Cannot copy to GPU device (%s)".format(remap(sym.Type)))
    }
  }

  def copyOutputDtoH(sym: Sym[Any]) : String = {
    remap(sym.Type) match {
      case _ => throw new GenerationFailedException("CGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    }
  }

  def copyMutableInputDtoH(sym: Sym[Any]) : String = {
    remap(sym.Type) match {
      case _ => throw new GenerationFailedException("CGen: copyMutableInputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    }
  }
      
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val x = fresh[A]
    val y = reifyBlock(f(x))

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting C Generated Code                  \n"+
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
                   "  End of C Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
    Nil
  }  

/*
  //TODO: is sym of type Any or Variable[Any] ?
  def emitConstDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }
*/
  def emitVarDef(sym: Sym[Variable[Any]], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs + ";")
  }
  
  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    
    //Currently only allow single return value
    if(syms.size > 1) throw new GenerationFailedException("CLikeGen: Cannot have more than 1 results!\n");
    if(external) throw new GenerationFailedException("CLikeGen: Cannot have external libraries\n")
    
    if(resultType != "void")
      stream.println("return " + quote(syms(0)) + ";")
    stream.println("}")
    
    // Emit input copy helper functions for object type inputs
    for(v <- (vals++vars) if isObjectType(v.Type)) {
      helperFuncString.append(emitCopyInputHtoD(v, syms, copyInputHtoD(v)))
      helperFuncString.append(emitCopyMutableInputDtoH(v, syms, copyMutableInputDtoH(v)))
    }

    // Emit output copy helper functions for object type inputs
    for(v <- (syms) if isObjectType(v.Type)) {
      helperFuncString.append(emitCopyOutputDtoH(v, syms, copyOutputDtoH(v)))
    }

    // Print helper functions to file stream
    hstream.print(helperFuncString)
    hstream.flush

    // Print out dsl.h file
    if(kernelsList.intersect(syms).isEmpty) {
      headerStream.println("#include \"%s.cpp\"".format(syms.map(quote).mkString("")))
      kernelsList ++= syms
    }
    headerStream.flush

    /*
    // Print out device function
    devStream.println(devFuncString)
    devStream.flush
    */
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    if(syms.size>1) throw new GenerationFailedException("CGen: Cannot have multiple kernel outputs!\n")
    
    //if( (vars.length>0) || (resultIsVar) ) throw new GenerationFailedException("Var is not supported for CPP kernels")
    
    val kernelName = syms.map(quote).mkString("")
   
    /*
    if (resultIsVar){
      stream.print("PrimitiveRef<" + resultType + ">")
    }
    else {
      stream.print(resultType)
    }
    */
    stream.print(resultType)
     
    stream.print(" kernel_" + kernelName + "(")
    stream.print(vals.map(p=>remap(p.Type) + " " + quote(p)).mkString(", "))
    if (vals.length > 0 && vars.length > 0){
      stream.print(", ")
    }
    if (vars.length > 0){
      stream.print(vars.map(v => remap(v.Type) + " &" + quote(v)).mkString(","))
    }

    stream.println(") {")
  }
  
  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[Any]] ) {
      remap(m.typeArguments.head)
    }
    else {
      m.toString match {
        case "Int" => "int"
        case "Long" => "long"
        case "Float" => "float"
        case "Double" => "double"
        case "Boolean" => "bool"
        case "Unit" => "void"
        case _ => throw new GenerationFailedException("CGen: remap(m) : Unknown data type (%s)".format(m.toString))
      }
    }
  }

  /*******************************************************
   * Methods below are for emitting helper functions
   *******************************************************/

  def emitCopyInputHtoD(sym: Sym[Any], ksym: List[Sym[Any]], contents: String) : String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
      helperFuncIdx += 1
      out.append("%s copyInputHtoD_%s_%s_%s(%s) {\n".format(remap(sym.Type), ksym.map(quote).mkString(""), quote(sym),helperFuncIdx, "JNIEnv *env , jobject obj"))
      out.append(copyInputHtoD(sym))
      out.append("}\n")
      val tr = metaData.inputs.getOrElse(sym,new TransferFunc)
      tr.funcHtoD = "copyInputHtoD_%s_%s_%s".format(ksym.map(quote).mkString(""),quote(sym),helperFuncIdx)
      metaData.inputs.put(sym,tr)
      out.toString
    }
    else {
      val tr = metaData.inputs.getOrElse(sym,new TransferFunc)
      tr.funcHtoD = "copyInputHtoD_dummy".format(ksym.map(quote).mkString(""),quote(sym),helperFuncIdx)
      metaData.inputs.put(sym,tr)
      ""
    }
  }

  // For mutable inputs, copy the mutated datastructure from GPU to CPU after the kernel is terminated
  def emitCopyMutableInputDtoH(sym: Sym[Any], ksym: List[Sym[Any]], contents: String): String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
      helperFuncIdx += 1
      out.append("void copyMutableInputDtoH_%s_%s_%s(%s) {\n".format(ksym.map(quote).mkString(""), quote(sym), helperFuncIdx, "JNIEnv *env , jobject obj, "+remap(sym.Type)+" *"+quote(sym)+"_ptr"))
      out.append("%s %s = *(%s_ptr);\n".format(remap(sym.Type),quote(sym),quote(sym)))
      out.append(copyMutableInputDtoH(sym))
      out.append("}\n")
      val tr = metaData.inputs.getOrElse(sym,new TransferFunc)
      tr.funcDtoH = "copyMutableInputDtoH_%s_%s_%s".format(ksym.map(quote).mkString(""),quote(sym),helperFuncIdx)
      metaData.inputs.put(sym,tr)
      out.toString
    }
    else {
      val tr = metaData.inputs.getOrElse(sym,new TransferFunc)
      tr.funcDtoH = "copyMutableInputDtoH_%s_%s_%s".format(ksym.map(quote).mkString(""),quote(sym),helperFuncIdx)
      metaData.inputs.put(sym,tr)
      ""
    }
  }

  def emitCopyOutputDtoH(sym: Sym[Any], ksym: List[Sym[Any]], contents: String): String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
    	helperFuncIdx += 1
      out.append("jobject copyOutputDtoH_%s(JNIEnv *env,%s) {\n".format(helperFuncIdx,remap(sym.Type)+" *"+quote(sym)+"_ptr"))
  	  out.append("\t%s %s = *(%s_ptr);\n".format(remap(sym.Type),quote(sym),quote(sym)))
      out.append(copyOutputDtoH(sym))
      out.append("}\n")
      val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
      tr.funcDtoH = "copyOutputDtoH_%s".format(helperFuncIdx)
      metaData.outputs.put(sym,tr)
      out.toString
    }
    else {
      val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
      tr.funcDtoH = "copyOutputDtoH_%s".format(helperFuncIdx)
      metaData.outputs.put(sym,tr)
      ""
    }
  }



}

// TODO: do we need this for each target?
trait CNestedCodegen extends GenericNestedCodegen with CCodegen {
  val IR: Expressions with Effects
  import IR._
  
  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait CFatCodegen extends GenericFatCodegen with CCodegen {
  val IR: Expressions with Effects with FatExpressions
}
