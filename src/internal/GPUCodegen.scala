package scala.virtualization.lms
package internal

import java.io.{StringWriter, PrintWriter, File}
import collection.mutable.{ArrayBuffer, HashMap, ListMap, HashSet}
import collection.immutable.List._
import scala.reflect.SourceContext

trait GPUCodegen extends CLikeCodegen with CppHostTransfer {
  val IR: Expressions
  import IR._

  /* Kernel input / output symbols */
  private var kernelInputs: List[Sym[Any]] = null
  private var kernelOutputs: List[Sym[Any]] = null
  def getKernelInputs = kernelInputs
  def setKernelInputs(syms: List[Sym[Any]]): Unit = { kernelInputs = syms }
  def getKernelOutputs = kernelOutputs
  def setKernelOutputs(syms: List[Sym[Any]]): Unit = { kernelOutputs = syms }
  def devFuncPrefix = ""

  var helperFuncIdx = 0
  val kernelsList = ArrayBuffer[Exp[Any]]()
  val helperFuncsList = ArrayBuffer[String]()

  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth

  var forceParallel = false

  var helperFuncString:StringBuilder = null
  var hstream: PrintWriter = null
  var helperFuncHdrStream: PrintWriter = null
  var devFuncIdx = 0

  var isGPUable:Boolean = false

  var processingHelperFunc: Boolean = false
  var isNestedNode: Boolean = false

  def emitMultiLoopFunc(func:Block[Any], postfix: String, lastInputs: List[Sym[Any]], stream:PrintWriter): List[String] = {
    isNestedNode = true
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString, true)
    val header = new StringWriter
    val footer = new StringWriter

    val currentTab = tabWidth
    tabWidth = 1
    withStream(tempStream) {
      emitBlock(func)
    }
    tabWidth = currentTab

    val inputs = getFreeVarBlock(func,lastInputs).distinct
    val paramStr = (inputs++lastInputs).map(ele=>remap(ele.tp)+" "+quote(ele)).mkString(",")

    header.append(devFuncPrefix + " %s dev_%s(%s) {\n".format(remap(getBlockResult(func).tp),postfix,paramStr))
    if(remap(getBlockResult(func).tp) != "void")
      footer.append("\treturn %s;\n".format(quote(getBlockResult(func))))
    footer.append("}\n")
    stream.print(header)
    stream.print(tempString)
    stream.print(footer)

    /*
    if(loopType=="FOREACH") {
      val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
      tr.funcHtoD = ""
      tr.argsFuncHtoD = List()
      tr.funcDtoH = ""
      tr.argsFuncDtoH = List()
      metaData.outputs.put(sym,tr)
    }
    */
    isNestedNode = false
    inputs.map(quote(_))
  }


  // MetaData
  override def hasMetaData: Boolean = true
  override def getMetaData: String = metaData.toString
  var metaData: GPUMetaData = null

  final class LoopFunc {
    var tpe: String = ""
    var hasCond: Boolean = false
    var loopFuncInputs: List[String] = Nil
    var loopFuncInputs_2: List[String] = Nil
    var loopFuncOutputType: String = ""
    var loopFuncOutputType_2: String = ""
    var loopCondInputs: List[String] = Nil
    var loopReduceInputs: List[String] = Nil
    var loopReduceInputs_2: List[String] = Nil
    var loopReduceParInputs: List[String] = Nil
    var loopReduceParInputs_2: List[String] = Nil
    var loopZeroInputs: List[String] = Nil
    var loopZeroInputs_2: List[String] = Nil

    override def toString: String = {
      "\"" + tpe + "\"," + hasCond + ",[" + loopFuncInputs.map(i => "\""+ i +"\"").mkString(",") + "],[" + loopFuncInputs_2.map(i => "\""+ i +"\"").mkString(",") + "]" +
      ",\"" + loopFuncOutputType + "\",\"" + loopFuncOutputType_2 + "\",[" + loopCondInputs.map(i => "\""+ i +"\"").mkString(",") + "],[" + loopReduceInputs.map(i => "\""+ i +"\"").mkString(",") + "],[" + loopReduceInputs_2.map(i => "\""+ i +"\"").mkString(",") + "]" +
      ",[" + loopReduceParInputs.map(i => "\""+ i +"\"").mkString(",") + "],[" + loopReduceParInputs_2.map(i => "\""+ i +"\"").mkString(",") + "]" +
      ",[" + loopZeroInputs.map(i => "\""+ i +"\"").mkString(",") + "],[" + loopZeroInputs_2.map(i => "\""+ i +"\"").mkString(",") + "]"
    }
  }
  final class TransferFunc {
    var funcHtoD:String = ""
    var argsFuncHtoD:List[Sym[Any]] = Nil
    var funcDtoH:String = ""
    var argsFuncDtoH:List[Sym[Any]] = Nil
    //override def toString: String = {  }
  }

  final class GPUMetaData {
    val inputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val outputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    var gpuLibCall: String = ""
    var loopFuncs: ListMap[Sym[Any],LoopFunc] = ListMap()
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")

      //if (kernelFileExt == "cu") {
        out.append("\"gpuInputs\":["+getKernelInputs.filter(in => !isPrimitiveType(in.tp)).map(in=>"{\""+quote(in)+"\":[\""+remap(in.tp)+"\"]}").mkString(",")+"],")
        out.append("\"gpuOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.tp)+"\",["+ out._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"],"+loopFuncs.getOrElse(out._1,new LoopFunc).toString+"]}").mkString(",")+"]")
      //}
      //else { //opencl
      //  out.append("\"gpuInputs\":["+getKernelInputs.filter(in=>isObjectType(in.Type)).map(in=>"{\""+quote(in)+"\":[\""+remap(in.Type)+"\",{"+unpackObject(in).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(",")+"}]}").mkString(",")+"],")
      //  out.append("\"gpuOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.Type)+"\",["+ out._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"],{"+unpackObject(out._1).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(",")+"}]}").mkString(",")+"],")
      //}

      out.append("}")
      out.toString
    }
  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {

    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("GPUGen: Not GPUable input/output types: Variable")

    // Set kernel input and output symbols
    setKernelInputs(vals)
    setKernelOutputs(syms)

    helperFuncString.clear
    metaData = new GPUMetaData
    tabWidth = 1
    isGPUable = false
    processingHelperFunc = false
    isNestedNode = false
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
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

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    if (!isGPUable) throw new GenerationFailedException("This kernel is not GPUable")

    if (external) {
      super.emitKernelFooter(syms, vals, vars, resultType, resultIsVar, external)
    }

    // aks TODO: the rest of this stuff adds to metadata and seems necessary even if we are external.
    // should probably be refactored...
    tabWidth -= 1

    // Emit input copy helper functions for object type inputs
    for(v <- vals) {
      helperFuncString.append(emitRecv(v,Hosts.JVM))
      helperFuncString.append(emitUpdate(v,Hosts.JVM))
      //helperFuncString.append(emitCopyInputHtoD(v, syms, copyInputHtoD(v)))
      //helperFuncString.append(emitCopyMutableInputDtoH(v, syms, copyMutableInputDtoH(v)))
    }

    // Emit output copy helper functions for object type inputs
    for(v <- (syms) if !isVoidType(v.tp)) {
      helperFuncString.append(emitSend(v,Hosts.JVM))
      //helperFuncString.append(emitCopyOutputDtoH(v, syms, copyMutableInputDtoH(v)))
    }

    // Print helper functions to file stream
    hstream.print(helperFuncString)
    hstream.flush
    helperFuncHdrStream.flush
  }

  def registerKernel(syms: List[Sym[Any]]) {
    isGPUable = true
    if(kernelsList.intersect(syms).isEmpty) {
      //headerStream.println("#include \"%s.%s\"".format(syms.map(quote).mkString(""),kernelFileExt))
      kernelsList ++= syms
    }
  }

  /*******************************************************
   * Methods below are for emitting helper functions
   *******************************************************/
  // For object type inputs, allocate GPU memory and copy from CPU to GPU.
  /*
  def emitCopyInputHtoD(sym: Sym[Any], ksym: List[Sym[Any]], contents: String) : String = {
    val out = new StringBuilder
    val funcName = "copyInputHtoD_%s_%s".format(ksym.map(quote).mkString(""),quote(sym))
    if(!isPrimitiveType(sym.tp) && !helperFuncsList.contains(funcName)) {
      helperFuncsList += funcName
      out.append("%s *%s(%s *%s)".format(remap(sym.tp),funcName))
      helperFuncHdrStream.append(out.toString + ";\n")
      out.append("{\n")
      out.append(contents)
      out.append("}\n")
      out.toString
    }
    else ""
  }

  // For mutable inputs, copy the mutated datastructure from GPU to CPU after the kernel is terminated
  def emitCopyMutableInputDtoH(sym: Sym[Any], ksym: List[Sym[Any]], contents: String): String = {
    val out = new StringBuilder
    val funcName = "copyMutableInputDtoH_%s_%s".format(ksym.map(quote).mkString(""),quote(sym))
    if(!isPrimitiveType(sym.tp) && !helperFuncsList.contains(funcName)) {
      helperFuncsList += funcName
      out.append("void %s(JNIEnv *env, jobject obj, %s *%s_ptr)".format(funcName,remap(sym.tp),quote(sym)))
      helperFuncHdrStream.append(out.toString + ";\n")
      out.append("{\n")
      out.append("%s %s = *(%s_ptr);\n".format(remap(sym.tp),quote(sym),quote(sym)))
      out.append(contents)
      out.append("}\n")
      out.toString
    }
    else ""
  }


  def emitCopyOutputDtoH(sym: Sym[Any], ksym: List[Sym[Any]], contents: String): String = {
    val out = new StringBuilder
    val funcName = "copyOutputDtoH_%s".format(quote(sym))
    if(helperFuncsList contains funcName) return ""
    helperFuncsList += funcName

    if(!isPrimitiveType(sym.tp)) {
      out.append("jobject %s(JNIEnv *env,%s)".format(funcName,remap(sym.tp)+" *"+quote(sym)+"_ptr"))
      helperFuncHdrStream.append(out.toString + ";\n")
      out.append("{\n")
  	  out.append("\t%s %s = *(%s_ptr);\n".format(remap(sym.tp),quote(sym),quote(sym)))
      out.append(contents)
      out.append("}\n")
      out.toString
    }
    else {
      out.append("%s %s(JNIEnv *env,%s)".format(remap(sym.tp),funcName,remap(sym.tp)+" *"+quote(sym)))
      helperFuncHdrStream.append(out.toString + ";\n")
      out.append("{\n")
      out.append(contents)
      out.append("}\n")
      out.toString
    }
  }
  */

  def emitAllocOutput(sym: Sym[Any], ksym: List[Sym[Any]], contents: String, args: List[Sym[Any]], aV: Sym[Any]): String = {
    val out = new StringBuilder
    val funcName = "allocFunc_%s".format(quote(sym))
    if(helperFuncsList contains funcName) return ""
    helperFuncsList += funcName

    if(!isPrimitiveType(sym.tp)) {
      val paramStr = args.map(ele =>
        if(isPrimitiveType(ele.tp)) remap(ele.tp) + " " + quote(ele)
        else remap(ele.tp) + " *" + quote(ele) + "_ptr"
      ).mkString(",")
      val derefParams = args.map(ele=>
         if(isPrimitiveType(ele.tp)) ""
         else "\t%s %s = *(%s_ptr);\n".format(remap(ele.tp),quote(ele),quote(ele))
       ).mkString("")
      out.append("%s *%s(%s %s)".format(remap(sym.tp), funcName, paramStr, if(args.nonEmpty) ",int size" else "int size"))
      helperFuncHdrStream.append(out.toString + ";\n")
      out.append("{\n")
      out.append(derefParams)
      //out.append("\t%s *%s_ptr = new %s(size);\n".format(remap(aV.tp),quote(aV),remap(aV.tp)))
      //out.append("\t%s %s = *%s_ptr;\n".format(remap(aV.tp),quote(aV),quote(aV)))
      out.append(contents)
      out.append("}\n")
      out.toString
    }
    else {
      out.append("%s *%s(void)".format(remap(sym.tp),funcName))
      helperFuncHdrStream.append(out.toString + ";\n")
      out.append("{\n")
      out.append(contents)
      out.append("}\n")
      out.toString
    }
  }


  /* emitAllocFunc method emits code for allocating the output memory of a kernel,
       and copying  it to CPU memory with allocation of new object in CPU */
  //TODO: Separate output and temporary allocations
  def emitAllocFunc(sym:Sym[Any], allocFunc:Block[Any], aV:Sym[Any]=null, size:Exp[Any]=null) {
    processingHelperFunc = true
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString,true)

    // Get free variables (exclude the arrayVariable)
    val inputs = if(allocFunc==null) Nil
    else getFreeVarBlock(allocFunc,List(aV))

    // Register metadata
    val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
    tr.argsFuncHtoD = inputs
    metaData.outputs.put(sym,tr)

    // Get the body (string) of the allocation function in tempString
    if(allocFunc!=null) {
      withStream(tempStream) {
        emitBlock(allocFunc)
      }
      tempString.append("\treturn %s_ptr;\n".format(quote(getBlockResult(allocFunc))))
	  }
    else {
      tempString.append("\treturn %s_ptr;\n".format(quote(sym)))
    }

    // Emit the full allocation function
    val allocOutputStr = emitAllocOutput(sym, null, tempString.toString, inputs, aV)

    // Write to helper function string
    helperFuncString.append(allocOutputStr)

    processingHelperFunc = false
  }

  def emitAllocFuncPrimitive(sym:Sym[Any]) {
    processingHelperFunc = true
    assert(isPrimitiveType(sym.tp))

    val tempString = new StringWriter

    // Register metadata
    val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
    tr.argsFuncHtoD = Nil
    metaData.outputs.put(sym,tr)

    tempString.append("\t%s *ptr;\n".format(remap(sym.tp)))
    tempString.append("\tDeliteCudaMalloc((void**)&ptr, sizeof(%s));\n".format(remap(sym.tp)))
    tempString.append("\treturn ptr;\n")

    // Emit the full allocation function
    val allocOutputStr = emitAllocOutput(sym, null, tempString.toString, null, null)

    // Write to helper function string
    helperFuncString.append(allocOutputStr)

    processingHelperFunc = false
  }

  def checkGPUAlloc(sym: Sym[Any]) {
    if(!processingHelperFunc) {
      if (isNestedNode) {
        printDebug(sym, "Code has nested memory allocations (not supported by current Delite GPU code generator). Try manually unrolling the outer loop.")
        throw new GenerationFailedException("CudaGen: Nested allocations not allowed\n")
      }
      else {
        throw new GenerationFailedException("CudaGen: Allocations cannot be done within a GPU kernel.\n")
      }
    }
  }

  def printDebug(sym: Sym[Any], msg: String) {
    def getFirstStack(cs: SourceContext): SourceContext = cs.parent match {
      case None => cs
      case Some(p) => getFirstStack(p)
    }
    print("\n** GPU Warning ")
    sym.pos match {
      case Nil => println("[unknown file] **")
      case cs => println("[" + cs.map(e=>getFirstStack(e).fileName.split(File.separator).last+":"+getFirstStack(e).line).mkString(", ") + "] **")
    }
    println(msg)
    println("Stack Trace: " + quotePos(sym))
  }
}
