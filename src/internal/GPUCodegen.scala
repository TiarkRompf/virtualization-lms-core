package scala.virtualization.lms
package internal

import java.io.{StringWriter, PrintWriter, File}
import collection.immutable.List._
import scala.reflect.SourceContext
import collection.mutable.{HashSet, ArrayBuffer, ListMap}

trait GPUCodegen extends CLikeCodegen with AbstractHostTransfer with AbstractDeviceTransfer {
  val IR: Expressions
  import IR._

  // Prefix string for device functions on the target GPU programming model (e.g., __device__ for CUDA)
  def devFuncPrefix:String = ""

  // List of kernels and helper functions emitted so far (not to emit the same thing multiple times)
  private val kernelsList = ArrayBuffer[Exp[Any]]()
  private val helperFuncList = ArrayBuffer[String]()

  // Current tab location for pretty printing
  protected var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth

  // Streams for helper functions and its header
  protected var helperFuncStream: PrintWriter = _
  protected var headerStream: PrintWriter = _

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

  final class GPUMetaData(val kernelInputs: List[Sym[Any]]) {
    val inputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val outputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    var gpuLibCall: String = ""
    var loopFuncs: ListMap[Sym[Any],LoopFunc] = ListMap()
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")

      //if (kernelFileExt == "cu") {
        out.append("\"gpuInputs\":["+kernelInputs.filter(in => !isPrimitiveType(in.tp)).map(in=>"{\""+quote(in)+"\":[\""+remap(in.tp)+"\"]}").mkString(",")+"],")
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
    //kernelInputs = vals
    //kernelOutputs = syms

    //helperFuncString.clear
    metaData = new GPUMetaData(vals)
    tabWidth = 1
    isGPUable = false
    processingHelperFunc = false
    isNestedNode = false
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    if (external) {
      // CUDA library ops use a C wrapper, so should be generated as a C kernel
      //stream.println(getDSLHeaders)
      super.emitKernelHeader(syms, syms ::: vals, vars, resultType, resultIsVar, external)
      return
    }
    val out = new StringBuilder
    //out.append(getDSLHeaders)
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

    dsTypesList ++= (syms++vals++vars).map(_.tp)

    // Print helper functions to file stream
    helperFuncStream.flush
    headerStream.flush
  }

  override def emitTransferFunctions() {

    
    for (tp <- dsTypesList) {
      // Emit input copy helper functions for object type inputs
      //TODO: For now just iterate over all possible hosts, but later we can pick one depending on the input target
      try {
        val (recvHeader, recvSource) = emitRecv(tp, Hosts.JVM)
        if (!helperFuncList.contains(recvHeader)) {
          headerStream.println(recvHeader)
          helperFuncStream.println(recvSource)
          helperFuncList.append(recvHeader)
        }
        val (recvViewHeader, recvViewSource) = emitRecvView(tp, Hosts.JVM)
        if (!helperFuncList.contains(recvViewHeader)) {
          headerStream.println(recvViewHeader)
          helperFuncStream.println(recvViewSource)
          helperFuncList.append(recvViewHeader)
        }
        val (sendUpdateHeader, sendUpdateSource) = emitSendUpdate(tp, Hosts.JVM)
        if (!helperFuncList.contains(sendUpdateHeader)) {
          headerStream.println(sendUpdateHeader)
          helperFuncStream.println(sendUpdateSource)
          helperFuncList.append(sendUpdateHeader)
        }
        val (recvUpdateHeader, recvUpdateSource) = emitRecvUpdate(tp, Hosts.JVM)
        if (!helperFuncList.contains(recvUpdateHeader)) {
          headerStream.println(recvUpdateHeader)
          helperFuncStream.println(recvUpdateSource)
          helperFuncList.append(recvUpdateHeader)
        }
        val (sendSlaveHeader, sendSlaveSource) = emitSendSlave(tp)
        if (!helperFuncList.contains(sendSlaveHeader)) {
          headerStream.println(sendSlaveHeader)
          helperFuncStream.println(sendSlaveSource)
          helperFuncList.append(sendSlaveHeader)
        }
        val (sendUpdateSlaveHeader, sendUpdateSlaveSource) = emitSendUpdateSlave(tp)
        if (!helperFuncList.contains(sendUpdateSlaveHeader)) {
          headerStream.println(sendUpdateSlaveHeader)
          helperFuncStream.println(sendUpdateSlaveSource)
          helperFuncList.append(sendUpdateSlaveHeader)
        }
        val (recvUpdateSlaveHeader, recvUpdateSlaveSource) = emitRecvUpdateSlave(tp)
        if (!helperFuncList.contains(recvUpdateSlaveHeader)) {
          headerStream.println(recvUpdateSlaveHeader)
          helperFuncStream.println(recvUpdateSlaveSource)
          helperFuncList.append(recvUpdateSlaveHeader)
        }

      // Emit output copy helper functions for object type inputs
        val (sendHeader, sendSource) = emitSend(tp, Hosts.JVM)
        if (!helperFuncList.contains(sendHeader)) {
          headerStream.println(sendHeader)
          helperFuncStream.println(sendSource)
          helperFuncList.append(sendHeader)
        }
        val (sendViewHeader, sendViewSource) = emitSendView(tp, Hosts.JVM)
        if (!helperFuncList.contains(sendViewHeader)) {
          headerStream.println(sendViewHeader)
          helperFuncStream.println(sendViewSource)
          helperFuncList.append(sendViewHeader)
        }
        val (recvSlaveHeader, recvSlaveSource) = emitRecvSlave(tp)
        if (!helperFuncList.contains(recvSlaveHeader)) {
          headerStream.println(recvSlaveHeader)
          helperFuncStream.println(recvSlaveSource)
          helperFuncList.append(recvSlaveHeader)
        }
      }
      catch {
        case e: GenerationFailedException => 
          helperFuncStream.flush
          headerStream.flush
        case e: Exception => throw(e)
      }
    }

    helperFuncStream.flush
    headerStream.flush
  }

  def registerKernel(syms: List[Sym[Any]]) {
    isGPUable = true
    if(kernelsList.intersect(syms).isEmpty) {
      //headerStream.println("#include \"%s.%s\"".format(syms.map(quote).mkString(""),kernelFileExt))
      kernelsList ++= syms
    }
  }

  def emitAllocOutput(sym: Sym[Any], ksym: List[Sym[Any]], contents: String, args: List[Sym[Any]], aV: Sym[Any]): String = {
    val out = new StringBuilder
    val funcName = "allocFunc_%s".format(quote(sym))
    if(helperFuncList contains funcName) return ""
    helperFuncList += funcName

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
      headerStream.append(out.toString + ";\n")
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
      headerStream.append(out.toString + ";\n")
      out.append("{\n")
      out.append(contents)
      out.append("}\n")
      out.toString
    }
  }


  /* emitAllocFunc method emits code for allocating the output memory of a kernel,
       and copying  it to CPU memory with allocation of new object in CPU */
  //TODO: Separate output and temporary allocations
  def emitAllocFunc(sym:Sym[Any], allocFunc:List[Block[Any]], boundings:List[(Sym[Any],Exp[Any])], aV:Sym[Any]=null, size:Exp[Any]=null) {
    processingHelperFunc = true
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString,true)

    // Get free variables (exclude the arrayVariable)
    val inputs = if(allocFunc.isEmpty) Nil
    else ((allocFunc.flatMap(f => getFreeVarBlock(f, Nil)) ++ boundings.map(_._2).filter(_.isInstanceOf[Sym[Any]]).map(_.asInstanceOf[Sym[Any]])).filterNot(s => (boundings.map(_._1)++List(aV)) contains s)).distinct

    // Register metadata
    val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
    tr.argsFuncHtoD = inputs
    metaData.outputs.put(sym,tr)


    for (b <- boundings) {
      withStream(tempStream) {
        emitValDef(b._1,quote(b._2))
      }
    }

    // Get the body (string) of the allocation function in tempString
    //if(allocFunc!=null) {
    if(!allocFunc.isEmpty) {
      withStream(tempStream) {
        allocFunc.foreach(emitBlock)
      }
      tempString.append("\treturn %s_ptr;\n".format(quote(getBlockResult(allocFunc.last))))
    }
    else {
      tempString.append("\treturn %s_ptr;\n".format(quote(sym)))
    }

    // Emit the full allocation function
    val allocOutputStr = emitAllocOutput(sym, null, tempString.toString, inputs, aV)

    // Write to helper function string
    helperFuncStream.println(allocOutputStr)
    //helperFuncString.append(allocOutputStr)

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
    helperFuncStream.println(allocOutputStr)
    //helperFuncString.append(allocOutputStr)

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
