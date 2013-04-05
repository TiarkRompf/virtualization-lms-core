package scala.virtualization.lms
package internal

import java.io.{StringWriter, PrintWriter, File}
import collection.immutable.List._
import scala.reflect.SourceContext
import collection.mutable.{HashMap, HashSet, ArrayBuffer, ListMap, ListBuffer}

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
  var outerLoopSize: Exp[Int] = null
  var outerLoopSym: Exp[Int] = null

  //TODO: Get rid of this variable
  protected var inVars = List[Sym[Any]]()
  protected val boundMap = HashMap[Exp[Int],Exp[Int]]()

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

    def addRef(elem: Sym[Any], tp: String): String = {
      if(inVars contains elem) {
        "Ref< " + tp + " >"
      }
      else 
        tp
    }

    val inputs = getFreeVarBlock(func,lastInputs).distinct
    val paramStr = ((inputs++lastInputs).map(ele => addRef(ele,remap(ele.tp)) + " " + quote(ele)) ++ metaData.temps.map(t=>t.tp + " *" + t.sym) ++ List("size_t tempMemSize","char *tempMemPtr","int *tempMemUsage")).mkString(",")

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


  protected def registerTempAlloc(sym:Sym[Any], tp:Manifest[Any], size:Exp[Int]):String = {
    metaData.temps prepend TempAlloc(quote(sym)+"_temp",remap(tp),quote(size))
    quote(sym) + "_temp"
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

  case class TempAlloc(sym: String, tp: String, size:String)

  final class GPUMetaData(val kernelInputs: List[Sym[Any]]) {
    val inputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val outputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val temps: ListBuffer[TempAlloc] = ListBuffer()
    var gpuLibCall: String = ""
    var loopFuncs: ListMap[Sym[Any],LoopFunc] = ListMap()
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")

      //if (kernelFileExt == "cu") {
        out.append("\"gpuInputs\":["+kernelInputs.filter(in => !isPrimitiveType(in.tp)).map(in=>"{\""+quote(in)+"\":[\""+remap(in.tp)+"\"]}").mkString(",")+"],")
        out.append("\"gpuOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.tp)+"\",["+ out._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"],"+loopFuncs.getOrElse(out._1,new LoopFunc).toString+"]}").mkString(",")+"],")
        out.append("\"gpuTemps\":["+temps.map(t=>"{\""+t.sym+"\":[\""+t.tp+"\",\""+t.size+"\"]}").mkString(",")+"]")
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
    inVars = vars
    boundMap.clear

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

  def emitAllocFunc(blocks: List[(Sym[Any],Block[Any])], funcName: String, lastInputs: List[Sym[Any]], boundVals:Map[Sym[Any],Exp[Any]]) {
    processingHelperFunc = true
    
    if(helperFuncList contains funcName) return 
    helperFuncList += funcName

    val sym = blocks.last._1
    val out = new StringBuilder
    val blockString = new StringWriter
    val blockStream = new PrintWriter(blockString,true)

    val inputs = (blocks.flatMap(f => getFreeVarBlock(f._2, Nil)) ++ boundVals.values.filter(_.isInstanceOf[Sym[Any]]).map(_.asInstanceOf[Sym[Any]]).toList).distinct.filterNot(i => blocks.map(_._1).contains(i) || boundVals.keySet.contains(i) || lastInputs.contains(i)) ++ lastInputs

    // register metadata
    val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
    tr.argsFuncHtoD = inputs
    metaData.outputs.put(sym,tr)

    // emit bouding symbols
    for (b <- boundVals) {
      withStream(blockStream) {
        emitValDef(b._1,quote(b._2))
      }
    }

    // emit block bodies
    for (b <- blocks) {
      withStream(blockStream) {
        emitBlock(b._2)
        emitValDef(b._1, quote(getBlockResult(b._2)))
        stream.println("\t%s *%s_ptr = %s_ptr;\n".format(remap(b._1.tp),quote(b._1),quote(getBlockResult(b._2))))
      }
    }
    blockString.append("\treturn %s_ptr;\n".format(quote(blocks.last._1)))

    val paramStr = inputs.map(s =>
      if(isPrimitiveType(s.tp)) remap(s.tp) + " " + quote(s)
      else remap(s.tp) + " *" + quote(s) + "_ptr"
    ).mkString(",")
    val derefParams = inputs.map(s =>
      if(isPrimitiveType(s.tp)) ""
      else "\t%s %s = *(%s_ptr);\n".format(remap(s.tp),quote(s),quote(s))
    ).mkString("")

    // emit header and complete host function
    out.append("%s *%s(%s)".format(remap(sym.tp), funcName, paramStr))
    headerStream.append(out.toString + ";\n")
    out.append("{\n")
    out.append(derefParams)
    out.append(blockString)
    out.append("}\n")
    helperFuncStream.println(out.toString)

    processingHelperFunc = false
  }

  def emitAllocFuncPrimitive(sym:Sym[Any], funcName: String) {
    processingHelperFunc = true
    assert(isPrimitiveType(sym.tp))
    
    if(helperFuncList contains funcName) return 
    helperFuncList += funcName
    val out = new StringBuilder
    val allocString = new StringWriter

    // Register metadata
    val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
    tr.argsFuncHtoD = Nil
    metaData.outputs.put(sym,tr)

    allocString.append("\t%s *ptr;\n".format(remap(sym.tp)))
    allocString.append("\tDeliteCudaMalloc((void**)&ptr, sizeof(%s));\n".format(remap(sym.tp)))
    allocString.append("\treturn ptr;\n")

    // Emit the full allocation function
    out.append("%s *%s(void)".format(remap(sym.tp),funcName))
    headerStream.append(out.toString + ";\n")
    out.append("{\n")
    out.append(allocString)
    out.append("}\n")

    // Write to helper function string
    helperFuncStream.println(out.toString)

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
