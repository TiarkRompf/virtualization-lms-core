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
  protected val helperFuncList = ArrayBuffer[String]()

  // Current tab location for pretty printing
  protected var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth

  var devFuncIdx = 0
  var isGPUable:Boolean = false
  var processingHelperFunc: Boolean = false
  var isNestedNode: Boolean = false
  var outerLoopSize: Exp[Int] = null
  var outerLoopSym: Exp[Int] = null

  //TODO: Get rid of this variable
  protected var inVars = List[Sym[Any]]()
  protected val boundMap = HashMap[Exp[Int],Exp[Int]]()

  protected def registerTempAlloc(sym:Sym[Any], tp:Manifest[Any], size:Exp[Int]):String = {
    metaData.temps prepend TempAlloc(quote(sym)+"_temp",remap(tp),quote(size))
    quote(sym) + "_temp"
  }

  // MetaData
  override def hasMetaData: Boolean = true
  override def getMetaData: String = metaData.toString
  var metaData: GPUMetaData = null

  final class LoopElem(val elemType: String, val types: Map[String,String]) {
    val funcs = new HashMap[String,List[String]]() // Mapping of function name to the argument list   

    override def toString: String = {
      "{\"elemType\":\"" + 
      elemType + 
      "\",\"types\":" + 
      types.map(t => "\"" + t._1 + "\":\"" + t._2 + "\"").mkString("{",",","}") + 
      ",\"funcs\":" + 
      funcs.map(f => "\"" + f._1 + "\":[" + f._2.map(i => "\"" + i +  "\"").mkString(",") + "]").mkString("{",",","}") + 
      "}"
    }
  }
  
  case class TempAlloc(sym: String, tp: String, size:String)

  final class GPUMetaData(val kernelInputs: List[Sym[Any]]) {
    //val inputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val outputs: ListMap[Sym[Any],LoopElem] = ListMap()
    val temps: ListBuffer[TempAlloc] = ListBuffer()
    var gpuLibCall: String = ""
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")

      //if (kernelFileExt == "cu") {
      //  out.append("\"gpuInputs\":["+kernelInputs.filter(in => !isPrimitiveType(in.tp)).map(in=>"{\""+quote(in)+"\":[\""+remap(in.tp)+"\"]}").mkString(",")+"],")
        out.append("\"gpuOutputs\":{"+outputs.map(o => "\""+quote(o._1)+"\":"+o._2.toString).mkString(",")+"},")
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
      super.emitKernelHeader(syms, syms ::: vals, vars, resultType, resultIsVar, external)
    }
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
    actRecordStream.flush
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
