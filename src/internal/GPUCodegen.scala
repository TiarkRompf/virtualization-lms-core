package scala.virtualization.lms
package internal

import java.io.{StringWriter, PrintWriter, File}
import collection.mutable.{ListBuffer, HashMap, ListMap, HashSet}
import collection.immutable.List._

trait GPUCodegen extends CLikeCodegen {
  val IR: Expressions
  import IR._

  /* Kernel input / output symbols */
  private var kernelInputs: List[Sym[Any]] = null
  private var kernelOutputs: List[Sym[Any]] = null
  def getKernelInputs = kernelInputs
  def setKernelInputs(syms: List[Sym[Any]]): Unit = { kernelInputs = syms }
  def getKernelOutputs = kernelOutputs
  def setKernelOutputs(syms: List[Sym[Any]]): Unit = { kernelOutputs = syms }
  def getKernelTemps = metaData.temps.toList.reverse.map(ele => ele._1)

  /* For using GPU local variables */
  final class GPUOptimizer {
    // Map (collection,idx) -> localVar
    var collToVar: HashMap[(Sym[Any],String),String] = HashMap[(Sym[Any],String),String]()
    // Map variable symbol -> set of aliased symbols
    var varToSet: HashMap[String,HashSet[String]] = HashMap[String,HashSet[String]]()

    /*
    //TODO: deal with aliased collection
    def hasLocalVar(coll:Sym[Any], idx: String): Boolean = {
      var hit = false
      for (i <- varToSet(idx)) {
        if (collToVar contains  (coll,i))
          hit = true
      }
      hit
    }
    def getLocalVar(coll:Sym[Any], idx: String): String = {
      assert(hasLocalVar(coll,idx))
      for (i <- varToSet(idx) if (collToVar contains (coll,i))) {
        return collToVar.get(coll,i).get
      }
    }
    def registerAlias(sym:String,aliased:String) {
      if (varToSet contains aliased) {
        val set = varToSet.get(aliased).get
        set.add(sym)
        varToSet.put(sym,set)
      }
      else if (varToSet contains sym) {
        val set = varToSet.get(sym).get
        set.add(aliased)
        varToSet.put(aliased,set)
      }
      else {
        val set = HashSet[String](sym,aliased)
        varToSet.put(sym,set)
        varToSet.put(aliased,set)
      }
      println("resitested alias: %s = %s".format(sym,aliased))
    }
    */
    //def registerLocalVarArith(sym:String, )
    //def registerLocalVar(key:(Sym[Any],String), value:String) {
    //  collToVar.put(key,value)
    //}
  }
  var optimizer: GPUOptimizer = null

  /* Indicates current dimension of work threads */
  var currDim = 0
  val xDimList =  ListBuffer[String]()
  val yDimList =  ListBuffer[String]()
  def getCurrDimStr():String = currDim match {
    case 0 => throw new RuntimeException("GPUGen: Cannot get current dimension string when the dimension is 0.")
    case 1 => "idxX"
    //case 2 => "idxY"
    case _ => throw new GenerationFailedException("GPUGen: Maximum 2 dimensions for GPU kernels.")
  }
  def getPrevDimStr():String = currDim match {
    case 0 => throw new RuntimeException("GPUGen: Cannot get previous dimension string when the dimension is 0.")
    case 1 => throw new RuntimeException("GPUGen: Cannot get previous dimension string when the dimension is 1.")
    case 2 => "idxX"
    //case 3 => "idxY"
    case _ => throw new GenerationFailedException("GPUGen: Maximum 1 dimensions for GPU kernels.")
  }
  def getNextDimStr():String = currDim match {
    case 0 => "idxX"
    //case 1 => throw new RuntimeException("GPUGen: Cannot get next dimension string when the dimension is 1.")
    case _ => throw new GenerationFailedException("GPUGen: Maximum 1 dimensions for GPU kernels.")
  }
  def setCurrDimLength(length: String) {
    currDim match {
      case 0 => throw new RuntimeException("GPUGen: Cannot set dimension length when the dimension is 0.")
      case 1 => xDimList += length
      //case 2 => yDimList += length
      case _ => throw new GenerationFailedException("GPUGen: Maximum 1 dimensions for GPU kernels.")
    }
  }
  val multDimInputs = ListBuffer[Sym[Any]]()

  var helperFuncIdx = 0
  var kernelsList = ListBuffer[Exp[Any]]()

  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth

  var forceParallel = false

  var helperFuncString:StringBuilder = null
  var hstream: PrintWriter = null
  var devFuncString:StringBuilder = null
  var devFuncIdx = 0
  var devStream: PrintWriter = null
  var headerStream: PrintWriter = null


  def emitDevFunc(func:Block[Any], locals:List[Exp[Any]]):(String,List[Exp[Any]]) = {
    devFuncIdx += 1
    val currIdx = devFuncIdx
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

    val inputs = (getFreeVarBlock(func,Nil).filterNot(ele => locals.contains(ele))++getKernelTemps).distinct
    val paramStr = (locals++inputs).map(ele=>remap(ele.Type)+" "+quote(ele)).mkString(",")
    header.append("__device__ %s dev_%s(%s) {\n".format(remap(getBlockResult(func).Type),currIdx,paramStr))
    header.append("\tint idxX = blockIdx.x*blockDim.x + threadIdx.x;\n")
    header.append("\tint idxY = blockIdx.y*blockDim.y + threadIdx.y;\n")
    if(remap(getBlockResult(func).Type) != "void")
      footer.append("\treturn %s;\n".format(quote(getBlockResult(func))))
    footer.append("}\n")
    devFuncString.append(header)
    devFuncString.append(tempString)
    devFuncString.append(footer)

    ("dev_"+currIdx,inputs)
  }

  // MetaData
  override def hasMetaData: Boolean = true
  override def getMetaData: String = metaData.toString
  var metaData: GPUMetaData = null

  final class TransferFunc {
    var funcHtoD:String = _
    var argsFuncHtoD:List[Sym[Any]] = _
    var funcDtoH:String = _
    var argsFuncDtoH:List[Sym[Any]] = _
    //override def toString: String = {  }
  }
  final class SizeFunc(val func:String, val args:List[Sym[Any]]) {
    override def toString: String = {
      "[\"%s\",[%s]]".format(func,args.map("\""+quote(_)+"\"").mkString(","))
    }
  }
  final class GPUMetaData {
    val inputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val outputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val temps: ListMap[Sym[Any],TransferFunc] = ListMap()
    val sizeFuncs: ListMap[String,SizeFunc] = ListMap()
    var gpuLibCall: String = ""
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")
      //TODO: Change to use the toString methods
      for (f <- sizeFuncs.toList) {
        out.append("\"%s\":".format(f._1)+f._2.toString+",")
      }

      if (kernelFileExt == "cu") {
        out.append("\"gpuInputs\":["+inputs.toList.reverse.map(in=>"{\""+quote(in._1)+"\":[\""+remap(in._1.Type)+"\",\""+in._2.funcHtoD+"\",\""+in._2.funcDtoH+"\"]}").mkString(",")+"],")
        out.append("\"gpuOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.Type)+"\",\""+out._2.funcHtoD+"\","+"["+ out._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"]"+",\""+out._2.funcDtoH+"\"]}").mkString(",")+"],")
        out.append("\"gpuTemps\":["+temps.toList.reverse.map(temp=>"{\""+quote(temp._1)+"\":[\""+remap(temp._1.Type)+"\",\""+temp._2.funcHtoD+"\","+"["+ temp._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"]"+",\""+temp._2.funcDtoH+"\"]}").mkString(",")+"]")
      }
      else { //opencl
        out.append("\"gpuInputs\":["+inputs.toList.reverse.map(in=>"{\""+quote(in._1)+"\":[\""+remap(in._1.Type)+"\",\""+in._2.funcHtoD+"\",\""+in._2.funcDtoH+"\",{"+unpackObject(in._1).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(",")+"}]}").mkString(",")+"],")
        out.append("\"gpuOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.Type)+"\",\""+out._2.funcHtoD+"\","+"["+ out._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"]"+",\""+out._2.funcDtoH+"\",{"+unpackObject(out._1).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(",")+"}]}").mkString(",")+"],")
        out.append("\"gpuTemps\":["+temps.toList.reverse.map(temp=>"{\""+quote(temp._1)+"\":[\""+remap(temp._1.Type)+"\",\""+temp._2.funcHtoD+"\","+"["+ temp._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"]"+",\""+temp._2.funcDtoH+"\",{"+unpackObject(temp._1).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(",")+"}]}").mkString(",")+"]")
      }

      out.append("}")
      out.toString
    }
  }

  // Exception Handler function
  override def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
     super.exceptionHandler(e, outFile, kstream)
     // TODO: Need to cleanup some data structures
  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    // Set kernel input and output symbols
    setKernelInputs(vals)
    setKernelOutputs(syms)

    // Conditions for not generating GPU kernels (may be relaxed later)
    for (sym <- syms) {
      if((!isObjectType(sym.Type)) && (remap(sym.Type)!="void")) throw new GenerationFailedException("GPUGen: Not GPUable output type : %s".format(remap(sym.Type)))
    }
    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("GPUGen: Not GPUable input/output types: Variable")

    // Initialize global variables
    //useLocalVar = false
    //GPUVarMap.clear
    optimizer = new GPUOptimizer

    currDim = 0
    xDimList.clear
    yDimList.clear
    multDimInputs.clear

    helperFuncString.clear
    metaData = new GPUMetaData
    //MetaData.init
    tabWidth = 1
    devFuncString = new StringBuilder

    forceParallel = false
  }

  // Map a scala primitive type to JNI type descriptor
  def JNITypeDescriptor[A](m: Manifest[A]) : String = m.toString match {
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case "Boolean" => "Z"
    case _ => throw new GenerationFailedException("Undefined GPU type")
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
      throw new GenerationFailedException("GPUGen: Type %s is not a GPUable Type.".format(m.toString))
  }

  // All the types supported by GPU Generation
  def isGPUableType[A](m : Manifest[A]) : Boolean = {
    if(!isObjectType(m) && !isPrimitiveType(m) && !isVoidType(m) && !isVariableType(m))
      false
    else
      true
  }

  def unpackObject[A](sym: Sym[Any]) : Map[String,Manifest[_]] = remap(sym.Type) match {
    case _ => throw new GenerationFailedException("GPUCodegen: Type %s cannot be unpacked.".format(sym.Type.toString))
  }

  def copyInputHtoD(sym: Sym[Any]) : String
  def copyOutputDtoH(sym: Sym[Any]) : String
  def copyMutableInputDtoH(sym: Sym[Any]) : String
  def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit
  def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit
  def positionMultDimInputs(sym: Sym[Any]) : String
  def cloneObject(sym: Sym[Any], src: Sym[Any]) : String

  /*******************************************************
   * Methods below are for emitting helper functions
   *******************************************************/
  // TODO: Change the metadata function names

  // For object type inputs, allocate GPU memory and copy from CPU to GPU.
  def emitCopyInputHtoD(sym: Sym[Any], ksym: List[Sym[Any]], contents: String) : String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
      helperFuncIdx += 1
      out.append("%s *copyInputHtoD_%s_%s_%s(%s) {\n".format(remap(sym.Type), ksym.map(quote).mkString(""), quote(sym),helperFuncIdx, "JNIEnv *env , jobject obj"))
      out.append(contents)
      out.append("}\n")
      val tr = metaData.inputs.getOrElse(sym,new TransferFunc)
      tr.funcHtoD = "copyInputHtoD_%s_%s_%s".format(ksym.map(quote).mkString(""),quote(sym),helperFuncIdx)
      metaData.inputs.put(sym,tr)
      out.toString
    }
    else ""
  }

  // For mutable inputs, copy the mutated datastructure from GPU to CPU after the kernel is terminated
  def emitCopyMutableInputDtoH(sym: Sym[Any], ksym: List[Sym[Any]], contents: String): String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
      helperFuncIdx += 1
      out.append("void copyMutableInputDtoH_%s_%s_%s(%s) {\n".format(ksym.map(quote).mkString(""), quote(sym), helperFuncIdx, "JNIEnv *env , jobject obj, "+remap(sym.Type)+" *"+quote(sym)+"_ptr"))
      out.append("%s %s = *(%s_ptr);\n".format(remap(sym.Type),quote(sym),quote(sym)))
      out.append(contents)
      out.append("}\n")
      val tr = metaData.inputs.getOrElse(sym,new TransferFunc)
      tr.funcDtoH = "copyMutableInputDtoH_%s_%s_%s".format(ksym.map(quote).mkString(""),quote(sym),helperFuncIdx)
      metaData.inputs.put(sym,tr)
      out.toString
    }
    else ""
  }

  def emitAllocOutput(sym: Sym[Any], ksym: List[Sym[Any]], contents: String, args: List[Sym[Any]]): String = {
    //println("alloc for " + quote(sym))
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
    	helperFuncIdx += 1
      val paramStr = args.map(ele =>
  			if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele) + "_ptr"
  			else remap(ele.Type) + " " + quote(ele)
      ).mkString(",")
    	val derefParams = args.map(ele=>
        if(isObjectType(ele.Type)) "\t%s %s = *(%s_ptr);\n".format(remap(ele.Type),quote(ele),quote(ele))
        else ""
      ).mkString("")

      if (getKernelOutputs contains sym) {
        val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
        tr.funcHtoD = "allocFunc_%s".format(helperFuncIdx)
        tr.argsFuncHtoD = args
        metaData.outputs.put(sym,tr)
      }
      else {
        //println("added to temp " + quote(sym))
        val tr = metaData.temps.getOrElse(sym,new TransferFunc)
        tr.funcHtoD = "allocFunc_%s".format(helperFuncIdx)
        tr.argsFuncHtoD = args
        metaData.temps.put(sym,tr)
      }

      out.append("%s *allocFunc_%s(%s) {\n".format(remap(sym.Type), helperFuncIdx, paramStr))
  	  out.append(derefParams+"\n")
      out.append(contents)
      out.append("}\n")
      out.toString
    }
    else ""
  }

  def emitCopyOutputDtoH(sym: Sym[Any], ksym: List[Sym[Any]], contents: String): String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
    	helperFuncIdx += 1
      if (getKernelOutputs contains sym) {
        val tr = metaData.outputs.getOrElse(sym,new TransferFunc)
        tr.funcDtoH = "copyOutputDtoH_%s".format(helperFuncIdx)
        metaData.outputs.put(sym,tr)
      }
      else {
        val tr = metaData.temps.getOrElse(sym,new TransferFunc)
        tr.funcDtoH = "copyOutputDtoH_%s".format(helperFuncIdx)
        metaData.temps.put(sym,tr)
      }

      out.append("jobject copyOutputDtoH_%s(JNIEnv *env,%s) {\n".format(helperFuncIdx,remap(sym.Type)+" *"+quote(sym)+"_ptr"))
  	  out.append("\t%s %s = *(%s_ptr);\n".format(remap(sym.Type),quote(sym),quote(sym)))
      out.append(contents)
      out.append("}\n")
      out.toString
    }
    else ""
  }

  /* emitAllocFunc method emits code for allocating the output memory of a kernel,
       and copying  it to CPU memory with allocation of new object in CPU */
  //TODO: Separate output and temporary allocations
  def emitAllocFunc(sym:Sym[Any], allocFunc:Block[Any]) {
    helperFuncIdx += 1
    val tempString = new StringWriter
    val tempString2 = new StringWriter
    val tempStream = new PrintWriter(tempString,true)

    // Need to save idx before calling emitBlock, which might recursively call this method
    val currHelperFuncIdx = helperFuncIdx

    // Get free variables
    val inputs = getFreeVarBlock(allocFunc,Nil)

    // Get the body (string) of the allocation function in tempString
    withStream(tempStream)(emitBlock(allocFunc))
    tempString.append("\treturn %s_ptr;\n".format(quote(getBlockResult(allocFunc))))

    // Emit the full allocation function
    val allocOutputStr = emitAllocOutput(sym, null, tempString.toString, inputs)

    // Generate copy (D->H) helper function
    tempString2.append(copyOutputDtoH(sym))
    val copyOutputStr = emitCopyOutputDtoH(sym, null, tempString2.toString)

    // Write to helper function string
    helperFuncString.append(allocOutputStr)
    helperFuncString.append(copyOutputStr)
  }

  def emitCloneFunc(sym:Sym[Any], src:Sym[Any]) {
    helperFuncIdx += 1
    val tempString = new StringWriter
    val tempString2 = new StringWriter
    val tempStream = new PrintWriter(tempString,true)

    // Need to save idx before calling emitBlock, which might recursively call this method
    val currHelperFuncIdx = helperFuncIdx

    // Get free variables
    //val inputs = getFreeVarBlock(allocFunc,Nil)
    val inputs = List(src)

    // Get the body (string) of the allocation function in tempString
    //emitBlock(allocFunc)(tempStream)
    tempString.append(cloneObject(sym,src))
    tempString.append("\treturn %s_ptr;\n".format(quote(sym)))

    // Emit the full allocation function
    val allocOutputStr = emitAllocOutput(sym, null, tempString.toString, inputs)

    // Generate copy (D->H) helper function
    tempString2.append(copyOutputDtoH(sym))
    val copyOutputStr = emitCopyOutputDtoH(sym, null, tempString2.toString)

    // Write to helper function string
    helperFuncString.append(allocOutputStr)
    helperFuncString.append(copyOutputStr)
  }

  /**********************************************************
   * Calculation and Emission of GPU kernel size functions
   *********************************************************/

  //TODO: Get this information from the environment (GPU device version)
  val MAX_THREADS_PER_BLOCK = 512

  def emitCheckSize(varName: String, lst: ListBuffer[String]):String = {
    val out = new StringBuilder
    out.append("int %s = 1;\n".format(varName))
    if(lst.size > 0) {
      out.append("%s = %s;\n".format(varName,lst(0)))
      for(size <- lst) {
        out.append("if(%s != %s) printf(\"ERROR: GPU kernel sizes for %s are not the same\\n\");\n".format(varName,size,varName))
        out.append("%s = %s;\n".format(varName,size))
      }
    }
    out.toString
  }

  // Prints out the helper functions for getting the threadBlcok size and grid size
  def emitSizeFuncs(syms: List[Sym[Any]], external: Boolean): String = {
    val sym = syms(0)  //TODO: Fix
    helperFuncIdx += 1

    val out = new StringBuilder

    if(xDimList.size == 0 && !external)
      throw new GenerationFailedException("GPUGen: No dimension specified for this kernel.")

    val inputs = (getKernelOutputs++getKernelInputs++getKernelTemps).filterNot(e=>isVoidType(e.Type))
    val paramStr = inputs.map(ele=>
  		if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele)
  		else remap(ele.Type) + " " + quote(ele)
    ).mkString(",")
    val argStr = inputs.map("\""+quote(_)+"\"").mkString(",")
    val argInputStr = inputs.map(quote(_)).mkString(",")

    //TODO: Restore safety check for the dimension sizes
    out.append("int gpuBlockSizeX_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    if(xDimList.length==0)
        out.append("\tint X = 1;\n")
    else
      out.append("\tint X = %s;\n".format(xDimList(xDimList.length-1)))
    out.append("\tif(X < %s) return X;\n".format(MAX_THREADS_PER_BLOCK))
    out.append("\telse return %s;\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    metaData.sizeFuncs.put("gpuBlockSizeX",new SizeFunc("gpuBlockSizeX_%s_%s".format(quote(sym),helperFuncIdx),inputs))

    out.append("int gpuBlockSizeY_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    metaData.sizeFuncs.put("gpuBlockSizeY",new SizeFunc("gpuBlockSizeY_%s_%s".format(quote(sym),helperFuncIdx),inputs))

    out.append("int gpuBlockSizeZ_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    metaData.sizeFuncs.put("gpuBlockSizeZ",new SizeFunc("gpuBlockSizeZ_%s_%s".format(quote(sym),helperFuncIdx),inputs))

    out.append("int gpuDimSizeX_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    if(xDimList.length==0)
    	out.append("\tint X = 1;\n")
    else
    	out.append("\tint X = %s;\n".format(xDimList(xDimList.length-1)))
    out.append("\treturn 1+((X-1)/%s);\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    metaData.sizeFuncs.put("gpuDimSizeX",new SizeFunc("gpuDimSizeX_%s_%s".format(quote(sym),helperFuncIdx),inputs))

    out.append("int gpuDimSizeY_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    metaData.sizeFuncs.put("gpuDimSizeY",new SizeFunc("gpuDimSizeY_%s_%s".format(quote(sym),helperFuncIdx),inputs))

    out.toString
  }
}
