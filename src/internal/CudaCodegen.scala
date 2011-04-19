package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap}
import collection.immutable.List._

trait CudaCodegen extends CLikeCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cu"
  override def toString = "cuda"

  /* For using GPU local variables */
  var useLocalVar:Boolean = false
  val indexMap = HashMap[Exp[Any],String]()
  private val cudaVarMap = HashMap[Tuple2[Exp[Any],String],String]()
  private var localVarIdx = 0
  def getNewLocalVar():String = {
    localVarIdx +=1
    "local_"+localVarIdx
  }
  def hasLocalVar(sym:Exp[Any],idx:String):Boolean = {
    cudaVarMap.contains(sym,idx)
  }
  def getLocalVar(sym:Exp[Any],idx:String):String = {
    cudaVarMap.get(sym,idx) match {
      case None => null
      case Some(varName) => varName
    }
  }
  def saveLocalVar(sym:Exp[Any],idx:String,varName:String) {
    cudaVarMap.put(Tuple2(sym,idx),varName)
  }

  /* Indicates current dimension of work threads */
  var currDim = 0
  val xDimList =  ListBuffer[String]()
  val yDimList =  ListBuffer[String]()
  def getCurrDimStr():String = currDim match {
    case 0 => throw new RuntimeException("CudaGen: Cannot get current dimension string when the dimension is 0.")
    case 1 => "idxX"
    //case 2 => "idxY"
    case _ => throw new GenerationFailedException("CudaGen: Maximum 2 dimensions for GPU kernels.")
  }
  def getPrevDimStr():String = currDim match {
    case 0 => throw new RuntimeException("CudaGen: Cannot get previous dimension string when the dimension is 0.")
    case 1 => throw new RuntimeException("CudaGen: Cannot get previous dimension string when the dimension is 1.")
    case 2 => "idxX"
    //case 3 => "idxY"
    case _ => throw new GenerationFailedException("CudaGen: Maximum 1 dimensions for GPU kernels.")
  }
  def getNextDimStr():String = currDim match {
    case 0 => "idxX"
    //case 1 => throw new RuntimeException("CudaGen: Cannot get next dimension string when the dimension is 1.")
    case _ => throw new GenerationFailedException("CudaGen: Maximum 1 dimensions for GPU kernels.")
  }
  def setCurrDimLength(length: String) {
    currDim match {
      case 0 => throw new RuntimeException("CudaGen: Cannot set dimension length when the dimension is 0.")
      case 1 => xDimList += length
      //case 2 => yDimList += length
      case _ => throw new GenerationFailedException("CudaGen: Maximum 1 dimensions for GPU kernels.")
    }
  }
  val multDimInputs = ListBuffer[Sym[Any]]()

  var helperFuncIdx = 0
  var kernelsList = ListBuffer[Exp[Any]]()

  var kernelSymbol:Sym[Any] = null
  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth
  
  var gpuInputs:List[Sym[Any]] = Nil
  var gpuOutputs:List[Sym[Any]] = Nil
  var gpuTemps:List[Sym[Any]] = Nil
  var gpuInputsStr = ""
  var gpuOutputStr = ""
  var gpuTempsStr = ""

  var helperFuncString:StringBuilder = null
  var hstream: PrintWriter = null
  var devFuncString:StringBuilder = null
  var devFuncIdx = 0
  var devStream: PrintWriter = null
  var headerStream: PrintWriter = null

  // MetaData structure
  override def hasMetaData: Boolean = true
  override def getMetaData: String = MetaData.toString

  def emitDevFunc(func:Exp[Any], locals:List[Exp[Any]]):(String,List[Exp[Any]]) = {
    devFuncIdx += 1
    val currIdx = devFuncIdx
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString, true)
    val header = new StringWriter
    val footer = new StringWriter

    val currentTab = tabWidth
    tabWidth = 1
    emitBlock(func)(tempStream)
    tabWidth = currentTab

    val inputs = (getFreeVarBlock(func,Nil).filterNot(ele => locals.contains(ele))++gpuTemps).distinct
    val paramStr = (locals++inputs).map(ele=>remap(ele.Type)+" "+quote(ele)).mkString(",")
    header.append("__device__ %s dev_%s(%s) {\n".format(remap(func.Type),currIdx,paramStr))
    header.append("\tint idxX = blockIdx.x*blockDim.x + threadIdx.x;\n")
    header.append("\tint idxY = blockIdx.y*blockDim.y + threadIdx.y;\n")
    if(remap(func.Type) != "void")
      footer.append("\treturn %s;\n".format(quote(getBlockResult(func))))
    footer.append("}\n")
    devFuncString.append(header)
    devFuncString.append(tempString)
    devFuncString.append(footer)

    ("dev_"+currIdx,inputs)
  }

  object MetaData {
    var gpuBlockSizeX: String = ""
    var gpuBlockSizeY: String = ""
    var gpuBlockSizeZ: String = ""
    var gpuDimSizeX: String = ""
    var gpuDimSizeY: String = ""
    var gpuInputs: ArrayList[String] = new ArrayList[String]
    var gpuOutput: String = ""
    var gpuTemps: ArrayList[String] = new ArrayList[String]
    var gpuLibCall: String = ""

    def init = {
      gpuBlockSizeX = ""
      gpuBlockSizeY = ""
      gpuBlockSizeZ = ""
      gpuDimSizeX = ""
      gpuDimSizeY = ""
      gpuInputs = new ArrayList[String]
      gpuOutput = ""
      gpuTemps = new ArrayList[String]
      gpuLibCall = ""
    }
    
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")
      out.append("\"gpuBlockSizeX\":"+gpuBlockSizeX+",")
      out.append("\"gpuBlockSizeY\":"+gpuBlockSizeY+",")
      out.append("\"gpuBlockSizeZ\":"+gpuBlockSizeZ+",")
      out.append("\"gpuDimSizeX\":"+gpuDimSizeX+",")
      out.append("\"gpuDimSizeY\":"+gpuDimSizeY+",")
      out.append("\"gpuInputs\":"+gpuInputs.toString+",")
      //if(gpuOutput == "") { println("ERROR:No Output for GPU?"); throw new Exception()}
      if(gpuOutput != "")
        out.append("\"gpuOutput\":"+gpuOutput+",")
      out.append("\"gpuTemps\":"+gpuTemps.toString)
      if(gpuLibCall != "") out.append(",\"gpuLibCall\":"+gpuLibCall.toString)
      out.append("}")
      out.toString
    }
  }

  // Exception Handler function
  override def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
     super.exceptionHandler(e, outFile, kstream)
     // TODO: Need to cleanup some data structures
  }

  override def generatorInit(buildDir:String): Unit = {
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
    hstream.print("#include <cublas.h>\n\n")
    hstream.print("#include <jni.h>\n\n")
    hstream.print("//Delite Runtime APIs\n")
    hstream.print("extern void DeliteCudaMallocHost(void **ptr, size_t size);\n")
    hstream.print("extern void DeliteCudaMalloc(void **ptr, size_t size);\n")
    hstream.print("extern void DeliteCudaMemcpyHtoDAsync(void *dptr, void *sptr, size_t size);\n")
    hstream.print("extern void DeliteCudaMemcpyDtoHAsync(void *dptr, void *sptr, size_t size);\n")
    hstream.print("typedef jboolean jbool;\n")              // TODO: Fix this
    hstream.print("typedef jbooleanArray jboolArray;\n\n")  // TODO: Fix this
  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    if(syms.length != 1) {
      println("TODO: implement cuda gen for fat exps!")
      throw new GenerationFailedException("TODO: implement cuda gen for fat exps!")
    }
    
    val List(sym) = syms

    // Conditions for not generating CUDA kernels (may be relaxed later)
    if((!isObjectType(sym.Type)) && (remap(sym.Type)!="void")) throw new GenerationFailedException("CudaGen: Not GPUable")
    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("CudaGen: Not GPUable")

    // Initialize global variables
    useLocalVar = false
    cudaVarMap.clear
    indexMap.clear

    currDim = 0
    xDimList.clear
    yDimList.clear
	  multDimInputs.clear

    helperFuncString.clear
    kernelSymbol = sym
    MetaData.init
    tabWidth = 1
    devFuncString = new StringBuilder

    gpuInputs = vals
    //gpuInputsStr = vals.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    gpuOutputs = Nil
    //gpuOutputs = if(remap(sym.Type)!= "void") List(sym) else Nil
    //gpuOutputStr = vals.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    gpuTemps = Nil
    //gpuTempsStr = ""
  }

  /****************************************
   *  Methods for managing GPUable Types
   *  **************************************/

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

  // Check the type and generate Exception if the type is not GPUable
  def checkGPUableType[A](m: Manifest[A]) : Unit = {
    if(!isGPUableType(m))
      throw new GenerationFailedException("CudaGen: Type %s is not a GPUable Type.".format(m.toString))
  }

  // All the types supported by CUDA Generation
  def isGPUableType[A](m : Manifest[A]) : Boolean = {
    if(!isObjectType(m) && !isPrimitiveType(m) && !isVoidType(m))
      false
    else
      true
  }

  override def remap[A](m: Manifest[A]) : String = {
    checkGPUableType(m)
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

  def emitConstDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }

  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+ remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }
  
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    val List(sym) = syms // TODO

    val out = new StringBuilder

    out.append("#include <cuda.h>\n\n")
    out.append(getDSLHeaders)

    val paramStr = (gpuOutputs:::gpuInputs:::gpuTemps).map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    //paramStr.append( (gpuOutputs:::gpuInputs:::gpuTemps).map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ") )
    //if(gpuOutputs.length>0) paramStr.append(gpuOutputStr)
    //if(gpuInputs.length>0) paramStr.append(","+gpuInputsStr)
    //if(gpuTemps.length>0) paramStr.append(","+gpuTemps.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", "))

    out.append("__global__ void kernel_%s(%s) {\n".format(quote(sym), paramStr))
    out.append(addTab()+"int idxX = blockIdx.x*blockDim.x + threadIdx.x;\n")
    out.append(addTab()+"int idxY = blockIdx.y*blockDim.y + threadIdx.y;\n")
    for(in <- multDimInputs) {
      out.append(addTab()+positionMultDimInputs(in))
    }
    stream.print(out.toString)
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    val List(sym) = syms // TODO
    
    tabWidth -= 1
    stream.println("}")
      
	  //if(MetaData.gpuOutput == "") { throw new GenerationFailedException("CudaGen:No output for GPU")}

    // Emit input copy helper functions for object type inputs
    for(v <- vals) {
      helperFuncString.append(emitCopyInputHtoD(v, sym))
      helperFuncString.append(emitCopyMutableInputDtoH(v, sym))
    }

    // Emit kerenl size calculation helper functions
    helperFuncString.append(emitSizeFuncs(sym))

    // Print out to file stream
    hstream.print(helperFuncString)
    hstream.flush

    // Print out dsl.h file
	  if(!kernelsList.contains(sym)) {
    	headerStream.println("#include \"%s.cu\"".format(quote(sym)))
		  kernelsList += sym
	  }
    headerStream.flush

    // Print out device function
    devStream.println(devFuncString)
    devStream.flush
  }

  /*******************************************************
   * Methods below are for emitting helper functions
   *******************************************************/
  // TODO: Change the metadata function names

  // For object type inputs, allocate GPU memory and copy from CPU to GPU.
  def emitCopyInputHtoD(sym: Sym[Any], ksym: Sym[Any]) : String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
	    helperFuncIdx += 1
      out.append("%s *copyInputHtoD_%s_%s_%s(%s) {\n".format(remap(sym.Type), quote(ksym), quote(sym),helperFuncIdx, "JNIEnv *env , jobject obj"))
      out.append(copyInputHtoD(sym))
      out.append("}\n")
      MetaData.gpuInputs.add("{\"%s\":[\"%s\",\"copyInputHtoD_%s_%s_%s\"".format(quote(sym),remap(sym.Type),quote(ksym),quote(sym),helperFuncIdx))
      out.toString
    }
    else ""
  }

  // For mutable inputs, copy the mutated datastructure from GPU to CPU after the kernel is terminated
  def emitCopyMutableInputDtoH(sym: Sym[Any], ksym: Sym[Any]): String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
	    helperFuncIdx += 1
      out.append("void copyMutableInputDtoH_%s_%s_%s(%s) {\n".format(quote(ksym), quote(sym), helperFuncIdx, "JNIEnv *env , jobject obj, "+remap(sym.Type)+" *"+quote(sym)))
      out.append(copyMutableInputDtoH(sym))
      out.append("}\n")
      MetaData.gpuInputs.add("\"copyMutableInputDtoH_%s_%s_%s\"]}".format(quote(ksym),quote(sym),helperFuncIdx))
      out.toString
    }
    else ""    
  }

  /* emitAllocFunc method emits code for allocating the output memory of a kernel,
       and copying  it to CPU memory with allocation of new object in CPU */
  //TODO: Separate output and temporary allocations
  def emitAllocFunc(sym:Sym[Any], allocFunc:Exp[Any]) {
    helperFuncIdx += 1
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString,true)

    // Need to save idx before calling emitBlock, which might recursively call this method
    val currHelperFuncIdx = helperFuncIdx

    // Get free variables
    val inputs = getFreeVarBlock(allocFunc,Nil)
    val paramStr = inputs.map(ele=>
			if(isObjectType(ele.Type)) remap(ele.Type) + " *_" + quote(ele)
			else remap(ele.Type) + " " + quote(ele)
	  ).mkString(",")

    /* Object type inputs of helper functions are pointers, but CUDA generators assume the actual objects,
           therefore need to dereference the objects before emitting the actual block contents. */
    val derefParams = inputs.map(ele=>
      if(isObjectType(ele.Type)) "\t%s %s = *_%s;\n".format(remap(ele.Type),quote(ele),quote(ele))
      else ""
    ).mkString("")

    // Generate allocation helper function
    tempString.append("%s *allocFunc_%s(%s) {\n".format(remap(allocFunc.Type),currHelperFuncIdx,paramStr))
    tempString.append(derefParams)
    emitBlock(allocFunc)(tempStream)
    tempString.append("\treturn %s;\n".format(quote(getBlockResult(allocFunc))))
    tempString.append("}\n")

    // Generate copy (D->H) helper function
    tempString.append("jobject copyOutputDtoH_%s(JNIEnv *env,%s) {\n".format(helperFuncIdx,remap(sym.Type)+" *"+quote(sym)))
    tempString.append(copyOutputDtoH(sym))
    tempString.append("}\n")

    // Register Metadata
    MetaData.gpuOutput = "{\"%s\":[\"%s\",\"allocFunc_%s\",[%s],\"copyOutputDtoH_%s\",[\"env\",\"%s\"]]}".format(quote(sym),remap(sym.Type),currHelperFuncIdx,inputs.map(quote(_)).mkString(","),currHelperFuncIdx,quote(sym))
    gpuOutputs = gpuOutputs :+ sym

    // Write to helper function string
    helperFuncString.append(tempString)
  }


  /**********************************************************
   * Calculation and Emission of GPU kernel size functions
   *********************************************************/

  //TODO: Get this information from the environment (Cuda device version)
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
  def emitSizeFuncs(sym: Sym[Any]): String = {
    helperFuncIdx += 1

    val out = new StringBuilder

    if((xDimList.size == 0) && (MetaData.gpuLibCall==""))
      throw new GenerationFailedException("CudaGen: No dimension specified for this kernel.")

    val inputs = (gpuOutputs ::: gpuInputs ::: gpuTemps)
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
    MetaData.gpuBlockSizeX = "[\"gpuBlockSizeX_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    out.append("int gpuBlockSizeY_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuBlockSizeY = "[\"gpuBlockSizeY_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    out.append("int gpuBlockSizeZ_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuBlockSizeZ = "[\"gpuBlockSizeZ_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    out.append("int gpuDimSizeX_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
	  if(xDimList.length==0)
    	out.append("\tint X = 1;\n")
	  else
    	out.append("\tint X = %s;\n".format(xDimList(xDimList.length-1)))
    out.append("\treturn 1+((X-1)/%s);\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    MetaData.gpuDimSizeX = "[\"gpuDimSizeX_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    out.append("int gpuDimSizeY_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
	  out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuDimSizeY = "[\"gpuDimSizeY_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    /*
    out.append("int gpuBlockSizeX_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append(emitCheckSize("X",xDimList))
    out.append("\tif(X < %s) return X;\n".format(MAX_THREADS_PER_BLOCK))
    out.append("\telse return %s;\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    MetaData.gpuBlockSizeX = "[\"gpuBlockSizeX_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    out.append("int gpuBlockSizeY_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\tint X = gpuBlockSizeX_%s_%s(%s);\n".format(quote(sym),helperFuncIdx,argInputStr))
    out.append(emitCheckSize("Y",yDimList))
	  out.append("\tif(Y == 1) return 1;\n")
    out.append("\telse if(X == %s) return 1;\n".format(MAX_THREADS_PER_BLOCK))
    out.append("\telse return (%s / X);\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    MetaData.gpuBlockSizeY = "[\"gpuBlockSizeY_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    out.append("int gpuBlockSizeZ_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuBlockSizeZ = "[\"gpuBlockSizeZ_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)

    out.append("int gpuDimSizeX_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
	  if(xDimList.length==0)
    	out.append("\tint X = 1;\n")
	  else
    	out.append("\tint X = %s;\n".format(xDimList(0)))
    out.append("\treturn 1+((X-1)/%s);\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    MetaData.gpuDimSizeX = "[\"gpuDimSizeX_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)
    
    out.append("int gpuDimSizeY_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
	  if(yDimList.length==0)
    	out.append("\tint Y = 1;")
	  else
    	out.append("\tint Y = %s;".format(yDimList(0)))
    out.append("\treturn 1+((Y-1)/%s);\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    MetaData.gpuDimSizeY = "[\"gpuDimSizeY_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)
    */
    out.toString
  }

  def emitLibCall(sym: Sym[Any], stmts: List[String]) : Unit = {
    val out = new StringBuilder

    if(sym == kernelSymbol) {
      // Emit code for library call function
      val inputs = (gpuOutputs ::: gpuInputs)
      val paramStr = inputs.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
      if(inputs.length != 0)
        out.append("void gpuLibCall_%s(%s,%s) {\n".format(quote(sym),paramStr,"cudaStream_t stream"))
      else
        out.append("void gpuLibCall_%s(%s) {\n".format(quote(sym),"cudaStream_t stream"))
      
      for(s <- stmts)
        out.append("\t"+s+"\n")
      out.append("}\n")
      helperFuncString.append(out.toString)

      // Add to metadata
      //MetaData.gpuLibCall = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStrTemp)
      MetaData.gpuLibCall = "\"gpuLibCall_%s\"".format(quote(sym))
    }
    else {
      throw new GenerationFailedException("CudaGen: Not GPUable (Only top-level node can use library call)")
    }

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
