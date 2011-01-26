package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap}

trait CudaCodegen extends CLikeCodegen with GenericCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cu"
  override def toString = "cuda"

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
    case _ => throw new GenerationFailedException("CudaGen: Maximum 2 dimensions for GPU kernels.")
  }
  def setCurrDimLength(length: String) {
    currDim match {
      case 0 => throw new RuntimeException("CudaGen: Cannot set dimension length when the dimension is 0.")
      case 1 => xDimList += length
      case 2 => yDimList += length
      case _ => throw new GenerationFailedException("CudaGen: Maximum 2 dimensions for GPU kernels.")
    }
  }
  val multDimInputs = ListBuffer[Sym[_]]()

  var helperFuncIdx = 0
  var kernelsList = ListBuffer[Exp[_]]()

  var kernelSymbol:Sym[_] = null
  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth
  
  var gpuInputs:List[Sym[_]] = Nil
  var gpuOutputs:List[Sym[_]] = Nil
  var gpuTemps:List[Sym[_]] = Nil
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

  override def generatorInit(build_dir:String): Unit = {
    // FileWriter for helper functions(TODO: Change to get from Config)
    //val outDir = new File(buildPath); outDir.mkdirs()
	helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(build_dir + "helperFuncs.cu"))
    devStream = new PrintWriter(new FileWriter(build_dir+"devFuncs.cu"))
    headerStream = new PrintWriter(new FileWriter(build_dir + "dsl.h"))
    headerStream.println("#include \"helperFuncs.cu\"")
    headerStream.println("#include \"devFuncs.cu\"")

    //TODO: Put all the DELITE APIs declarations somewhere
    hstream.print(getDSLHeaders)
    hstream.print("#include <iostream>\n")
    hstream.print("#include <limits>\n")
    hstream.print("#include <cublas.h>\n\n")
    hstream.print("#include <jni.h>\n\n")
    hstream.print("//Delite Runtime APIs\n")
    hstream.print("extern void DeliteCudaMallocHost(void **ptr, int size);\n")
    hstream.print("extern void DeliteCudaMalloc(void **ptr, int size);\n")
    hstream.print("extern void DeliteCudaMemcpyHtoDAsync(void *dptr, void *sptr, int size);\n")
    hstream.print("extern void DeliteCudaMemcpyDtoHAsync(void *dptr, void *sptr, int size);\n")
    hstream.print("typedef jboolean jbool;\n")              // TODO: Fix this
    hstream.print("typedef jbooleanArray jboolArray;\n\n")  // TODO: Fix this
  }

  override def kernelInit(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean): Unit = {
    // Conditions for not generating CUDA kernels (may be relaxed later)
    if((!isObjectType(sym.Type)) && (remap(sym.Type)!="void")) throw new GenerationFailedException("CudaGen: Not GPUable")
    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("CudaGen: Not GPUable")

    // Initialize global variables
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

  // Map scala primitive type to JNI type descriptor
  def JNITypeDescriptor(m: Manifest[_]) : String = m.toString match {
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case "Boolean" => "Z"
    case _ => throw new GenerationFailedException("Undefined CUDA type")
  }

  def isObjectType(m: Manifest[_]) : Boolean = remap(m) match {
    case "int" => false
    case "long" => false
    case "float" => false
    case "double" => false
    case "bool" => false
    case "void" => false
    case _ => throw new GenerationFailedException("CudaGen: isObjectType(m) : Unknown data type (%s)".format(remap(m)))
  }

  def isPrimitiveType(m: Manifest[_]) : Boolean = remap(m) match {
    case "int" => true
    case "long" => true
    case "float" => true
    case "double" => true
    case "bool" => true
    case _ => false
  }

  def isVoidType(m: Manifest[_]) : Boolean = remap(m) match {
    case "void" => true
    case _ => false
  }

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Unit" => "void"
    case _ => throw new GenerationFailedException("CudaGen: remap(m) : Unknown data type (%s)".format(m.toString))
  }

  def copyDataStructureHtoD(sym: Sym[_]) : String = {
    throw new GenerationFailedException("CudaGen: copyDataStructureHtoD(sym) : Cannot copy to GPU device (%s)".format(remap(sym.Type)))
  }

  def copyDataStructureDtoH(sym: Sym[_]) : String = {
    throw new GenerationFailedException("CudaGen: copyDataStructureDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
  }

  def copyDataStructureDtoHBack(sym: Sym[_]) : String = {
    throw new GenerationFailedException("CudaGen: copyDataStructureDtoHBack(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
  }

  def allocOutput(newSym: Sym[_], sym: Sym[_]) : Unit = {
    throw new GenerationFailedException("CudaGen: allocOutput(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.Type)))
  }

  def allocReference(newSym: Sym[_], sym: Sym[_]) : Unit = {
    throw new GenerationFailedException("CudaGen: allocReference(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.Type)))
  }

  def positionMultDimInputs(sym: Sym[_]) : String = {
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

  def emitConstDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }

  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+ remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }
  
  override def emitKernelHeader(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
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

  override def emitKernelFooter(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    tabWidth -= 1
    stream.println("}")
      
	  //if(MetaData.gpuOutput == "") { throw new GenerationFailedException("CudaGen:No output for GPU")}

    // Emit input copy helper functions for object type inputs
    for(v <- vals) {
      helperFuncString.append(emitCopyHtoD(v, sym))
      helperFuncString.append(emitCopyDtoHBack(v, sym))
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
   * TODO: Needs to be moved to some place else
   * TODO: Factor out common framework functionality
   *******************************************************/

  // Generate allocation & copy functions for the DSL Type inputs (Host -> Device)
  def emitCopyHtoD(sym: Sym[_], ksym: Sym[_]) : String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
	  helperFuncIdx += 1
      out.append("%s *gpuMemAllocAndCopy_%s_%s_%s(%s) {\n".format(remap(sym.Type), quote(ksym), quote(sym),helperFuncIdx, "JNIEnv *env , jobject obj"))
      // Create C data structure and Copy from Scala to C
      out.append(copyDataStructureHtoD(sym))
      out.append("}\n")

      // Register MetaData
      //MetaData.gpuInputs.add("{\"%s\":[\"%s\",\"gpuMemAllocAndCopy_%s_%s\",[%s]]}".format(quote(sym),remap(sym.Type),quote(ksym),quote(sym),"\"env\", \"obj\""))
      MetaData.gpuInputs.add("{\"%s\":[\"%s\",\"gpuMemAllocAndCopy_%s_%s_%s\"".format(quote(sym),remap(sym.Type),quote(ksym),quote(sym),helperFuncIdx))
      out.toString
    }
    else ""
  }

  // Generate copy functions for the DSL Type outputs (Device -> Host)
  def emitCopyDtoH(sym: Sym[_]): String = {
    val out = new StringBuilder
    out.append("jobject gpuMemCopy_%s_%s_%s(%s,%s) {\n".format(quote(kernelSymbol), quote(sym),helperFuncIdx,"JNIEnv *env", remap(sym.Type)+" *"+quote(sym)))
    out.append(copyDataStructureDtoH(sym))
    out.append("}\n")
    out.toString
  }

  def emitCopyDtoHBack(sym: Sym[_], ksym: Sym[_]): String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
	  helperFuncIdx += 1
      out.append("void gpuMemCopyBack_%s_%s_%s(%s) {\n".format(quote(ksym), quote(sym), helperFuncIdx, "JNIEnv *env , jobject obj, "+remap(sym.Type)+" *"+quote(sym)))
      out.append(copyDataStructureDtoHBack(sym))
      out.append("}\n")

      // Register MetaData
      MetaData.gpuInputs.add("\"gpuMemCopyBack_%s_%s_%s\"]}".format(quote(ksym),quote(sym),helperFuncIdx))
      out.toString
    }
    else ""    
  }


  /**********************************************************
   * Calculation and Emission of GPU kernel size functions
   *********************************************************/

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
  def emitSizeFuncs(sym: Sym[_]): String = {
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

    out.toString
  }

  def emitLibCall(sym: Sym[_], stmts: List[String]) : Unit = {
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


  def CudaConsts(x:Exp[_], s:String): String = {
    s match {
      case "Infinity" => "std::numeric_limits<%s>::max()".format(remap(x.Type))
      case _ => s
    }
  }
  
  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "NULL"
    case Const(z) => CudaConsts(x, z.toString)
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait CudaGenBase extends CudaCodegen {
  import IR._

}

trait CudaGenEffect extends CudaNestedCodegen with CudaGenBase {

}
