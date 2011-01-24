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

  /*
   * This flag indicates whether current codegen is for parallel execution or not
   */
  var parallelCudagen = false

  var helperFuncIdx = 0
  var kernelsList = ListBuffer[Exp[_]]()

  var kernelSymbol:Sym[_] = null
  var parallelFor = false
  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth
  val varLink = HashMap[Exp[_], List[Exp[_]]]()
  
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

  def emitDevFunc(func:Exp[Any], outType: Manifest[_], inputs:List[Exp[Any]]):String = {
      devFuncIdx += 1
      val currIdx = devFuncIdx
      val tempString = new StringWriter
      val tempStream = new PrintWriter(tempString, true)
      val paramStr = inputs.map(ele=>remap(ele.Type)+" "+quote(ele)).mkString(",")
      val currentTab = tabWidth
      if(outType != null)
        tempStream.println("__device__ %s dev_%s(%s) {".format(remap(outType),currIdx,paramStr))
       else
        tempStream.println("__device__ void dev_%s(%s) {".format(currIdx,paramStr))
      tabWidth = 1
      emitBlock(func)(tempStream)
      if(outType != null)
        tempStream.println(addTab()+"return %s;".format(quote(getBlockResult(func))))
      tempStream.println("}")
      tabWidth = currentTab
      devFuncString.append(tempString)
      "dev_"+currIdx
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
    parallelCudagen = true
    helperFuncString.clear
    varLink.clear
    kernelSymbol = sym
    MetaData.init
    tabWidth = 1
    parallelFor = true
    devFuncString = new StringBuilder

    gpuBlockSizeX = null
    gpuBlockSizeY = null
    gpuBlockSizeZ = null

    gpuInputs = vals
    //gpuInputsStr = vals.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    gpuOutputs = if(remap(sym.Type)!= "void") List(sym) else Nil
    //gpuOutputStr = vals.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    gpuTemps = Nil
    //gpuTempsStr = ""
  }

  // Add variable links across IR nodes
  def addVarLink(from:Exp[_], to:Exp[_]): Unit = {
    if(varLink.contains(to)) {
      val list = varLink.get(to).get
      val newList = from +: list
      varLink.remove(to)
      varLink.put(from,newList)
    }
    else {
      val newList = List[Exp[_]](from,to)
      varLink.put(from,newList)
    }
  }

  def removeVarLink(from:Exp[_], to:Exp[_]): Unit = {
    if(varLink.contains(from)) {
      val newList = varLink.get(from).get.tail
      varLink.remove(from)
      varLink.put(to,newList)
    }
  }

  def getVarLink(sym:Exp[_]): Exp[_] = {
    if(sym.isInstanceOf[Sym[_]] && varLink.contains(sym)) {
      val out = varLink.get(sym).get.last
      out
    }
    else
      null
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
    //out.append(addTab()+"int idxY = blockIdx.y*blockDim.y + threadIdx.y;")
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

  //TODO: Currently only assume 1D kernel. 2D/3D kernel needs to be utilized.
  var gpuBlockSizeX:String = null
  var gpuBlockSizeY:String = null
  var gpuBlockSizeZ:String = null

  // Prints out the helper functions for getting the threadBlcok size and grid size
  def emitSizeFuncs(sym: Sym[_]): String = {
	helperFuncIdx += 1

    val out = new StringBuilder
    //if (gpuBlockSizeX == null) gpuBlockSizeX = "1"
    if ((gpuBlockSizeX==null) && (MetaData.gpuLibCall=="")) throw new GenerationFailedException("GPU Codegen: gpuBlockSizeX is not set")
    if (gpuBlockSizeX == null) gpuBlockSizeX = "1"
    if (gpuBlockSizeY == null) gpuBlockSizeY = "1"
    if (gpuBlockSizeZ == null) gpuBlockSizeZ = "1"

    val inputs = (gpuOutputs ::: gpuInputs ::: gpuTemps)
    val paramStr = inputs.map(ele=>
			if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele)
			else remap(ele.Type) + " " + quote(ele)
	).mkString(",")
    val argStr = inputs.map("\""+quote(_)+"\"").mkString(",")
    
    out.append("int gpuBlockSizeX_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\tif(%s < 512) return %s;\n".format(gpuBlockSizeX, gpuBlockSizeX))
    out.append("\telse return 512;\n")
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
    out.append("\treturn 1+((%s-1)/512);\n".format(gpuBlockSizeX))
    out.append("}\n")
    MetaData.gpuDimSizeX = "[\"gpuDimSizeX_%s_%s\",[%s]]".format(quote(sym),helperFuncIdx,argStr)
    
    out.append("int gpuDimSizeY_%s_%s(%s) {\n".format(quote(sym),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
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
