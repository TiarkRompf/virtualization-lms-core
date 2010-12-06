package scala.virtualization.lms
package internal

import collection.mutable.HashMap
import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList

trait CudaCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cu"
  override def toString = "cuda"

  var kernelSymbol:Sym[_] = null
  var parallelFor = true
  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth
  val varLink = HashMap[Sym[_],Sym[_]]()

  var gpuInputs:List[Sym[_]] = Nil
  var gpuOutput: Sym[_] = null
  var gpuTemps:List[Sym[_]] = Nil
  var gpuInputsStr = ""
  var gpuOutputStr = ""
  var gpuTempsStr = ""

  // FileWriter for helper functions(TODO: Change to get from Config)
  val helperFuncString:StringBuilder = new StringBuilder
  val buildPath = "generated/delite-gen/cuda/"
  val outDir = new File(buildPath); outDir.mkdirs()
  val hstream = new PrintWriter(new FileWriter(buildPath + "helperFuncs.cu"))

  //TODO: Put all the DELITE APIs declarations somewhere
  hstream.print("#include \"VectorImpl.h\"\n")
  hstream.print("#include \"MatrixImpl.h\"\n")
  hstream.print("#include <jni.h>\n\n")
  hstream.print("//Delite Runtime APIs\n")
  hstream.print("extern void DeliteCudaMallocHost(void **ptr, int size);\n")
  hstream.print("extern void DeliteCudaMalloc(void **ptr, int size);\n")
  hstream.print("extern void DeliteCudaMemcpyHtoDAsync(void *dptr, void *sptr, int size);\n")
  hstream.print("extern void DeliteCudaMemcpyDtoHAsync(void *dptr, void *sptr, int size);\n")
  hstream.print("typedef jboolean jbool;\n")              // TODO: Fix this 
  hstream.print("typedef jbooleanArray jboolArray;\n\n")  // TODO: Fix this

  // MetaData structure
  override def getMetaData:String = { MetaData.toString }
  object MetaData {
    var gpuBlockSizeX: String = ""
    var gpuBlockSizeY: String = ""
    var gpuBlockSizeZ: String = ""
    var gpuDimSizeX: String = ""
    var gpuDimSizeY: String = ""
    var gpuInputs: ArrayList[String] = new ArrayList[String]
    var gpuOutput: String = ""
    var gpuTemps: ArrayList[String] = new ArrayList[String]

    def init = {
      gpuBlockSizeX = ""
      gpuBlockSizeY = ""
      gpuBlockSizeZ = ""
      gpuDimSizeX = ""
      gpuDimSizeY = ""
      gpuInputs = new ArrayList[String]
      gpuOutput = ""
      gpuTemps = new ArrayList[String]
    }
    
    override def toString:String = {
      val out = new StringBuilder
      out.append("\"gpuBlockSizeX\":"+gpuBlockSizeX+",")
      out.append("\"gpuBlockSizeY\":"+gpuBlockSizeY+",")
      out.append("\"gpuBlockSizeZ\":"+gpuBlockSizeZ+",")
      out.append("\"gpuDimSizeX\":"+gpuDimSizeX+",")
      out.append("\"gpuDimSizeY\":"+gpuDimSizeY+",")
      out.append("\"gpuInputs\":%s".format(gpuInputs.toString)+",")
      if(gpuOutput == "") { println("ERROR:No Output for GPU?"); throw new Exception()}
      out.append("\"gpuOutput\":"+gpuOutput+",")
      out.append("\"gpuTemps\":%s".format(gpuTemps.toString))
      out.toString
    }
  }

  // Exception Handler function
  override def exceptionHandler(outFile:File, kstream:PrintWriter): Unit = {
     super.exceptionHandler(outFile, kstream)
     // TODO: Need to cleanup some data structures
  }

  override def init(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean): Unit = {
    // Conditions for not generating CUDA kernels (may be relaxed later)
    if(!isObjectType(sym.Type)) throw new RuntimeException("CudaGen: Not GPUable")
    if((vars.length > 0)  || (resultIsVar)) throw new RuntimeException("CudaGen: Not GPUable")

    // Initialize global variables
    helperFuncString.clear
    varLink.clear
    kernelSymbol = sym
    MetaData.init
    tabWidth = 1
    parallelFor = true

    gpuBlockSizeX = null
    gpuBlockSizeY = null
    gpuBlockSizeZ = null

    gpuInputs = vals
    gpuInputsStr = vals.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    gpuOutput = sym
    gpuOutputStr = remap(sym.Type) + " " + quote(sym)
    gpuTemps = Nil
    gpuTempsStr = ""
  }

  // Maps the scala type to cuda type
  // TODO: The remap has both framework types (primitive types) and DSL types. Need to separate this
  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "ppl.dsl.optiml.Matrix[Int]" => "Matrix<int>"
    case "ppl.dsl.optiml.Matrix[Long]" => "Matrix<long>"
    case "ppl.dsl.optiml.Matrix[Float]" => "Matrix<float>"
    case "ppl.dsl.optiml.Matrix[Double]" => "Matrix<double>"
    case "ppl.dsl.optiml.Matrix[Boolean]" => "Matrix<bool>"
    case "ppl.dsl.optiml.Vector[Int]" => "Vector<int>"
    case "ppl.dsl.optiml.Vector[Long]" => "Vector<long>"
    case "ppl.dsl.optiml.Vector[Float]" => "Vector<float>"
    case "ppl.dsl.optiml.Vector[Double]" => "Vector<double>"
    case "ppl.dsl.optiml.Vector[Boolean]" => "Vector<bool>"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case _ => throw new Exception("Undefined CUDA type")
  }

  // Map scala primitive type to JNI type descriptor
  def JNITypeDescriptor(m: Manifest[_]) : String = m.toString match {
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case "Boolean" => "Z"
    case _ => throw new Exception("Undefined CUDA type")
  }
  
  // HashMap for DSL Data structure information (map to specific functions)
  val DSLDataType = HashMap[String,(Sym[_]=>String,Sym[_]=>String,(Sym[_],Sym[_])=>Unit)](
    "ppl.dsl.optiml.Matrix[Int]" -> (matrixCopyHtoD,matrixCopyDtoH,emitMatrixAllocSym),
    "ppl.dsl.optiml.Matrix[Long]" -> (matrixCopyHtoD,matrixCopyDtoH,emitMatrixAllocSym),
    "ppl.dsl.optiml.Matrix[Float]" -> (matrixCopyHtoD,matrixCopyDtoH,emitMatrixAllocSym),
    "ppl.dsl.optiml.Matrix[Double]" -> (matrixCopyHtoD,matrixCopyDtoH,emitMatrixAllocSym),
    "ppl.dsl.optiml.Matrix[Boolean]" -> (matrixCopyHtoD,matrixCopyDtoH,emitMatrixAllocSym),
    "ppl.dsl.optiml.Vector[Int]" -> (vectorCopyHtoD,vectorCopyDtoH,emitVectorAllocSym),
    "ppl.dsl.optiml.Vector[Long]" -> (vectorCopyHtoD,vectorCopyDtoH,emitVectorAllocSym),
    "ppl.dsl.optiml.Vector[Float]" -> (vectorCopyHtoD,vectorCopyDtoH,emitVectorAllocSym),
    "ppl.dsl.optiml.Vector[Double]" -> (vectorCopyHtoD,vectorCopyDtoH,emitVectorAllocSym),
    "ppl.dsl.optiml.Vector[Boolean]" -> (vectorCopyHtoD,vectorCopyDtoH,emitVectorAllocSym)
  )

  def isObjectType(m: Manifest[_]): Boolean = {
    if(DSLDataType.contains(m.toString))
      true
    else
      false
  }

  def copyDataStructureHtoD(sym: Sym[_]) : String = {
    if(isObjectType(sym.Type)) {
      DSLDataType.get(sym.Type.toString).get._1(sym)
    }
    else ""
  }

  def copyDataStructureDtoH(sym: Sym[_]) : String = {
    if(isObjectType(sym.Type)) {
      DSLDataType.get(sym.Type.toString).get._2(sym)
    }
    else ""
  }

  def allocOutput(newSym: Sym[_], sym: Sym[_]) : Unit = {
    if(isObjectType(sym.Type)) {
      DSLDataType.get(sym.Type.toString).get._3(newSym, sym)
    }
    else {
      // TODO: What to do if this is called for primitive type?
    }
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

  def emitConstDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
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
  
  override def emitKernelHeader(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    
    stream.println("#include <cuda.h>")
    stream.println("#include \"VectorImpl.h\"")
    stream.println("#include \"MatrixImpl.h\"")
    stream.println("")

    val paramStr = new StringBuilder
    paramStr.append(gpuOutputStr)
    if(gpuInputs.length>0) paramStr.append(","+gpuInputsStr)
    if(gpuTemps.length>0) paramStr.append(","+gpuTemps.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", "))
    
    stream.println("__global__ void gpuKernel_%s(%s) {".format(quote(sym), paramStr.toString))

    stream.println(addTab()+"int idxX = blockIdx.x*blockDim.x + threadIdx.x;")
    //stream.println(addTab()+"int idxY = blockIdx.y*blockDim.y + threadIdx.y;")
  }

  override def emitKernelFooter(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    tabWidth -= 1
    stream.println("}")

    // Emit input copy helper functions for object type inputs
    for(v <- vals)
      helperFuncString.append(emitCopyHtoD(v, sym))

    // Emit kerenl size calculation helper functions
    helperFuncString.append(emitSizeFuncs(sym))

    // Print out to file stream
    hstream.print(helperFuncString)
    hstream.flush
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
      out.append("%s gpuMemAllocAndCopy_%s_%s(%s,%s) {\n".format(remap(sym.Type), quote(ksym), quote(sym),"JNIEnv *env", "jobject obj"))
      // Create C data structure and Copy from Scala to C
      out.append(copyDataStructureHtoD(sym))
      out.append("}\n")

      // Register MetaData
      MetaData.gpuInputs.add("{\"%s\":\"gpuMemAlloc_%s(%s)\"}".format(quote(sym),quote(sym),"env, obj"))
      out.toString
    }
    else ""
  }

  // Generate copy functions for the DSL Type outputs (Device -> Host)
  def emitCopyDtoH(sym: Sym[_]): String = {
    val out = new StringBuilder
    out.append("jobject gpuMemCopy_%s_%s(%s,%s) {\n".format(quote(kernelSymbol), quote(sym),"JNIEnv *env", remap(sym.Type)+" "+quote(sym)))
    out.append(copyDataStructureDtoH(sym))
    out.append("}\n")
    out.toString
  }

  def matrixCopyHtoD(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Get class, method ID and set the fields other than data
    out.append("\t%s %s;\n".format(remap(sym.Type),quote(sym)))
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");\n")
    out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");\n")
    out.append("\t%s.numRows = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numRows)"))
    out.append("\t%s.numCols = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_numCols)"))

    // Get data(array) from scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))

    // Allocate pinned-memory and device memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))

    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))

    // Store the device pointer to the C data structure
    out.append("\t%s.data = %s;\n".format(quote(sym),"devPtr"))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
    
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString

  }

  def vectorCopyHtoD(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Get class, method ID and set the fields other than data
    out.append("\t%s %s;\n".format(remap(sym.Type),quote(sym)))
    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");\n")
    out.append("\tjmethodID mid_is_row = env->GetMethodID(cls,\"is_row\",\"()Z\");\n")
    out.append("\t%s.length = %s;\n".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    out.append("\t%s.is_row = %s;\n".format(quote(sym),"env->CallBooleanMethod(obj,mid_is_row)"))

    // Get data(array) from scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))

    // Allocate pinned-memory and device memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))
    out.append("\t%s *devPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMalloc((void**)%s,%s);\n".format("&devPtr",numBytesStr))

    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    out.append("\tmemcpy(%s, %s, %s);\n".format("hostPtr","dataPtr",numBytesStr))
    out.append("\tDeliteCudaMemcpyHtoDAsync(%s, %s, %s);\n".format("devPtr","hostPtr",numBytesStr))

    // Store the device pointer to the C data structure
    out.append("\t%s.data = %s;\n".format(quote(sym),"devPtr"))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString

  }

  def matrixCopyDtoH(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.numRows * %s.numCols * sizeof(%s)".format(quote(sym),quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"ppl/dsl/optiml/MatrixImpl\");\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.numRows,%s.numCols);\n".format(quote(sym),quote(sym)))

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  def vectorCopyDtoH(sym: Sym[_]): String = {
    val out = new StringBuilder
    val typeStr = remap(sym.Type.typeArguments(0))
    val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(sym.Type.typeArguments(0)))

    // Allocate Scala object for the destination
    out.append("\tjclass cls = env->FindClass(\"ppl/dsl/optiml/VectorImpl\");\n")
    out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
    out.append("\tjobject obj = env->NewObject(cls,mid,%s.length,%s.is_row);\n".format(quote(sym),quote(sym)))

    // Allocate pinned-memory
    out.append("\t%s *hostPtr;\n".format(typeStr))
    out.append("\tDeliteCudaMallocHost((void**)%s,%s);\n".format("&hostPtr",numBytesStr))

    // Get data(array) of scala data structure
    out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");\n".format(JNITypeDescriptor(sym.Type.typeArguments(0))))
    out.append("\tj%sArray data = (j%sArray)(%s);\n".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(typeStr,typeStr))

    // Copy twice (devMem->pinnedHostMem, pinnedHostMem->hostMem)
    out.append("\tDeliteCudaMemcpyDtoHAsync(%s, %s.data, %s);\n".format("hostPtr",quote(sym),numBytesStr))
    out.append("\tmemcpy(%s, %s, %s);\n".format("dataPtr","hostPtr",numBytesStr))

    // Release
    out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")

    out.append("\treturn obj;\n")
    out.toString
  }

  //TODO: Currently only assume 1D kernel. 2D/3D kernel needs to be utilized.
  var gpuBlockSizeX:String = null
  var gpuBlockSizeY:String = null
  var gpuBlockSizeZ:String = null

  // Prints out the helper functions for getting the threadBlcok size and grid size
  def emitSizeFuncs(sym: Sym[_]): String = {
    val out = new StringBuilder
    if (gpuBlockSizeX == null) gpuBlockSizeX = "1"
    if (gpuBlockSizeY == null) gpuBlockSizeY = "1"
    if (gpuBlockSizeZ == null) gpuBlockSizeZ = "1"

    val inputs = (gpuOutput :: gpuInputs ::: gpuTemps)
    val paramStr = inputs.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
    val argStr = inputs.map(quote).mkString(",")
    
    out.append("int gpuBlockSizeX_%s(%s) {\n".format(quote(sym),paramStr))
    out.append("\tif(%s < 512) return %s;\n".format(gpuBlockSizeX, gpuBlockSizeX))
    out.append("\telse return 512;\n")
    out.append("}")
    MetaData.gpuBlockSizeX = "\"gpuBlockSizeX_%s(%s)\"".format(quote(sym),argStr)

    out.append("int gpuBlockSizeY_%s(%s) {\n".format(quote(sym),paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuBlockSizeY = "\"gpuBlockSizeY_%s(%s)\"".format(quote(sym),argStr)

    out.append("int gpuBlockSizeZ_%s(%s) {\n".format(quote(sym),paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuBlockSizeZ = "\"gpuBlockSizeZ_%s(%s)\"".format(quote(sym),argStr)

    out.append("int gpuDimSizeX_%s(%s) {\n".format(quote(sym),paramStr))
    out.append("\treturn 1+((%s-1)/512);\n".format(gpuBlockSizeX))
    out.append("}\n")
    MetaData.gpuDimSizeX = "\"gpuDimSizeX_%s(%s)\"".format(quote(sym),argStr)
    
    out.append("int gpuDimSizeY_%s(%s) {\n".format(quote(sym),paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuDimSizeY = "\"gpuDimSizeY_%s(%s)\"".format(quote(sym),argStr)

    out.toString
  }

  // Generate & register temporary data structures (which could be the output) for GPU kernel
  def emitVectorAlloc(newSym:Sym[_], length:String, isRow:String):Unit = {
    //TODO: Check if both symbols are Vectors

    //Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return

    val out = new StringBuilder

    val inputs = (gpuOutput :: gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val paramStr = inputs.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
    val argStr = inputs.map(quote).mkString(",")

    out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStr))
    out.append("\t%s %s;\n".format(remap(newSym.Type),quote(newSym)))
    out.append("\t%s *devPtr;\n".format(remap(newSym.Type.typeArguments(0))))
    out.append("\tDeliteCudaMalloc((void**)%s,%s*sizeof(%s));\n".format("&devPtr",length,remap(newSym.Type.typeArguments(0))))
    out.append("\t%s.length = %s;\n".format(quote(newSym),length))
    out.append("\t%s.is_row = %s;\n".format(quote(newSym),isRow))
    out.append("\t%s.data = devPtr;\n".format(quote(newSym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")

    helperFuncString.append(out.toString)

    // Register MetaData
    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"gpuMemAlloc_%s_%s(%s)\",\"gpuMemCopy_%s_%s(%s,%s)\"]}".format(quote(newSym),quote(kernelSymbol),quote(newSym),argStr,quote(kernelSymbol), quote(newSym), "env", quote(newSym))
      emitCopyDtoH(newSym)

    }
    else {
      MetaData.gpuTemps.add("{\"%s\":{\"%s\":\"gpuMemAlloc_%s_%s(%s)\"}}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStr))
      gpuTemps = newSym :: gpuTemps
    }
  }    
  def emitVectorAllocSym(newSym:Sym[_], sym:Sym[_]): Unit = {
    emitVectorAlloc(newSym, quote(sym)+".length", quote(sym)+".is_row")
  }

  def emitMatrixAlloc(newSym:Sym[_], numRows:String, numCols:String): Unit = {
    //TODO: Check if both symbols are Matrices

    //Do not add the same temporary if it already exists
    if(gpuTemps.contains(newSym)) return
    
    val out = new StringBuilder
    val inputs = (gpuOutput :: gpuInputs ::: gpuTemps) filterNot (_==newSym)
    val paramStr = inputs.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
    val argStr = inputs.map(quote).mkString(",")

    out.append("%s gpuMemAlloc_%s_%s(%s) {\n".format(remap(newSym.Type),quote(kernelSymbol),quote(newSym),paramStr))
    out.append("\t%s %s;\n".format(remap(newSym.Type),quote(newSym)))
    out.append("\t%s *devPtr;\n".format(remap(newSym.Type.typeArguments(0))))
    out.append("\tDeliteCudaMalloc((void**)%s,%s*%s*sizeof(%s));\n".format("&devPtr",numRows,numCols,remap(newSym.Type.typeArguments(0))))
    out.append("\t%s.numRows = %s;\n".format(quote(newSym),numRows))
    out.append("\t%s.numCols = %s;\n".format(quote(newSym),numCols))
    out.append("\t%s.data = devPtr;\n".format(quote(newSym)))
    out.append("\treturn %s;\n".format(quote(newSym)))
    out.append("}\n")
    helperFuncString.append(out.toString)

    // Register MetaData
    if(newSym == kernelSymbol) {
      MetaData.gpuOutput = "{\"%s\":[\"gpuMemAlloc_%s_%s(%s)\",\"gpuMemCopy_%s_%s(%s,%s)\"]}".format(quote(newSym),quote(kernelSymbol),quote(newSym),argStr,quote(kernelSymbol), quote(newSym), "env", quote(newSym))
      emitCopyDtoH(newSym)
    }
    else {
      MetaData.gpuTemps.add("{\"%s\":{\"%s\":\"gpuMemAlloc_%s_%s(%s)\"}}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStr))
      gpuTemps = newSym :: gpuTemps
    }
  }
  def emitMatrixAllocSym(newSym:Sym[_], sym:Sym[_]): Unit = {
    emitMatrixAlloc(newSym, quote(sym)+".numRows", quote(sym)+".numCols")
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

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait CudaGenBase extends CudaCodegen {
  import IR._

}

trait CudaGenEffect extends CudaNestedCodegen with CudaGenBase {

}