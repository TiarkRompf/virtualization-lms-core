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

  var isGPUable = true
  var parallelFor = true
  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth
  
  var gpuInputs:List[Sym[_]] = Nil
  var gpuOutput: Sym[_] = null
  val gpuTemps:List[Sym[_]] = Nil
  var gpuInputsStr = ""
  var gpuOutputStr = ""

  var gpuAllocString:StringWriter = null
  var gpuAllocStream:PrintWriter = null

  // FileWriter for helper functions(TODO: Change to get from Config)
  val buildPath = "generated/delite-gen/cuda/"
  val outDir = new File(buildPath); outDir.mkdirs()
  val hstream = new PrintWriter(new FileWriter(buildPath + "helperFuncs.cpp"))

  hstream.println("#include \"VectorImpl.h\"")
  hstream.println("#include \"MatrixImpl.h\"")
  hstream.println("#include <jni.h>")

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
      out.append("\"gpuOutput\":"+gpuOutput+",")
      out.append("\"gpuTemps\":%s".format(gpuTemps.toString))
      out.toString
    }

  }
  override def getMetaData:String = { MetaData.toString }
  
  // DSL data structure functions
  // TODO: Where should these functions located?
  def matrixAllocFunc(sym: Sym[_], stream:PrintWriter): Unit = {
    stream.println("\t%s *%s = new %s();".format(CudaType(sym.Type.toString),quote(sym),CudaType(sym.Type.toString)))
    stream.println("\tjclass cls = env->GetObjectClass(obj);")
    stream.println("\tjmethodID mid_numRows = env->GetMethodID(cls,\"numRows\",\"()I\");")
    stream.println("\tjmethodID mid_numCols = env->GetMethodID(cls,\"numCols\",\"()I\");")
    stream.println("\t%s->numRows = %s;".format(quote(sym),"env->CallIntMethod(obj,mid_numRows)"))
    stream.println("\t%s->numCols = %s;".format(quote(sym),"env->CallIntMethod(obj,mid_numCols)"))
  }

  def vectorAllocFunc(sym: Sym[_], stream:PrintWriter): Unit = {
    stream.println("\t%s *%s = new %s();".format(CudaType(sym.Type.toString),quote(sym),CudaType(sym.Type.toString)))
    stream.println("\tjclass cls = env->GetObjectClass(obj);")
    stream.println("\tjmethodID mid_length = env->GetMethodID(cls,\"length\",\"()I\");")
    stream.println("\tjmethodID mid_is_row = env->GetMethodID(cls,\"is_row\",\"()Z\");")
    stream.println("\t%s->length = %s;".format(quote(sym),"env->CallIntMethod(obj,mid_length)"))
    stream.println("\t%s->is_row = %s;".format(quote(sym),"env->CallBooleanMethod(obj,mid_is_row)"))
  }

  def matrixCopyFunc(sym: Sym[_], stream:PrintWriter): Unit = {
    val typeStr = CudaType(sym.Type.typeArguments(0).toString)
    val numBytesStr = "%s->numRows * %s->numCols * sizeof(%s)".format(quote(sym),quote(sym),CudaType(sym.Type.typeArguments(0).toString))

    // Get data(array) from scala data structure
    //stream.println("\tjclass cls = env->GetObjectClass(obj);")
    stream.println("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");".format(JNITypeDescriptor(sym.Type.typeArguments(0).toString)))
    stream.println("\tj%sArray data = (j%sArray)(%s);".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    stream.println("\tj%s *dataPtr = env->GetPrimitiveArrayCritical(data,0);".format(typeStr))

    // Allocate pinned-memory and device memory
    stream.println("\tvoid *hostPtr;")
    stream.println("\tDeliteCudaMallocHost(%s,%s);".format("&hostPtr",numBytesStr))
    stream.println("\tvoid *devPtr;")
    stream.println("\tDeliteCudaMalloc(%s,%s);".format("&devPtr",numBytesStr))

    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    stream.println("\tmemcpy(%s, %s, %s);".format("hostPtr","dataPtr",numBytesStr))
    stream.println("\tDeliteCudaMemcpyAsync(%s, %s, %s, %s);".format("devPtr","hostPtr",numBytesStr,"cudaMemcpyHostToDevice"))

    // Store the device pointer to the C data structure
    stream.println("\t%s->data = %s;".format(quote(sym),"devPtr"))

    // Release
    stream.println("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);")

  }

  def vectorCopyFunc(sym: Sym[_], stream:PrintWriter): Unit = {
    val typeStr = CudaType(sym.Type.typeArguments(0).toString)
    val numBytesStr = "%s->length * sizeof(%s)".format(quote(sym),CudaType(sym.Type.typeArguments(0).toString))

    // Get data(array) from scala data structure
    //stream.println("\tjclass cls = env->GetObjectClass(obj);")
    stream.println("\tjmethodID mid_data = env->GetMethodID(cls,\"data\",\"()[%s\");".format(JNITypeDescriptor(sym.Type.typeArguments(0).toString)))
    stream.println("\tj%sArray data = (j%sArray)(%s);".format(typeStr,typeStr,"env->CallObjectMethod(obj,mid_data)"))
    stream.println("\tj%s *dataPtr = env->GetPrimitiveArrayCritical(data,0);".format(typeStr))

    // Allocate pinned-memory and device memory
    stream.println("\tvoid *hostPtr;")
    stream.println("\tDeliteCudaMallocHost(%s,%s);".format("&hostPtr",numBytesStr))
    stream.println("\tvoid *devPtr;")
    stream.println("\tDeliteCudaMalloc(%s,%s);".format("&devPtr",numBytesStr))

    // Copy twice (hostMem->pinnedHostMem, pinnedHostMem->devMem)
    stream.println("\tmemcpy(%s, %s, %s);".format("hostPtr","dataPtr",numBytesStr))
    stream.println("\tDeliteCudaMemcpyAsync(%s, %s, %s, %s);".format("devPtr","hostPtr",numBytesStr,"cudaMemcpyHostToDevice"))

    // Store the device pointer to the C data structure
    stream.println("\t%s->data = %s;".format(quote(sym),"devPtr"))

    // Release
    stream.println("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);")

  }

  // HashMap for Type Conversions : scalaType -> (CudaType,isObjectType)
  val TypeTable = HashMap[String,(String,Boolean,(Sym[_],PrintWriter)=>Unit,(Sym[_],PrintWriter)=>Unit)](
          "ppl.dsl.optiml.Matrix[Int]" -> ("Matrix<int>",true,matrixAllocFunc,matrixCopyFunc),
          "ppl.dsl.optiml.Matrix[Long]" -> ("Matrix<long>",true,matrixAllocFunc,matrixCopyFunc),
          "ppl.dsl.optiml.Matrix[Float]" -> ("Matrix<float>",true,matrixAllocFunc,matrixCopyFunc),
          "ppl.dsl.optiml.Matrix[Double]" -> ("Matrix<double>",true,matrixAllocFunc,matrixCopyFunc),
          "ppl.dsl.optiml.Matrix[Boolean]" -> ("Matrix<bool>",true,matrixAllocFunc,matrixCopyFunc),
          "ppl.dsl.optiml.Vector[Int]" -> ("Vector<int>",true,vectorAllocFunc,vectorCopyFunc),
          "ppl.dsl.optiml.Vector[Long]" -> ("Vector<long>",true,vectorAllocFunc,vectorCopyFunc),
          "ppl.dsl.optiml.Vector[Float]" -> ("Vector<float>",true,vectorAllocFunc,vectorCopyFunc),
          "ppl.dsl.optiml.Vector[Double]" -> ("Vector<double>",true,vectorAllocFunc,vectorCopyFunc),
          "ppl.dsl.optiml.Vector[Boolean]" -> ("Vector<bool>",true,vectorAllocFunc,vectorCopyFunc),
          "Int" -> ("int",false,null,null),
          "Long" -> ("long",false,null,null),
          "Float" -> ("float",false,null,null),
          "Double" -> ("double",false,null,null),
          "Boolean" -> ("bool",false,null,null)
  )

  
  def JNITypeDescriptor(scalaType:String) : String = {
    scalaType match {
      case "Int" => "I"
      case "Long" => "J"
      case "Float" => "F"
      case "Double" => "D"
      case "Boolean" => "Z"
      case _ => throw new Exception(scalaType + " is not a valid type for Cuda.")
    }
  }

  def CudaType(scalaType: String): String = TypeTable.get(scalaType) match {
    case Some(cudaType) => cudaType._1
    case None => throw new Exception(scalaType + " is not a valid type for Cuda.")
  }

  def isObjectType(scalaType: String): Boolean = TypeTable.get(scalaType) match {
    case Some(cudaType) => cudaType._2
    case None => throw new Exception(scalaType + " is not a valid type for Cuda.")
  }

  def allocDataStructure(sym: Sym[_], stream:PrintWriter) : Unit = {
    val scalaType = sym.Type.toString
    if(isObjectType(scalaType)) {
      TypeTable.get(scalaType).get._3(sym, stream)
    }
  }

  def copyDataStructure(sym: Sym[_], stream:PrintWriter) : Unit = {
    val scalaType = sym.Type.toString
    if(isObjectType(scalaType)) {
      TypeTable.get(scalaType).get._4(sym, stream)
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

    //stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("int main(int argc, char** argv) {")

    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))

    //stream.println("}")
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
    stream.println(addTab()+"no_type" + " " + quote(sym) + " = " + rhs + ";")
  }
  
  def emitValDef(tpe: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+tpe + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+"no_type" + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(tpe: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+tpe + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }
  
  override def emitKernelHeader(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {

    if(!isObjectType(sym.Type.toString)) throw new RuntimeException("CudaGen: Not GPUable")
    if((vars.length > 0)  || (resultIsVar)) throw new RuntimeException("CudaGen: Not GPUable")

    MetaData.init

    gpuBlockSizeX = null
    gpuBlockSizeY = null
    gpuBlockSizeZ = null

    tabWidth = 0
    
    // Initialize datastructures
    if(gpuAllocString != null) gpuAllocString.close
    if(gpuAllocStream != null) gpuAllocStream.close
    gpuAllocString = new StringWriter()
    gpuAllocStream = new PrintWriter(gpuAllocString)

    parallelFor = true

    gpuInputs = vals
    gpuInputsStr = vals.map(ele=>CudaType(ele.Type.toString) + " " + quote(ele)).mkString(", ")
    gpuOutput = sym
    gpuOutputStr = CudaType(sym.Type.toString) + " " + quote(sym)

    stream.println("#include <cuda.h>")
    stream.println("#include \"VectorImpl.h\"")
    stream.println("#include \"MatrixImpl.h\"")
    stream.println("")

    stream.println("__global__ void gpuKernel_%s(%s,%s) {".format(quote(sym), gpuOutputStr ,gpuInputsStr))
    tabWidth += 1
    stream.println(addTab()+"int idxX = blockIdx.x*blockDim.x + threadIdx.x;")
    //stream.println(addTab()+"int idxY = blockIdx.y*blockDim.y + threadIdx.y;")


  }

  override def emitKernelFooter(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    tabWidth -= 1
    stream.println("}")

    // Emit input copy helper functions for val type object inputs
    vals.map(emitAllocCopy(_, sym, hstream))

    // From this point, all the inputs to these helper functions are the C land data structures
    // Emit allocation helper functions
    hstream.println(gpuAllocString)

    // Emit size helper functions
    emitSizeFuncs(sym, hstream)
  }

  override def exceptionHandler(outFile:File, kstream:PrintWriter): Unit = {
     super.exceptionHandler(outFile, kstream)
     // TODO: Need to cleanup some data structures
  }

  //TODO: Currently only assume 1D kernel. 2D/3D kernel needs to be utilized.
  var gpuBlockSizeX:String = null
  var gpuBlockSizeY:String = null
  var gpuBlockSizeZ:String = null

  // Prints out the helper functions for getting the threadBlcok size and grid size
  def emitSizeFuncs(sym: Sym[_], stream: PrintWriter): Unit = {
    if (gpuBlockSizeX == null) gpuBlockSizeX = "1"
    if (gpuBlockSizeY == null) gpuBlockSizeY = "1"
    if (gpuBlockSizeZ == null) gpuBlockSizeZ = "1"

    stream.println("int gpuBlockSizeX_%s(%s) {".format(quote(sym),gpuInputsStr))
    stream.println("\tif(%s < 512) return %s;".format(gpuBlockSizeX, gpuBlockSizeX))
    stream.println("\telse return 512;")
    stream.println("}")
    MetaData.gpuBlockSizeX = "\"gpuBlockSizeX_%s(%s)\"".format(quote(sym),gpuInputs.map(quote).mkString(","))

    stream.println("int gpuBlockSizeY_%s(%s) {".format(quote(sym),gpuInputsStr))
    stream.println("\treturn 1;")
    stream.println("}")
    MetaData.gpuBlockSizeY = "\"gpuBlockSizeY_%s(%s)\"".format(quote(sym),gpuInputs.map(quote).mkString(","))

    stream.println("int gpuBlockSizeZ_%s(%s) {".format(quote(sym),gpuInputsStr))
    stream.println("\treturn 1;")
    stream.println("}")
    MetaData.gpuBlockSizeZ = "\"gpuBlockSizeZ_%s(%s)\"".format(quote(sym),gpuInputs.map(quote).mkString(","))

    stream.println("int gpuDimSizeX_%s(%s) {".format(quote(sym),gpuInputsStr))
    stream.println("\treturn 1+((%s-1)/512);".format(gpuBlockSizeX))
    stream.println("}")
    MetaData.gpuDimSizeX = "\"gpuDimSizeX_%s(%s)\"".format(quote(sym),gpuInputs.map(quote).mkString(","))
    
    stream.println("int gpuDimSizeY_%s(%s) {".format(quote(sym),gpuInputsStr))
    stream.println("\treturn 1;")
    stream.println("}")
    MetaData.gpuDimSizeY = "\"gpuDimSizeY_%s(%s)\"".format(quote(sym),gpuInputs.map(quote).mkString(","))

    stream.flush
  }

  def emitAllocCopy(sym: Sym[_], ksym: Sym[_], stream: PrintWriter) : Unit = {
    // Only emit copy functions for object type arguments
    if(isObjectType(sym.Type.toString)) {

      stream.println("%s gpuMemAllocAndCopy_%s_%s(%s,%s) {".format(CudaType(sym.Type.toString), quote(ksym), quote(sym),"JNIEnv *env", "jobject obj"))

      // Create C data structure
      allocDataStructure(sym, stream)

      // Copy from Scala to C
      copyDataStructure(sym, stream)

      stream.println("\treturn *%s;".format(quote(sym)))
      stream.println("}")

      stream.flush

      //Register MetaData
      //MetaData.gpuInputs.add("{\"%s\":{\"%s\":\"gpuMemAlloc_%s(%s)\"}}".format(quote(sym),CudaType(sym.Type.toString),quote(sym),gpuInputs.map(quote).mkString(",")))
      MetaData.gpuInputs.add("{\"%s\":\"gpuMemAlloc_%s(%s)\"}}".format(quote(sym),quote(sym),"env, obj"))
    }
  }

  def emitAlloc(sym: Sym[_], stmts: String) {

    gpuAllocStream.println("%s gpuMemAlloc_%s(%s) {".format(CudaType(sym.Type.toString),quote(sym),gpuInputsStr))
    gpuAllocStream.println(stmts)

    // TODO: Find out all the necessary IRs to generate this allocation  (needed?)
    //val listIR:List[TP[_]] = findDefinition(sym).get :: Nil
    //for(TP(lhs,rhs) <- listIR) {
      //emitNode(lhs,rhs)(gpuAllocStream)
    //}

    gpuAllocStream.println("}")
    gpuAllocStream.flush

    // Register to the metadata
    // TODO: Also add gpuTemps
    MetaData.gpuOutput = "{\"%s\":\"gpuMemAlloc_%s(%s)\"}".format(quote(sym),quote(sym),gpuInputs.map(quote).mkString(","))
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