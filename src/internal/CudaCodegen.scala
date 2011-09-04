package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap, ListMap, HashSet}
import collection.immutable.List._


trait CudaCodegen extends CLikeCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cu"
  override def toString = "cuda"

  /* Kernel input / output symbols */
  private var kernelInputs: List[Sym[Any]] = null
  private var kernelOutputs: List[Sym[Any]] = null
  def getKernelInputs = kernelInputs
  def setKernelInputs(syms: List[Sym[Any]]): Unit = { kernelInputs = syms }
  def getKernelOutputs = kernelOutputs
  def setKernelOutputs(syms: List[Sym[Any]]): Unit = { kernelOutputs = syms }
  def getKernelTemps = metaData.temps.toList.reverse.map(ele => ele._1)

  /* For using GPU local variables */
  final class CudaOptimizer {
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
  var optimizer: CudaOptimizer = null

  /*
  var useLocalVar:Boolean = false
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
  */

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

  var tabWidth:Int = 0
  def addTab():String = "\t"*tabWidth
  
  var forceParallel = false

  var helperFuncString:StringBuilder = null
  var hstream: PrintWriter = null
  var devFuncString:StringBuilder = null
  var devFuncIdx = 0
  var devStream: PrintWriter = null
  var headerStream: PrintWriter = null


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

    val inputs = (getFreeVarBlock(func,Nil).filterNot(ele => locals.contains(ele))++getKernelTemps).distinct
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


  // MetaData
  override def hasMetaData: Boolean = true
  override def getMetaData: String = metaData.toString
  var metaData: CudaMetaData = null

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
  final class CudaMetaData {
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
      out.append("\"gpuInputs\":["+inputs.toList.reverse.map(in=>"{\""+quote(in._1)+"\":[\""+remap(in._1.Type)+"\",\""+in._2.funcHtoD+"\",\""+in._2.funcDtoH+"\"]}").mkString(",")+"],")
      out.append("\"gpuOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.Type)+"\",\""+out._2.funcHtoD+"\","+"["+ out._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"]"+",\""+out._2.funcDtoH+"\"]}").mkString(",")+"],")
      out.append("\"gpuTemps\":["+temps.toList.reverse.map(temp=>"{\""+quote(temp._1)+"\":[\""+remap(temp._1.Type)+"\",\""+temp._2.funcHtoD+"\","+"["+ temp._2.argsFuncHtoD.map("\""+quote(_)+"\"").mkString(",")+"]"+",\""+temp._2.funcDtoH+"\"]}").mkString(",")+"]")

      //if(gpuLibCall != "") out.append(",\"gpuLibCall\":"+gpuLibCall.toString)
      out.append("}")
      out.toString
    }
  }

  // Exception Handler function
  override def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
     super.exceptionHandler(e, outFile, kstream)
     // TODO: Need to cleanup some data structures
  }

  override def initializeGenerator(buildDir:String): Unit = {

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
    // Set kernel input and output symbols
    setKernelInputs(vals)
    setKernelOutputs(syms)

    // Conditions for not generating CUDA kernels (may be relaxed later)
    for (sym <- syms) {
      if((!isObjectType(sym.Type)) && (remap(sym.Type)!="void")) throw new GenerationFailedException("CudaGen: Not GPUable output type : %s".format(remap(sym.Type)))
    }
    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("CudaGen: Not GPUable input/output types: Variable")

    // Initialize global variables
    //useLocalVar = false
    //cudaVarMap.clear
    optimizer = new CudaOptimizer

    currDim = 0
    xDimList.clear
    yDimList.clear
    multDimInputs.clear

    helperFuncString.clear
    metaData = new CudaMetaData
    //MetaData.init
    tabWidth = 1
    devFuncString = new StringBuilder

    forceParallel = false
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

  def isVariableType[A](m: Manifest[A]) : Boolean = {
    if(m.erasure == classOf[Variable[AnyVal]]) true
    else false
  }

  // Check the type and generate Exception if the type is not GPUable
  def checkGPUableType[A](m: Manifest[A]) : Unit = {
    if(!isGPUableType(m))
      throw new GenerationFailedException("CudaGen: Type %s is not a GPUable Type.".format(m.toString))
  }

  // All the types supported by CUDA Generation
  def isGPUableType[A](m : Manifest[A]) : Boolean = {
    if(!isObjectType(m) && !isPrimitiveType(m) && !isVoidType(m) && !isVariableType(m))
      false
    else
      true
  }

  override def remap[A](m: Manifest[A]) : String = {
    checkGPUableType(m)
    if (m.erasure == classOf[Variable[AnyVal]])
      remap(m.typeArguments.head)
    else {
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

  def cloneObject(sym: Sym[Any], src: Sym[Any]) : String = {
    throw new GenerationFailedException("CudaGen: cloneObject(sym)")
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

/*
  //TODO: is sym of type Any or Variable[Any] ?
  def emitConstDef(sym: Sym[Any], rhs: emitK)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }
*/

  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab()+ remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }
  
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    if (external) {
      // CUDA library ops use a C wrapper, so should be generated as a C kernel
      stream.println(getDSLHeaders)
      super.emitKernelHeader(syms, getKernelOutputs ::: vals, vars, resultType, resultIsVar, external)
      return
    }

    val List(sym) = syms // TODO

    val out = new StringBuilder

    out.append("#include <cuda.h>\n\n")
    out.append(getDSLHeaders)

    val paramStr = (getKernelOutputs++getKernelInputs++getKernelTemps).map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")

    out.append("__global__ void kernel_%s(%s) {\n".format(quote(sym), paramStr))
    out.append(addTab()+"int idxX = blockIdx.x*blockDim.x + threadIdx.x;\n")
    out.append(addTab()+"int idxY = blockIdx.y*blockDim.y + threadIdx.y;\n")
    for(in <- multDimInputs) {
      out.append(addTab()+positionMultDimInputs(in))
    }
    stream.print(out.toString)
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    if (external) {
      super.emitKernelFooter(syms, vals, vars, resultType, resultIsVar, external)
      //return
    }
    else {
      stream.println("}")
    }

    // aks TODO: the rest of this stuff adds to metadata and seems necessary even if we are external.
    // should probably be refactored...
    tabWidth -= 1

    // Emit input copy helper functions for object type inputs
    for(v <- vals if isObjectType(v.Type)) {
      helperFuncString.append(emitCopyInputHtoD(v, syms, copyInputHtoD(v)))
      helperFuncString.append(emitCopyMutableInputDtoH(v, syms, copyMutableInputDtoH(v)))
    }

    // Emit kernel size calculation helper functions
    helperFuncString.append(emitSizeFuncs(syms, external))

    // Print helper functions to file stream
    hstream.print(helperFuncString)
    hstream.flush

    // Print out dsl.h file
    if(kernelsList.intersect(syms).isEmpty) {
      headerStream.println("#include \"%s.cu\"".format(syms.map(quote).mkString("")))
      kernelsList ++= syms
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
  def emitAllocFunc(sym:Sym[Any], allocFunc:Exp[Any]) {
    helperFuncIdx += 1
    val tempString = new StringWriter
    val tempString2 = new StringWriter
    val tempStream = new PrintWriter(tempString,true)

    // Need to save idx before calling emitBlock, which might recursively call this method
    val currHelperFuncIdx = helperFuncIdx

    // Get free variables
    val inputs = getFreeVarBlock(allocFunc,Nil)

    // Get the body (string) of the allocation function in tempString
    emitBlock(allocFunc)(tempStream)
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
  def emitSizeFuncs(syms: List[Sym[Any]], external: Boolean): String = {
    val sym = syms(0)  //TODO: Fix
    helperFuncIdx += 1

    val out = new StringBuilder

    if(xDimList.size == 0 && !external)
      throw new GenerationFailedException("CudaGen: No dimension specified for this kernel.")

    val inputs = (getKernelOutputs++getKernelInputs++getKernelTemps)
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
