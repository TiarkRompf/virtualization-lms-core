package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap}
import collection.immutable.List._

trait OpenCLCodegen extends GPUCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cl"
  override def toString = "opencl"

  override def emitDevFunc(func:Exp[Any], locals:List[Exp[Any]]):(String,List[Exp[Any]]) = {
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
    header.append("%s dev_%s(%s) {\n".format(remap(func.Type),currIdx,paramStr))
    //header.append("\tint idxX = get_global_id(0);\n")
    if(remap(func.Type) != "void")
      footer.append("\treturn %s;\n".format(quote(getBlockResult(func))))
    footer.append("}\n")
    devFuncString.append(header)
    devFuncString.append(tempString)
    devFuncString.append(footer)

    ("dev_"+currIdx,inputs)
  }

  /*
  /* Indicates current dimension of work threads */
  var currDim = 0
  val xDimList =  ListBuffer[String]()
  val yDimList =  ListBuffer[String]()
  def getCurrDimStr():String = currDim match {
    case 0 => throw new RuntimeException("OpenCLGen: Cannot get current dimension string when the dimension is 0.")
    case 1 => "idxX"
    //case 2 => "idxY"
    case _ => throw new GenerationFailedException("OpenCLGen: Maximum 2 dimensions for GPU kernels.")
  }
  def getPrevDimStr():String = currDim match {
    case 0 => throw new RuntimeException("OpenCLGen: Cannot get previous dimension string when the dimension is 0.")
    case 1 => throw new RuntimeException("OpenCLGen: Cannot get previous dimension string when the dimension is 1.")
    case 2 => "idxX"
    //case 3 => "idxY"
    case _ => throw new GenerationFailedException("OpenCLGen: Maximum 1 dimensions for GPU kernels.")
  }
  def getNextDimStr():String = currDim match {
    case 0 => "idxX"
    //case 1 => throw new RuntimeException("OpenCLGen: Cannot get next dimension string when the dimension is 1.")
    case _ => throw new GenerationFailedException("OpenCLGen: Maximum 1 dimensions for GPU kernels.")
  }
  def setCurrDimLength(length: String) {
    currDim match {
      case 0 => throw new RuntimeException("OpenCLGen: Cannot set dimension length when the dimension is 0.")
      case 1 => xDimList += length
      //case 2 => yDimList += length
      case _ => throw new GenerationFailedException("OpenCLGen: Maximum 1 dimensions for GPU kernels.")
    }
  }
  val multDimInputs = ListBuffer[Sym[Any]]()

  var helperFuncIdx = 0
  var kernelsList = ListBuffer[Exp[Any]]()

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

  var enforcePar = false

  // MetaData structure
  override def hasMetaData: Boolean = true
  override def getMetaData: String = MetaData.toString

  override def emitDevFunc(func:Exp[Any], locals:List[Exp[Any]]):(String,List[Exp[Any]]) = {
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
    header.append("%s dev_%s(%s) {\n".format(remap(func.Type),currIdx,paramStr))
    //header.append("\tint idxX = get_global_id(0);\n")
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
    var gpuOutput: ArrayList[String] = new ArrayList[String]
    var gpuTemps: ArrayList[String] = new ArrayList[String]
    var gpuLibCall: String = ""

    def init = {
      gpuBlockSizeX = ""
      gpuBlockSizeY = ""
      gpuBlockSizeZ = ""
      gpuDimSizeX = ""
      gpuDimSizeY = ""
      gpuInputs = new ArrayList[String]
      gpuOutput = new ArrayList[String]
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
      out.append("\"gpuOutputs\":"+gpuOutput.toString+",")
      //if(gpuOutput != "")
      //  out.append("\"gpuOutput\":"+gpuOutput+",")
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
  */

  override def initializeGenerator(buildDir:String): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.h"))
    devStream = new PrintWriter(new FileWriter(buildDir+"devFuncs.cl"))
    headerStream = new PrintWriter(new FileWriter(buildDir + "dsl.h"))
    headerStream.println("#include \"helperFuncs.h\"")
    //headerStream.println("#include \"devFuncs.cu\"")

    //TODO: Put all the DELITE APIs declarations somewhere
    hstream.print(getDSLHeaders)
    hstream.print("#include <iostream>\n")
    hstream.print("#include <limits>\n")
    hstream.print("#include <jni.h>\n")
    hstream.print("#include <assert.h>\n")
    hstream.print("#include \"OpenCLList.h\"\n\n")
    hstream.print("//Delite Runtime APIs\n")
    hstream.print("extern void DeliteOpenCLMallocHost(void **ptr, size_t size);\n")
    hstream.print("extern void DeliteOpenCLMalloc(void **ptr, size_t size);\n")
    hstream.print("extern void DeliteOpenCLMemcpyHtoDAsync(void *dptr, void *sptr, size_t size);\n")
    hstream.print("extern void DeliteOpenCLMemcpyDtoHAsync(void *dptr, void *sptr, size_t size);\n")
    hstream.print("typedef jboolean jbool;\n")              // TODO: Fix this
    hstream.print("typedef jbooleanArray jboolArray;\n\n")  // TODO: Fix this
  }

  /*
  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {

    // Conditions for not generating OpenCL kernels (may be relaxed later)
    for(sym <- syms) {
      if((isPrimitiveType(sym.Type)) && (remap(sym.Type)!="void")) throw new GenerationFailedException("OpenCLGen: Not GPUable")
    }
    if((vars.length > 0)  || (resultIsVar)) throw new GenerationFailedException("OpenCLGen: Not GPUable")

    // Initialize global variables
    useLocalVar = false
    openclVarMap.clear
    indexMap.clear

    currDim = 0
    xDimList.clear
    yDimList.clear
	  multDimInputs.clear

    helperFuncString.clear
    MetaData.init
    tabWidth = 1
    devFuncString = new StringBuilder

    gpuInputs = vals
    gpuOutputs = Nil
    gpuTemps = Nil
  }
  */

  /****************************************
   *  Methods for managing GPUable Types
   *  **************************************/
  /*
  // Map a scala primitive type to JNI type descriptor
  def JNITypeDescriptor[A](m: Manifest[A]) : String = m.toString match {
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case "Boolean" => "Z"
    case _ => throw new GenerationFailedException("Undefined OpenCL type")
  }
  */

  override def isObjectType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "scala.collection.immutable.List[Int]" => true
        //TODO: ObjectTypes needs to be able to broken down, but array does not have to be.
        //TODO: What we need to do is to distinguish between passable types or not to the opencl kernel
      case "Array[Int]" | "Array[Long]" | "Array[Float]" | "Array[Double]" | "Array[Boolean]" => true
      case _ => super.isObjectType(m)
    }
  }
  /*
  def isPrimitiveType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Int" | "Long" | "Float" | "Double" | "Boolean"  => true
      case "scala.Tuple2[Int,Float]" => true // TODO: Is Tuple a primitive type?
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
      throw new GenerationFailedException("OpenCLGen: Type %s is not a GPUable Type.".format(m.toString))
  }

  // All the types supported by OpenCL Generation
  def isGPUableType[A](m : Manifest[A]) : Boolean = {
    if(!isObjectType(m) && !isPrimitiveType(m) && !isVoidType(m) && !isVariableType(m))
      false
    else
      true
  }
  */

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
          //TODO: How to not pass bool in general?
          case "Boolean" => "char"
          case "Unit" => "void"
          case "scala.Tuple2[Int,Float]" => "Tuple2_Int_Float"
          case "scala.collection.immutable.List[Int]" => "OpenCLIntList"  //TODO: Use C++ list
          //TODO: When retrieving the type arguments of array from Manifest works, then below can be simplified
          //case "Array[Int]" | "Array[Long]" | "Array[Float]" | "Array[Double]" | "Array[Boolean]" => remap(m.typeArguments(0)) + " *"
          //TODO: Is it appropriate to put global here?
          case "Array[Int]" => "__global int *"
          case "Array[Long]" => "__global long *"
          case "Array[Float]" => "__global float *"
          case "Array[Double]" => "__global double *"
          case "Array[Boolean]" => "__global char *"
          case _ => throw new Exception("OpenCLGen: remap(m) : GPUable Type %s does not have mapping table.".format(m.toString))
      }
    }
  }

  override def unpackObject[A](sym: Sym[Any]) : Map[String,Manifest[_]] = remap(sym.Type) match {
    case "OpenCLIntList" => Map("length"->Manifest.Int)    //TODO: How to initialize the data array type for the list?
    case _ => throw new GenerationFailedException("OpenCLGen: Type %s cannot be unpacked.".format(sym.Type.toString))
  }

  // TODO: Handle general C datastructure
  def copyInputHtoD(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "OpenCLIntList" => {
        val out = new StringBuilder
        out.append("\t%s *%s = new %s();\n".format(remap(sym.Type),quote(sym),remap(sym.Type)))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.toString
      }
      case _ => throw new Exception("OpenCLGen: copyInputHtoD(sym) : Cannot copy to GPU device (%s)".format(remap(sym.Type)))
    }
  }

  def copyOutputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "OpenCLIntList" => "\t//TODO: Implement this!\n"
      case _ => throw new Exception("OpenCLGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    }
  }

  def copyMutableInputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.Type)
    remap(sym.Type) match {
      case "OpenCLIntList" => "\t//TODO: Implement this!\n"
      case _ => throw new Exception("OpenCLGen: copyMutableInputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.Type)))
    }
  }

  //TODO: Remove below methods
  def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = {
    throw new GenerationFailedException("OpenCLGen: allocOutput(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.Type)))
  }
  def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = {
    throw new GenerationFailedException("OpenCLGen: allocReference(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.Type)))
  }

  def positionMultDimInputs(sym: Sym[Any]) : String = {
    throw new GenerationFailedException("OpenCLGen: positionMultDimInputs(sym) : Cannot reposition GPU memory (%s)".format(remap(sym.Type)))

  }

  def cloneObject(sym: Sym[Any], src: Sym[Any]) : String = {
    throw new GenerationFailedException("OpenCLGen: cloneObject(sym)")
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    val x = fresh[A]
    val y = f(x)

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting OpenCL Generated Code                  \n"+
                   "*******************************************/\n" +
                   "#include <stdio.h>\n" +
                   "#include <stdlib.h>"
    )

    stream.println("int main(int argc, char** argv) {")

    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))

    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of OpenCL Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }

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
      assert(syms.length == 1)
      stream.println(getDSLHeaders)
      super.emitKernelHeader(syms, getKernelOutputs ::: vals, vars, resultType, resultIsVar, external)
      hstream.println("#include \""+quote(syms(0))+".cl\"")
      hstream.flush
      return
    }

    val out = new StringBuilder
    //out.append(getDSLHeaders)

    val paramStr = (getKernelOutputs++getKernelInputs++getKernelTemps).filterNot(e=>isVoidType(e.Type)).map(ele =>
      if(isPrimitiveType(ele.Type))
        remap(ele.Type) + " " + quote(ele)
      else
        unpackObject(ele).map(e => remap(e._2) + " " + quote(ele) + "_" + e._1).mkString(",")
    ).mkString(", ")

    //TODO: Kernel parameters needs to be unrolled
    out.append("__kernel void kernel_%s(%s) {\n".format(syms.map(quote(_)).mkString(""),paramStr))
    out.append(addTab()+"int idxX = get_global_id(0);\n")
    val reAssembleString = (getKernelOutputs++getKernelInputs++getKernelTemps).filter(e=>isObjectType(e.Type)).map( ele =>
      remap(ele.Type) + " " + quote(ele) + ";" +
      unpackObject(ele).map(e => quote(ele) + "." + e._1 + " = " + quote(ele) + "_" + e._1).mkString(";")
    ).mkString(";\n") + ";\n"

    out.append(reAssembleString)
    /*
    for(in <- multDimInputs) {
      out.append(addTab()+positionMultDimInputs(in))
    }
    */
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

    tabWidth -= 1
      
	  //if(MetaData.gpuOutput == "") { throw new GenerationFailedException("OpenCLGen:No output for GPU")}

    // Emit input copy helper functions for object type inputs
    for(v <- vals if isObjectType(v.Type)) {
      helperFuncString.append(emitCopyInputHtoD(v, syms, copyInputHtoD(v)))
      helperFuncString.append(emitCopyMutableInputDtoH(v, syms, copyMutableInputDtoH(v)))
    }
    /*
    // Emit input copy helper functions for object type inputs
    for(v <- vals) {
      if(isObjectType(v.Type)) {
        MetaData.gpuInputs.add("{\"%s\":[\"%s\",\"copyInputHtoD_%s\",\"copyMutableInputDtoH_%s\"]}".format(quote(v),remap(v.Type),remap(v.Type),remap(v.Type)))
      }
    }
    */

    // Emit kerenl size calculation helper functions
    if (!external) {
      helperFuncString.append(emitSizeFuncs(syms,external))
    }

    // Print out to file stream
    hstream.print(helperFuncString)
    hstream.flush

    // Print out device function
    devStream.println(devFuncString)
    devStream.flush

  }

  /*******************************************************
   * Methods below are for emitting helper functions
   *******************************************************/
  // TODO: Change the metadata function names

  /*
  // For object type inputs, allocate GPU memory and copy from CPU to GPU.
  def emitCopyInputHtoD(sym: Sym[Any], ksyms: List[Sym[Any]], contents: String) : String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
	    helperFuncIdx += 1
      out.append("%s *copyInputHtoD_%s_%s_%s(%s) {\n".format(remap(sym.Type), ksyms.map(quote(_)).mkString(""), quote(sym),helperFuncIdx, "JNIEnv *env , jobject obj"))
      //out.append(copyInputHtoD(sym))
      out.append(contents)
      out.append("}\n")
      MetaData.gpuInputs.add("{\"%s\":[\"%s\",\"copyInputHtoD_%s_%s_%s\"".format(quote(sym),remap(sym.Type),ksyms.map(quote(_)).mkString(""),quote(sym),helperFuncIdx))
      out.toString
    }
    else ""
  }

  // For mutable inputs, copy the mutated datastructure from GPU to CPU after the kernel is terminated
  def emitCopyMutableInputDtoH(sym: Sym[Any], ksyms: List[Sym[Any]], contents: String): String = {
    val out = new StringBuilder
    if(isObjectType(sym.Type)) {
	  helperFuncIdx += 1
      out.append("void copyMutableInputDtoH_%s_%s_%s(%s) {\n".format(ksyms.map(quote(_)).mkString(""), quote(sym), helperFuncIdx, "JNIEnv *env , jobject obj, "+remap(sym.Type)+" *"+quote(sym)+"_ptr"))
      out.append("%s %s = *(%s_ptr);\n".format(remap(sym.Type),quote(sym),quote(sym)))
      //out.append(copyMutableInputDtoH(sym))
      out.append(contents)
      out.append("}\n")
      MetaData.gpuInputs.add("\"copyMutableInputDtoH_%s_%s_%s\",{%s}]}".format(ksyms.map(quote(_)).mkString(""),quote(sym),helperFuncIdx,unpackObject(sym).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(",")))
      out.toString
    }
    else ""    
  }

  def emitAllocOutput(sym: Sym[Any], contents: String, args: List[Sym[Any]]): String = {
	  val out = new StringBuilder
	  if(isObjectType(sym.Type)) {
	  	helperFuncIdx += 1
		val argStr = args.map("\""+quote(_)+"\"").mkString(",")
		val paramStr = args.map(ele =>
		  if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele) + "_ptr"
		  else remap(ele.Type) + " " + quote(ele)
		).mkString(",")
    val derefParams = args.map(ele=>
   	  if(isObjectType(ele.Type)) "\t%s %s = *(%s_ptr);\n".format(remap(ele.Type),quote(ele),quote(ele))
      else ""
    ).mkString("")

    MetaData.gpuOutput.add("{\"%s\":[\"%s\",\"allocFunc_%s\",[%s],".format(quote(sym),remap(sym.Type),helperFuncIdx,argStr))
    out.append("%s *allocFunc_%s(%s) {\n".format(remap(sym.Type), helperFuncIdx, paramStr))
		out.append(derefParams+"\n")
    out.append(contents)
    out.append("}\n")
    out.toString
	  }
	  else ""
  }

  def emitCopyOutputDtoH(sym: Sym[Any], ksym: Sym[Any], contents: String): String = {
	  val out = new StringBuilder
	  if(isObjectType(sym.Type)) {
	  	helperFuncIdx += 1
      val str = MetaData.gpuOutput.get(MetaData.gpuOutput.size-1)
      MetaData.gpuOutput.remove(MetaData.gpuOutput.size-1)
      MetaData.gpuOutput.add(str+"\"copyOutputDtoH_%s\",{%s}]}".format(helperFuncIdx,unpackObject(sym).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(",")))
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
    //val paramStr = inputs.map(ele=>
	//		if(isObjectType(ele.Type)) remap(ele.Type) + " *_" + quote(ele)
	//		else remap(ele.Type) + " " + quote(ele)
	//  ).mkString(",")

    /* Object type inputs of helper functions are pointers, but OpenCL generators assume the actual objects,
           therefore need to dereference the objects before emitting the actual block contents. */
    //val derefParams = inputs.map(ele=>
    //  if(isObjectType(ele.Type)) "\t%s %s = *_%s;\n".format(remap(ele.Type),quote(ele),quote(ele))
    //  else ""
    //).mkString("")

    // Generate allocation helper function
    //tempString.append("%s *allocFunc_%s(%s) {\n".format(remap(allocFunc.Type),currHelperFuncIdx,paramStr))
    //tempString.append(derefParams)
    //emitBlock(allocFunc)(tempStream)
    //tempString.append("\treturn %s;\n".format(quote(getBlockResult(allocFunc))))
    //tempString.append("}\n")

    // Generate allocation helper function
	//tempString.append(derefParams)
    emitBlock(allocFunc)(tempStream)
    tempString.append("\treturn %s_ptr;\n".format(quote(getBlockResult(allocFunc))))
    val allocOutputStr = emitAllocOutput(sym, tempString.toString, inputs)

    // Generate copy (D->H) helper function
    //tempString.append("jobject copyOutputDtoH_%s(JNIEnv *env,%s) {\n".format(helperFuncIdx,remap(sym.Type)+" *"+quote(sym)))
    //tempString.append(copyOutputDtoH(sym))
    //tempString.append("}\n")

    // Generate copy (D->H) helper function
    tempString2.append(copyOutputDtoH(sym))
	val copyOutputStr = emitCopyOutputDtoH(sym, null, tempString2.toString)

    // Register Metadata
    //TODO: How can I get rid of __global from the result of remap??
    //MetaData.gpuOutput = "{\"%s\":[\"%s\",\"allocFunc_%s\",[%s],\"copyOutputDtoH_%s\",{%s}]}".format(quote(sym),remap(sym.Type),currHelperFuncIdx,inputs.map("\""+quote(_)+"\"").mkString(","),helperFuncIdx,unpackObject(sym).map(f => "\"%s\":\"%s\"".format(f._1,remap(f._2)).replaceAll("__global ","")).mkString(","))
    gpuOutputs = gpuOutputs :+ sym

    // Write to helper function string
    //helperFuncString.append(tempString)
	helperFuncString.append(allocOutputStr)
	helperFuncString.append(copyOutputStr)
  }


  /**********************************************************
   * Calculation and Emission of GPU kernel size functions
   *********************************************************/

  //TODO: Get this information from the environment (OpenCL device version)
  val MAX_THREADS_PER_BLOCK = 64

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
  def emitSizeFuncs(syms: List[Sym[Any]]): String = {
    helperFuncIdx += 1

    val out = new StringBuilder

    if((xDimList.size == 0) && (MetaData.gpuLibCall==""))
      throw new GenerationFailedException("OpenCLGen: No dimension specified for this kernel.")

    val inputs = (gpuOutputs ::: gpuInputs ::: gpuTemps)
    val paramStr = inputs.map(ele=>
			if(isObjectType(ele.Type)) remap(ele.Type) + " *" + quote(ele)
			else remap(ele.Type) + " " + quote(ele)
	  ).mkString(",")
    val argStr = inputs.map("\""+quote(_)+"\"").mkString(",")
    val argInputStr = inputs.map(quote(_)).mkString(",")

    //TODO: Restore safety check for the dimension sizes
    out.append("int gpuBlockSizeX_%s_%s(%s) {\n".format(syms.map(quote(_)).mkString(""),helperFuncIdx,paramStr))
	  if(xDimList.length==0)
      out.append("\tint X = 1;\n")
	  else
	    out.append("\tint X = %s;\n".format(xDimList(xDimList.length-1)))
    out.append("\tif(X < %s) return X;\n".format(MAX_THREADS_PER_BLOCK))
    out.append("\telse return %s;\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    MetaData.gpuBlockSizeX = "[\"gpuBlockSizeX_%s_%s\",[%s]]".format(syms.map(quote(_)).mkString(""),helperFuncIdx,argStr)

    out.append("int gpuBlockSizeY_%s_%s(%s) {\n".format(syms.map(quote(_)).mkString(""),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuBlockSizeY = "[\"gpuBlockSizeY_%s_%s\",[%s]]".format(syms.map(quote(_)).mkString(""),helperFuncIdx,argStr)

    out.append("int gpuBlockSizeZ_%s_%s(%s) {\n".format(syms.map(quote(_)).mkString(""),helperFuncIdx,paramStr))
    out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuBlockSizeZ = "[\"gpuBlockSizeZ_%s_%s\",[%s]]".format(syms.map(quote(_)).mkString(""),helperFuncIdx,argStr)

    out.append("int gpuDimSizeX_%s_%s(%s) {\n".format(syms.map(quote(_)).mkString(""),helperFuncIdx,paramStr))
	  if(xDimList.length==0)
    	out.append("\tint X = 1;\n")
	  else
    	out.append("\tint X = %s;\n".format(xDimList(xDimList.length-1)))
    out.append("\treturn 1+((X-1)/%s);\n".format(MAX_THREADS_PER_BLOCK))
    out.append("}\n")
    MetaData.gpuDimSizeX = "[\"gpuDimSizeX_%s_%s\",[%s]]".format(syms.map(quote(_)).mkString(""),helperFuncIdx,argStr)

    out.append("int gpuDimSizeY_%s_%s(%s) {\n".format(syms.map(quote(_)).mkString(""),helperFuncIdx,paramStr))
	  out.append("\treturn 1;\n")
    out.append("}\n")
    MetaData.gpuDimSizeY = "[\"gpuDimSizeY_%s_%s\",[%s]]".format(syms.map(quote(_)).mkString(""),helperFuncIdx,argStr)
    out.toString
  }

  def emitLibCall(sym: Sym[Any], stmts: List[String]) : Unit = {
    val out = new StringBuilder

    //TODO: Move this check to call site
    //if(sym == kernelSymbol) {
      // Emit code for library call function
      val inputs = (gpuOutputs ::: gpuInputs)
      val paramStr = inputs.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(",")
      out.append("void gpuLibCall_%s(%s) {\n".format(quote(sym),paramStr))
      
      for(s <- stmts)
        out.append("\t"+s+"\n")
      out.append("}\n")
      helperFuncString.append(out.toString)

      // Add to metadata
      //MetaData.gpuLibCall = "{\"%s\":[\"%s\",\"gpuMemAlloc_%s_%s\",[%s]]}".format(quote(newSym),remap(newSym.Type),quote(kernelSymbol),quote(newSym),argStrTemp)
      MetaData.gpuLibCall = "\"gpuLibCall_%s\"".format(quote(sym))
    //}
    //else {
    //  throw new GenerationFailedException("OpenCLGen: Not GPUable (Only top-level node can use library call)")
    //}

  }
  */

}

// TODO: do we need this for each target?
trait OpenCLNestedCodegen extends GenericNestedCodegen with OpenCLCodegen {
  val IR: Expressions with Effects
  import IR._
  
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    super.emitSource[A,B](x => reifyEffects(f(x)), className, stream)
  }


  def OpenCLConsts(x:Exp[Any], s:String): String = {
    s match {
      case "Infinity" => "std::numeric_limits<%s>::max()".format(remap(x.Type))
      case _ => s
    }
  }
  
  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "NULL"
    case Const(z) => OpenCLConsts(x, z.toString)
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait OpenCLFatCodegen extends GenericFatCodegen with OpenCLCodegen {
  val IR: Expressions with Effects with FatExpressions
}
