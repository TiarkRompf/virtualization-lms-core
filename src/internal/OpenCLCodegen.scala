package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap}
import collection.mutable.{Map => MMap}
import collection.immutable.List._

trait OpenCLCodegen extends GPUCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cl"
  override def toString = "opencl"

  /*
  override def emitDevFunc(func:Block[Any], locals:List[Exp[Any]]):(String,List[Exp[Any]]) = {
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
    val paramStr = (locals++inputs).map(ele=>remap(ele.tp)+" "+quote(ele)).mkString(",")
    header.append("%s dev_%s(%s) {\n".format(remap(func.tp),currIdx,paramStr))
    //header.append("\tint idxX = get_global_id(0);\n")
    if(remap(func.tp) != "void")
      footer.append("\treturn %s;\n".format(quote(getBlockResult(func))))
    footer.append("}\n")
    devFuncString.append(header)
    devFuncString.append(tempString)
    devFuncString.append(footer)

    ("dev_"+currIdx,inputs)
  }
  */

  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.h"))
    //devStream = new PrintWriter(new FileWriter(buildDir+"devFuncs.cl"))
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
    
    super.initializeGenerator(buildDir, args, _analysisResults)
  }

  override def isObjectType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "scala.collection.immutable.List[Int]" => true
        //TODO: ObjectTypes needs to be able to broken down, but array does not have to be.
        //TODO: What we need to do is to distinguish between passable types or not to the opencl kernel
      case "Array[Int]" | "Array[Long]" | "Array[Float]" | "Array[Double]" | "Array[Boolean]" => true
      case _ => super.isObjectType(m)
    }
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

  override def unpackObject[A](sym: Sym[Any]) : Map[String,Manifest[_]] = remap(sym.tp) match {
    case "OpenCLIntList" => Map("length"->Manifest.Int)    //TODO: How to initialize the data array type for the list?
    case _ => throw new GenerationFailedException("OpenCLGen: Type %s cannot be unpacked.".format(sym.tp.toString))
  }

  // TODO: Handle general C datastructure
  def copyInputHtoD(sym: Sym[Any]) : String = {
    checkGPUableType(sym.tp)
    remap(sym.tp) match {
      case "OpenCLIntList" => {
        val out = new StringBuilder
        out.append("\t%s *%s = new %s();\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.toString
      }
      case _ => throw new Exception("OpenCLGen: copyInputHtoD(sym) : Cannot copy to GPU device (%s)".format(remap(sym.tp)))
    }
  }

  def copyOutputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.tp)
    remap(sym.tp) match {
      case "OpenCLIntList" => "\t//TODO: Implement this!\n"
      case _ => throw new Exception("OpenCLGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.tp)))
    }
  }

  def copyMutableInputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.tp)
    remap(sym.tp) match {
      case "OpenCLIntList" => "\t//TODO: Implement this!\n"
      case _ => throw new Exception("OpenCLGen: copyMutableInputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.tp)))
    }
  }

  //TODO: Remove below methods
  def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = {
    throw new GenerationFailedException("OpenCLGen: allocOutput(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.tp)))
  }
  def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = {
    throw new GenerationFailedException("OpenCLGen: allocReference(newSym, sym) : Cannot allocate GPU memory (%s)".format(remap(sym.tp)))
  }

  def positionMultDimInputs(sym: Sym[Any]) : String = {
    throw new GenerationFailedException("OpenCLGen: positionMultDimInputs(sym) : Cannot reposition GPU memory (%s)".format(remap(sym.tp)))
  }

  def cloneObject(sym: Sym[Any], src: Sym[Any]) : String = {
    throw new GenerationFailedException("OpenCLGen: cloneObject(sym)")
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, out: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val x = fresh[A]
    val y = reifyBlock(f(x))

    val sA = mA.toString
    val sB = mB.toString

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  Emitting OpenCL Generated Code                  \n"+
                     "*******************************************/\n" +
                     "#include <stdio.h>\n" +
                     "#include <stdlib.h>"
      )

      stream.println("int main(int argc, char** argv) {")

      emitBlock(y)
      //stream.println(quote(getBlockResult(y)))

      stream.println("}")
      stream.println("/*****************************************\n"+
                     "  End of OpenCL Generated Code                  \n"+
                     "*******************************************/")
      }
    Nil
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println(addTab() + remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println(addTab()+ remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }

  /*
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
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

    val paramStr = (getKernelOutputs++getKernelInputs++getKernelTemps).filterNot(e=>isVoidType(e.tp)).map(ele =>
      if(isPrimitiveType(ele.tp))
        remap(ele.tp) + " " + quote(ele)
      else
        unpackObject(ele).map(e => remap(e._2) + " " + quote(ele) + "_" + e._1).mkString(",")
    ).mkString(", ")

    //TODO: Kernel parameters needs to be unrolled
    out.append("__kernel void kernel_%s(%s) {\n".format(syms.map(quote(_)).mkString(""),paramStr))
    out.append(addTab()+"int idxX = get_global_id(0);\n")
    val reAssembleString = (getKernelOutputs++getKernelInputs++getKernelTemps).filter(e=>isObjectType(e.tp)).map( ele =>
      remap(ele.tp) + " " + quote(ele) + ";" +
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

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
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
    for(v <- vals if isObjectType(v.tp)) {
      helperFuncString.append(emitCopyInputHtoD(v, syms, copyInputHtoD(v)))
      helperFuncString.append(emitCopyMutableInputDtoH(v, syms, copyMutableInputDtoH(v)))
    }
    /*
    // Emit input copy helper functions for object type inputs
    for(v <- vals) {
      if(isObjectType(v.tp)) {
        MetaData.gpuInputs.add("{\"%s\":[\"%s\",\"copyInputHtoD_%s\",\"copyMutableInputDtoH_%s\"]}".format(quote(v),remap(v.tp),remap(v.tp),remap(v.tp)))
      }
    }
    */

    // Print out to file stream
    hstream.print(helperFuncString)
    hstream.flush

    // Print out device function
    //devStream.println(devFuncString)
    //devStream.flush
  }
  */

}

// TODO: do we need this for each target?
trait OpenCLNestedCodegen extends GenericNestedCodegen with OpenCLCodegen {
  val IR: Expressions with Effects
  import IR._

  def OpenCLConsts(x:Exp[Any], s:String): String = {
    s match {
      case "Infinity" => "std::numeric_limits<%s>::max()".format(remap(x.tp))
      case _ => s
    }
  }
  
  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "NULL"
    case Const(z) => OpenCLConsts(x, z.toString)
    case _ => super.quote(x)
  }
  
}

trait OpenCLFatCodegen extends GenericFatCodegen with OpenCLCodegen {
  val IR: Expressions with Effects with FatExpressions
}
