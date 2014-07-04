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

  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.cpp"))
    helperFuncHdrStream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.h"))

    //TODO: Put all the DELITE APIs declarations somewhere
    hstream.print("#include \"helperFuncs.h\"\n")
    helperFuncHdrStream.print(getDSLHeaders)
    helperFuncHdrStream.print("#include <iostream>\n")
    helperFuncHdrStream.print("#include <limits>\n")
    helperFuncHdrStream.print("#include <jni.h>\n\n")
    helperFuncHdrStream.print("#define CHAR short\n")
    helperFuncHdrStream.print("#define jCHAR jshort\n")
    helperFuncHdrStream.print("#include \"DeliteOpenCL.h\"\n")
    helperFuncHdrStream.print("#include \"DeliteArray.h\"\n")

    super.initializeGenerator(buildDir, args, _analysisResults)
  }

  /*
  override def isObjectType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "scala.collection.immutable.List[Int]" => true
        //TODO: ObjectTypes needs to be able to broken down, but array does not have to be.
        //TODO: What we need to do is to distinguish between passable types or not to the opencl kernel
      case "Array[Int]" | "Array[Long]" | "Array[Float]" | "Array[Double]" | "Array[Boolean]" => true
      case _ => super.isObjectType(m)
    }
  }
  */

  /*
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
  */

  /*
  override def unpackObject[A](sym: Sym[Any]) : Map[String,Manifest[_]] = remap(sym.Type) match {
    case "OpenCLIntList" => Map("length"->Manifest.Int)    //TODO: How to initialize the data array type for the list?
    case _ => throw new GenerationFailedException("OpenCLGen: Type %s cannot be unpacked.".format(sym.Type.toString))
  }
  */

  // TODO: Move to Delite?
  def copyInputHtoD(sym: Sym[Any]) : String = {
    checkGPUableType(sym.tp)
    remap(sym.tp) match {
      case "DeliteArray_bool" | "DeliteArray_char" | "DeliteArray_CHAR" | "DeliteArray_short" | "DeliteArray_int" | "DeiteArray_long" | "DeliteArray_float" | "DeliteArray_double" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val numBytesStr = "length * sizeof(%s)".format(remap(typeArg))
        out.append("\tint length = env->GetArrayLength((j%sArray)obj);\n".format(remapToJNI(typeArg).toLowerCase))
        out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
        out.append("\t%s *%s = new %s(length);\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
        out.append("\tDeliteOpenCLMemcpyHtoDAsync(%s->data, dataPtr, %s);\n".format(quote(sym),numBytesStr))
        out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
        out.append("\treturn %s;\n".format(quote(sym)))
        out.toString
      case _ => throw new Exception("OpenCLGen: copyInputHtoD(sym) : Cannot copy to GPU device (%s)".format(remap(sym.tp)))
    }
  }

  def copyOutputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.tp)
    if (isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      out.append("\t%s data;\n".format(remap(sym.tp)))
      out.append("\tDeliteOpenCLMemcpyDtoHAsync(&data, %s, sizeof(%s));\n".format(quote(sym),remap(sym.tp)))
      out.append("\treturn data;\n")
      out.toString
    }
    else {
      remap(sym.tp) match {
        case "DeliteArray_bool" | "DeliteArray_char" | "DeliteArray_CHAR" | "DeliteArray_short" | "DeliteArray_int" | "DeiteArray_long" | "DeliteArray_float" | "DeliteArray_double" =>
          val out = new StringBuilder
          val typeArg = sym.tp.typeArguments.head
          val numBytesStr = "%s.length * sizeof(%s)".format(quote(sym),remap(typeArg))
          out.append("\tj%sArray arr = env->New%sArray(%s.length);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg),quote(sym)))
          out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)arr,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
          out.append("\tDeliteOpenCLMemcpyDtoHAsync(dataPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
          out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)arr, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
          out.append("\treturn arr;\n")
          out.toString
        case _ => throw new Exception("OpenCLGen: copyOutputDtoH(sym) : Cannot copy from GPU device (%s)".format(remap(sym.tp)))
      }
    }
  }

  def copyMutableInputDtoH(sym: Sym[Any]) : String = {
    checkGPUableType(sym.tp)
    remap(sym.tp) match {
      case "DeliteArray_bool" | "DeliteArray_char" | "DeliteArray_CHAR" | "DeliteArray_short" | "DeliteArray_int" | "DeiteArray_long" | "DeliteArray_float" | "DeliteArray_double" =>
        val out = new StringBuilder
        val typeArg = sym.tp.typeArguments.head
        val numBytesStr = "length * sizeof(%s)".format(remap(typeArg))
        out.append("\tint length = %s.length;\n".format(quote(sym)))
        out.append("\tj%s *dataPtr = (j%s *)env->GetPrimitiveArrayCritical((j%sArray)obj,0);\n".format(remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase,remapToJNI(typeArg).toLowerCase))
        out.append("\tDeliteOpenCLMemcpyDtoHAsync(dataPtr, %s.data, %s);\n".format(quote(sym),numBytesStr))
        out.append("\tenv->ReleasePrimitiveArrayCritical((j%sArray)obj, dataPtr, 0);\n".format(remapToJNI(typeArg).toLowerCase))
        out.toString
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

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val sB = manifest[A].toString

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  Emitting OpenCL Generated Code                  \n"+
                     "*******************************************/\n" +
                     "#include <stdio.h>\n" +
                     "#include <stdlib.h>"
      )

      stream.println("int main(int argc, char** argv) {")

      emitBlock(body)
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

  def emitAssignment(sym: Sym[Any], lhs:String, rhs: String): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }

}

// TODO: do we need this for each target?
trait OpenCLNestedCodegen extends CLikeNestedCodegen with OpenCLCodegen {
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
    case Const(s: Char) => "'"+s+"'"
    case Const(null) => "NULL"
    case Const(z) => OpenCLConsts(x, z.toString)
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait OpenCLFatCodegen extends CLikeFatCodegen with OpenCLCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
  
  def emitMultiLoopCond(sym: Sym[Any], funcs:List[Block[Any]], idx: Sym[Int], postfix: String="", stream:PrintWriter):(String,List[Exp[Any]]) = {
    isNestedNode = true
    devFuncIdx += 1
    val currIdx = devFuncIdx
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString, true)
    val header = new StringWriter
    val footer = new StringWriter

    val currentTab = tabWidth
    tabWidth = 1
    withStream(tempStream) {
      emitFatBlock(funcs)
    }
    tabWidth = currentTab

    val inputs = getFreeVarBlock(Block(Combine(funcs.map(getBlockResultFull))),Nil).filterNot(quote(_)==quote(idx)).distinct
    val paramStr = (inputs++List(idx)).map(ele=>remap(ele.tp)+" "+quote(ele)).mkString(",")
    header.append("bool dev_%s(%s) {\n".format(postfix,paramStr))
    footer.append("\treturn %s;\n".format(funcs.map(f=>quote(getBlockResult(f))).mkString("&&")))
    footer.append("}\n")
    stream.print(header)
    stream.print(tempString)
    stream.print(footer)

    //Register Metadata for loop function
    val lf = metaData.loopFuncs.getOrElse(sym,new LoopFunc)
    lf.hasCond = true
    lf.loopCondInputs = inputs.map(quote)
    metaData.loopFuncs.put(sym,lf)
    isNestedNode = false

    ("dev_"+currIdx,inputs)
  }

}
