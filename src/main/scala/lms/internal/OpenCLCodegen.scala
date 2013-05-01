package scala.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap}
import collection.mutable.{Map => MMap}
import collection.immutable.List._

trait OpenCLCodegen extends GPUCodegen with CppHostTransfer with OpenCLDeviceTransfer {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cl"
  override def toString = "opencl"

  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncStream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.cpp"))
    headerStream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.h"))

    //TODO: Put all the DELITE APIs declarations somewhere
    helperFuncStream.print("#include \"helperFuncs.h\"\n")
    headerStream.print(getDataStructureHeaders())
    headerStream.print("#include <iostream>\n")
    headerStream.print("#include <limits>\n")
    headerStream.print("#include <jni.h>\n\n")
    headerStream.print("#define CHAR short\n")
    headerStream.print("#define jCHAR jshort\n")
    headerStream.print("#include \"DeliteOpenCL.h\"\n")
    headerStream.print("#include \"DeliteArray.h\"\n")

    super.initializeGenerator(buildDir, args, _analysisResults)
  }

  def emitSource[A:TypeRep](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {

    val sB = typeRep[A].toString

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
}
