package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap}
import collection.mutable.{Map => MMap}
import collection.immutable.List._

trait OpenCLCodegen extends GPUCodegen with CppHostTransfer with OpenCLDeviceTransfer {
  val IR: Expressions
  import IR._

  override def deviceTarget: Targets.Value = Targets.OpenCL

  override def kernelFileExt = "cl"
  override def toString = "opencl"

  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.cpp"))
    headerStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.h"))

    //TODO: Put all the DELITE APIs declarations somewhere
    helperFuncStream.print("#include \"" + deviceTarget + "helperFuncs.h\"\n")
    headerStream.print(getDSLHeaders)
    headerStream.print("#include <iostream>\n")
    headerStream.print("#include <limits>\n")
    headerStream.print("#include <jni.h>\n\n")
    headerStream.print("#define CHAR short\n")
    headerStream.print("#define jCHAR jshort\n")
    headerStream.print("#include \"DeliteOpenCL.h\"\n")
    headerStream.print("#include \"DeliteArray.h\"\n")

    super.initializeGenerator(buildDir, args, _analysisResults)
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

}

// TODO: do we need this for each target?
trait OpenCLNestedCodegen extends CLikeNestedCodegen with OpenCLCodegen {
  val IR: Expressions with Effects
  import IR._
  
}

trait OpenCLFatCodegen extends CLikeFatCodegen with OpenCLCodegen {
  val IR: Expressions with Effects with FatExpressions
	import IR._
}
