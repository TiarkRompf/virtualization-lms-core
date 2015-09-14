package scala.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap}
import collection.immutable.List._

trait OpenCLCodegen extends GPUCodegen with CppHostTransfer with OpenCLDeviceTransfer {
  val IR: BaseExp
  import IR._

  override def deviceTarget: Targets.Value = Targets.OpenCL

  override def fileExtension = "cl"
  override def toString = "opencl"

  override def initializeGenerator(buildDir:String): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.cpp"))
    helperFuncStream.println("#include \"" + deviceTarget + "helperFuncs.h\"")

    typesStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "types.h"))

    //TODO: Put all the DELITE APIs declarations somewhere
    headerStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.h"))
    headerStream.println("#include <iostream>")
    headerStream.println("#include <limits>")
    headerStream.println("#include <float.h>")
    headerStream.println("#include <jni.h>")
    headerStream.println("#include \"" + deviceTarget + "types.h\"")
    headerStream.println(getDataStructureHeaders())

    super.initializeGenerator(buildDir)
  }

  def emitSource[A : Typ](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {

    val sB = typ[A].toString

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
