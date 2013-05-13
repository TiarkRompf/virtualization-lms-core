package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import collection.mutable.{Map => MMap}
import collection.immutable.List._

trait CudaCodegen extends GPUCodegen with CppHostTransfer with CudaDeviceTransfer {
  val IR: Expressions
  import IR._

  override def deviceTarget: Targets.Value = Targets.Cuda

  override def kernelFileExt = "cu"
  override def toString = "cuda"
  override def devFuncPrefix = "__device__"
  
  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs

    actRecordStream = new PrintWriter(new FileWriter(buildDir + "actRecords.h"))
    helperFuncStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.cu"))
    headerStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.h"))

    //TODO: Put all the DELITE APIs declarations somewhere
    helperFuncStream.print("#include \"" + deviceTarget + "helperFuncs.h\"\n")    
    headerStream.print(getDSLHeaders)
    headerStream.print("#include <iostream>\n")
    headerStream.print("#include <limits>\n")
    headerStream.print("#include <assert.h>\n")
    headerStream.print("#include <jni.h>\n\n")
    headerStream.print("#define CHAR short\n")
    headerStream.print("#define jCHAR jshort\n")
    headerStream.print("#include \"DeliteCuda.h\"\n")
    headerStream.print("#include \"actRecords.h\"\n")
    super.initializeGenerator(buildDir, args, _analysisResults)
  }

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val sB = manifest[A].toString

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  Emitting Cuda Generated Code                  \n"+
                     "*******************************************/\n" +
                     "#include <stdio.h>\n" +
                     "#include <stdlib.h>"
      )

      stream.println("int main(int argc, char** argv) {")

      emitBlock(body)
      //stream.println(quote(getBlockResult(y)))

      stream.println("}")
      stream.println("/*****************************************\n"+
                     "  End of Cuda Generated Code                  \n"+
                     "*******************************************/")
    }
    Nil
  }

}

// TODO: do we need this for each target?
trait CudaNestedCodegen extends CLikeNestedCodegen with CudaCodegen {
  val IR: Expressions with Effects
  import IR._
  
}

trait CudaFatCodegen extends CLikeFatCodegen with CudaCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
}
