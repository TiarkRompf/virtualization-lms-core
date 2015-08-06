package scala.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import collection.immutable.List._

trait CudaCodegen extends GPUCodegen with CppHostTransfer with CudaDeviceTransfer {
  val IR: Expressions
  import IR._

  override def deviceTarget: Targets.Value = Targets.Cuda

  override def kernelFileExt = "cu"
  override def toString = "cuda"
  override def devFuncPrefix = "__device__"
  
  override def initializeGenerator(buildDir:String, args: Array[String]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs

    actRecordStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "actRecords.h"))
    
    helperFuncStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.cu"))
    helperFuncStream.print("#include \"" + deviceTarget + "helperFuncs.h\"\n")    
    
    typesStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "types.h"))
    typesStream.flush
    
    //TODO: Put all the DELITE APIs declarations somewhere
    headerStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.h"))
    headerStream.println("#include <iostream>")
    headerStream.println("#include <limits>")
    headerStream.println("#include <float.h>")
    headerStream.println("#include <assert.h>")
    headerStream.println("#include <jni.h>")
    headerStream.println("#include <stdint.h>")
    headerStream.println("#include \"" + deviceTarget + "types.h\"")
    headerStream.println(getDataStructureHeaders())
    headerStream.println("#include \"" + deviceTarget + "actRecords.h\"")

    super.initializeGenerator(buildDir, args)
  }

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val sB = remap(manifest[A])

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
