package scala.virtualization.lms
package internal

import java.io.{FileWriter, StringWriter, PrintWriter, File}
import java.util.ArrayList
import collection.mutable.{ListBuffer, ArrayBuffer, LinkedList, HashMap, ListMap, HashSet, Map => MMap}
import collection.immutable.List._


trait CCodegen extends CLikeCodegen with CppHostTransfer {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cpp"
  override def toString = "cpp"

  var helperFuncIdx = 0
  var helperFuncString:StringBuilder = null
  var hstream: PrintWriter = null
  var headerStream: PrintWriter = null
  var kernelsList = ListBuffer[Exp[Any]]()

  /*
  override def hasMetaData: Boolean = false
  override def getMetaData: String = metaData.toString
  var metaData: CMetaData = null

  final class TransferFunc {
    var funcHtoD:String = _
    var argsFuncHtoD:List[Sym[Any]] = _
    var funcDtoH:String = _
    var argsFuncDtoH:List[Sym[Any]] = _
  }
  final class CMetaData {
    val inputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    val outputs: ListMap[Sym[Any],TransferFunc] = ListMap()
    override def toString: String = {
      val out = new StringBuilder
      out.append("{")

      out.append("\"cppInputs\":["+inputs.toList.reverse.map(in=>"{\""+quote(in._1)+"\":[\""+remap(in._1.tp)+"\",\""+in._2.funcHtoD+"\",\""+in._2.funcDtoH+"\"]}").mkString(",")+"],")
      out.append("\"cppOutputs\":["+outputs.toList.reverse.map(out=>"{\""+quote(out._1)+"\":[\""+remap(out._1.tp)+"\",\""+out._2.funcDtoH+"\"]}").mkString(",")+"]")
      out.append("}")
      out.toString
    }
  }
  */

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    helperFuncString.clear
    //metaData = new CMetaData
  }

  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    helperFuncIdx = 0
    helperFuncString = new StringBuilder
    hstream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.cpp"))
    headerStream = new PrintWriter(new FileWriter(buildDir + "dsl.hpp"))
    headerStream.println("#include \"helperFuncs.cpp\"")
    super.initializeGenerator(buildDir, args, _analysisResults)
  }
      
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, out: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val x = fresh[A]
    val y = reifyBlock(f(x))

    val sA = mA.toString
    val sB = mB.toString

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  Emitting C Generated Code                  \n"+
                     "*******************************************/\n" +
                     "#include <stdio.h>\n" +
                     "#include <stdlib.h>"
      )

      //stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
      stream.println("int main(int argc, char** argv) {")

      emitBlock(y)
      //stream.println(quote(getBlockResult(y)))

      //stream.println("}")
      stream.println("}")
      stream.println("/*****************************************\n"+
                     "  End of C Generated Code                  \n"+
                     "*******************************************/")
    }
    Nil
  }  

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {

    
    if(resultType != "void")
      stream.println("return " + quote(syms(0)) + ";")
    stream.println("}")

    // Emit input copy helper functions for object type inputs
    for(v <- (vals++vars) if !isVoidType(v.tp)) {
      //TODO: For now just iterate over all possible hosts, but later we can pick one depending on the input target
      helperFuncString.append(emitRecv(v,Hosts.JVM))
      helperFuncString.append(emitUpdate(v,Hosts.JVM))
      //helperFuncString.append(emitUpdated(v,Hosts.JVM))
    }

    // Emit output copy helper functions for object type inputs
    for(v <- (syms) if !isVoidType(v.tp)) {
      helperFuncString.append(emitSend(v,Hosts.JVM))
    }

    // Print helper functions to file stream
    hstream.print(helperFuncString)
    hstream.flush

    // Print out dsl.h file
    if(kernelsList.intersect(syms).isEmpty) {
      headerStream.println("#include \"%s.cpp\"".format(syms.map(quote).mkString("")))
      kernelsList ++= syms
    }
    headerStream.flush

    /*
    // Print out device function
    devStream.println(devFuncString)
    devStream.flush
    */
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {

    //TODO: fix this
    if(external) throw new GenerationFailedException("CGen: Cannot have external libraries\n")

    val kernelName = syms.map(quote).mkString("")

    stream.print(resultType)
     
    stream.print(" kernel_" + kernelName + "(")
    stream.print(vals.map(p=>remap(p.tp) + " " + quote(p)).mkString(", "))
    if (vals.length > 0 && vars.length > 0){
      stream.print(", ")
    }
    if (vars.length > 0){
      stream.print(vars.map(v => remap(v.tp) + " &" + quote(v)).mkString(","))
    }

    stream.println(") {")
  }


}

// TODO: do we need this for each target?
trait CNestedCodegen extends CLikeNestedCodegen with CCodegen {
  val IR: Expressions with Effects
  import IR._
  
}

trait CFatCodegen extends CLikeFatCodegen with CCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._

  def emitMultiLoopCond(sym: Sym[Any], funcs:List[Block[Any]], idx: Sym[Int], postfix: String="", stream:PrintWriter):(String,List[Exp[Any]]) = {
    throw new GenerationFailedException("CGen: emitMultiLoopCond not supported yet.")
  }
}
