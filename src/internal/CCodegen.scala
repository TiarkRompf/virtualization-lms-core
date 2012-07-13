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

  //var helperFuncIdx = 0
  //var helperFuncString:StringBuilder = null
  var helperFuncStream: PrintWriter = null
  var headerStream: PrintWriter = null
  val helperFuncList = ArrayBuffer[String]()
  //var kernelsList = ListBuffer[Exp[Any]]()

  var kernelInputVals: List[Sym[Any]] = Nil
  var kernelInputVars: List[Sym[Any]] = Nil
  var kernelOutputs: List[Sym[Any]] = Nil

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

  private def deref[A](m: Manifest[A]): String = {
    if (isPrimitiveType(m)) remap(m) + " "
    else remap(m) + " * "
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (!isVoidType(sym.tp))
      stream.println(deref(sym.tp) + quote(sym) + " = " + rhs + ";")
    else
      stream.println(rhs + ";")
  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    kernelInputVals = vals
    kernelInputVars = vars
    kernelOutputs = syms
    //helperFuncString.clear
    //metaData = new CMetaData
  }

  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs
    //helperFuncIdx = 0
    //helperFuncString = new StringBuilder

    /* file for helper functions (transfer function, allocation function) */
    helperFuncStream = new PrintWriter(new FileWriter(buildDir + "helperFuncs.cpp"))
    helperFuncStream.println("#include <jni.h>")
    helperFuncStream.println("#include \"cppHeader.hpp\"")

    /* header file for kernels and helper functions */
    headerStream = new PrintWriter(new FileWriter(buildDir + "cppHeader.hpp"))
    headerStream.println("#include <stdio.h>")
    headerStream.println("#include <string.h>")
    headerStream.println("#include <stdlib.h>")
    headerStream.println("#include <jni.h>")
    headerStream.println(getDSLHeaders)

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

    //TODO: Remove the dependency to Multiloop to Delite
    if(resultType != "void" && !resultType.startsWith("DeliteOpMultiLoop"))
      stream.println("return " + quote(syms(0)) + ";")

    if(!resultType.startsWith("DeliteOpMultiLoop"))
      stream.println("}")

    // Emit input copy helper functions for object type inputs
    for(v <- (vals++vars) if !isVoidType(v.tp)) {
      //TODO: For now just iterate over all possible hosts, but later we can pick one depending on the input target
      val (recvHeader, recvSource) = emitRecv(v, Hosts.JVM)
      if (!helperFuncList.contains(recvHeader)) {
        headerStream.println(recvHeader)
        helperFuncStream.println(recvSource)
        helperFuncList.append(recvHeader)
      }
      val (recvViewHeader, recvViewSource) = emitRecvView(v, Hosts.JVM)
      if (!helperFuncList.contains(recvViewHeader)) {
        headerStream.println(recvViewHeader)
        helperFuncStream.println(recvViewSource)
        helperFuncList.append(recvViewHeader)
      }
      val (updateHeader, updateSource) = emitSendUpdate(v, Hosts.JVM)
      if (!helperFuncList.contains(updateHeader)) {
        headerStream.println(updateHeader)
        helperFuncStream.println(updateSource)
        helperFuncList.append(updateHeader)
      }
      //helperFuncString.append(emitRecvUpdate(v,Hosts.JVM))
    }

    // Emit output copy helper functions for object type inputs
    for(v <- (syms) if !isVoidType(v.tp)) {
      val (sendHeader, sendSource) = emitSend(v, Hosts.JVM)
      if (!helperFuncList.contains(sendHeader)) {
        headerStream.println(sendHeader)
        helperFuncStream.println(sendSource)
        helperFuncList.append(sendHeader)
      }
      val (sendViewHeader, sendViewSource) = emitSendView(v, Hosts.JVM)
      if (!helperFuncList.contains(sendViewHeader)) {
        headerStream.println(sendViewHeader)
        helperFuncStream.println(sendViewSource)
        helperFuncList.append(sendViewHeader)
      }
    }

    // Print out dsl.h file
    //if(kernelsList.intersect(syms).isEmpty) {
      //headerStream.println("#include \"%s.cpp\"".format(syms.map(quote).mkString("")))
      //kernelsList ++= syms
    //}


    // Print helper functions to file stream
    //helperFuncStream.print(helperFuncString)
    helperFuncStream.flush
    headerStream.flush

    /*
    // Print out device function
    devStream.println(devFuncString)
    devStream.flush
    */
  }


  private def addRef[A](m: Manifest[A]): String = {
    if (!isPrimitiveType(m) && !isVoidType(m)) " *"
    else " "
  }

  def kernelName = "kernel_" + kernelOutputs.map(quote).mkString("")

  override def emitFileHeader() {
    stream.println("#include \"cppHeader.hpp\"\n")
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {

    //TODO: fix this
    if(external) throw new GenerationFailedException("CGen: Cannot have external libraries\n")



    def kernelSignature: String = {
      val out = new StringBuilder
      if(resultIsVar)
        out.append("Ref<" + resultType + ">")
      else
        out.append(resultType)
      out.append(addRef(syms(0).tp))
      out.append(kernelName + "(")
      out.append(vals.map(p=>remap(p.tp) + addRef(p.tp) + quote(p)).mkString(", "))
      if (vals.length > 0 && vars.length > 0){
        out.append(", ")
      }
      if (vars.length > 0){
        out.append(vars.map(v => "Ref<" + remap(v.tp) + "> " + addRef(v.tp) + quote(v)).mkString(","))
      }
      out.append(")")
      out.toString
    }

    //TODO: Remove the dependency to Multiloop to Delite
    if (!resultType.startsWith("DeliteOpMultiLoop")) {
      stream.println(kernelSignature + " {")
      headerStream.println(kernelSignature + ";")
    }
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
