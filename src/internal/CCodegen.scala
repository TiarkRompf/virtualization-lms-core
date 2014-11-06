package scala.virtualization.lms
package internal

import java.io.{FileWriter, PrintWriter, File}
import collection.immutable.List._
import collection.mutable.ArrayBuffer

trait CCodegen extends CLikeCodegen with CppHostTransfer {
  val IR: Expressions
  import IR._

  override def deviceTarget: Targets.Value = Targets.Cpp

  override def kernelFileExt = "cpp"
  override def toString = "cpp"

  val helperFuncList = ArrayBuffer[String]()

  var kernelInputVals: List[Sym[Any]] = Nil
  var kernelInputVars: List[Sym[Any]] = Nil
  var kernelOutputs: List[Sym[Any]] = Nil

  override def resourceInfoType = "resourceInfo_t"
  override def resourceInfoSym = "resourceInfo"

  override def remap[A](m: Manifest[A]) : String = {
    m.toString match {
      case "java.lang.String" => "string"
      case _ => super.remap(m)
    }    
  }

  // we treat string as a primitive type to prevent memory management on strings
  // strings are always stack allocated and freed automatically at the scope exit
  override def isPrimitiveType(tpe: String) : Boolean = {
    tpe match {
      case "string" => true
      case _ => super.isPrimitiveType(tpe)
    }
  }
  
  override def quote(x: Exp[Any]) = x match {
    case Const(s: String) => "string(" + super.quote(x) + ")"
    case _ => super.quote(x)
  }

  override def isPrimitiveType[A](m: Manifest[A]) : Boolean = isPrimitiveType(remap(m))

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (!isVoidType(sym.tp))
      stream.println(remapWithRef(sym.tp) + quote(sym) + " = " + rhs + ";")
  }

  override def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
      stream.println(remapWithRef(sym.tp.typeArguments.head) + quote(sym) + " = " + rhs + ";")
  }

  override def emitVarDecl(sym: Sym[Any]): Unit = {
    stream.println(remapWithRef(sym.tp) + " " + quote(sym) + ";")
  }

  override def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    stream.println(quote(sym) + " = " + rhs + ";")
  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    kernelInputVals = vals
    kernelInputVars = vars
    kernelOutputs = syms
  }

  override def initializeGenerator(buildDir:String, args: Array[String]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs

    /* file for helper functions (transfer function, allocation function) */
    helperFuncStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.cpp"))
    helperFuncStream.println("#include <jni.h>")
    helperFuncStream.println("#include \"" + deviceTarget + "helperFuncs.h\"")

    /* type aliases */
    typesStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "types.h"))

    /* header file for kernels and helper functions */
    headerStream = new PrintWriter(new FileWriter(buildDir + deviceTarget + "helperFuncs.h"))
    headerStream.println("#include <stdio.h>")
    headerStream.println("#include <string.h>")
    headerStream.println("#include <stdint.h>")
    headerStream.println("#include <stdlib.h>")
    headerStream.println("#include <memory>")
    headerStream.println("#include <float.h>")
    headerStream.println("#include <jni.h>")
    headerStream.println("#include <assert.h>")
    headerStream.println("#include <math.h>")
    headerStream.println("#include <iostream>")
    headerStream.println("#include <limits>")
    headerStream.println("#include <algorithm>")
    headerStream.println("#include \"" + deviceTarget + "types.h\"")
    headerStream.println(getDataStructureHeaders())

    super.initializeGenerator(buildDir, args)
  }

  def emitForwardDef[A:Manifest](args: List[Manifest[_]], functionName: String, out: PrintWriter) = {
    out.println(remap(manifest[A])+" "+functionName+"("+args.map(a => remap(a)).mkString(", ")+");")
  }
      
  def emitSource[A:Manifest](args: List[Sym[_]], body: Block[A], functionName: String, out: PrintWriter) = {

    val sA = remap(manifest[A])

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  Emitting C Generated Code                  \n"+
                     "*******************************************/\n" +
                     "#include <stdio.h>\n" +
                     "#include <stdlib.h>\n" +
                     "#include <stdbool.h>"
      )


      // TODO: static data

      //stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tp).mkString(",")+")")+" 
      //extends (("+args.map(a => remap(a.tp)).mkString(", ")+")=>("+sA+")) {")

      stream.println(sA+" "+functionName+"("+args.map(a => remapWithRef(a.tp)+" "+quote(a)).mkString(", ")+") {")

      emitBlock(body)

      val y = getBlockResult(body)
      if (remap(y.tp) != "void")
        stream.println("return " + quote(y) + ";")

      stream.println("}")
      stream.println("/*****************************************\n"+
                     "  End of C Generated Code                  \n"+
                     "*******************************************/")
    }
    Nil
  }  

  override def emitTransferFunctions() {

    for ((tp,name) <- dsTypesList) {
      try {
        // Emit input copy helper functions for object type inputs
        //TODO: For now just iterate over all possible hosts, but later we can pick one depending on the input target
        val (recvHeader, recvSource) = emitRecv(tp, Targets.JVM)
        if (!helperFuncList.contains(recvHeader)) {
          headerStream.println(recvHeader)
          helperFuncStream.println(recvSource)
          helperFuncList.append(recvHeader)
        }
        val (recvViewHeader, recvViewSource) = emitRecvView(tp, Targets.JVM)
        if (!helperFuncList.contains(recvViewHeader)) {
          headerStream.println(recvViewHeader)
          helperFuncStream.println(recvViewSource)
          helperFuncList.append(recvViewHeader)
        }
        val (sendUpdateHeader, sendUpdateSource) = emitSendUpdate(tp, Targets.JVM)
        if (!helperFuncList.contains(sendUpdateHeader)) {
          headerStream.println(sendUpdateHeader)
          helperFuncStream.println(sendUpdateSource)
          helperFuncList.append(sendUpdateHeader)
        }
        val (recvUpdateHeader, recvUpdateSource) = emitRecvUpdate(tp, Targets.JVM)
        if (!helperFuncList.contains(recvUpdateHeader)) {
          headerStream.println(recvUpdateHeader)
          helperFuncStream.println(recvUpdateSource)
          helperFuncList.append(recvUpdateHeader)
        }

        // Emit output copy helper functions for object type inputs
        val (sendHeader, sendSource) = emitSend(tp, Targets.JVM)
        if (!helperFuncList.contains(sendHeader)) {
          headerStream.println(sendHeader)
          helperFuncStream.println(sendSource)
          helperFuncList.append(sendHeader)
        }
        val (sendViewHeader, sendViewSource) = emitSendView(tp, Targets.JVM)
        if (!helperFuncList.contains(sendViewHeader)) {
          headerStream.println(sendViewHeader)
          helperFuncStream.println(sendViewSource)
          helperFuncList.append(sendViewHeader)
        }
        val (mkManifestHeader, mkManifestSource) = emitMakeManifest(tp)
        if (!helperFuncList.contains(mkManifestHeader)) {
          headerStream.println(mkManifestHeader)
          helperFuncStream.println(mkManifestSource)
          helperFuncList.append(mkManifestHeader)
        }
      }
      catch {
        case e: GenerationFailedException => 
          helperFuncStream.flush
          headerStream.flush
        case e: Exception => throw(e)
      }
    }

    helperFuncStream.flush
    headerStream.flush
    typesStream.flush
  }

  def kernelName = "kernel_" + kernelOutputs.map(quote).mkString("")

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {
    super.emitKernelHeader(syms, vals, vars, resultType, resultIsVar, external, isMultiLoop)
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {
    super.emitKernelFooter(syms, vals, vars, resultType, resultIsVar, external, isMultiLoop)
  }

}

trait CNestedCodegen extends CLikeNestedCodegen with CCodegen {
  val IR: Expressions with Effects
  import IR._
  
}

trait CFatCodegen extends CLikeFatCodegen with CCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._

}
