package scala.virtualization.lms
package internal

import java.io.{File, FileWriter, PrintWriter}

import scala.reflect.SourceContext

trait ScalaCodegen extends GenericCodegen with Config {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "scala"

  override def toString = "scala"

  override def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      e.printStackTrace()
      kstream.close()
      outFile.delete
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {

    val x = fresh[A]
    val y = f(x)

    val sA = mA.toString
    val sB = mB.toString

    val staticData = getFreeDataBlock(y)

    stream.println("/*****************************************\n"+
                   "  Emitting Generated Code                  \n"+
                   "*******************************************/")
                   
    // TODO: separate concerns, should not hard code "pxX" name scheme for static data here
    stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.Type).mkString(",")+")")+" extends (("+sA+")=>("+sB+")) {")
    stream.println("def apply("+quote(x)+":"+sA+"): "+sB+" = {")
    
    emitBlock(y)(stream)
    stream.println(quote(getBlockResult(y)))
    
    stream.println("}")
    
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
    
    staticData
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    val kernelName = syms.map(quote).mkString("")
    
    stream.println("package generated." + this.toString)
    stream.println("object kernel_" + kernelName + " {")
    stream.print("def apply(")
    stream.print(vals.map(p => quote(p) + ":" + remap(p.Type)).mkString(","))

    // variable name mangling
    if (vals.length > 0 && vars.length > 0){
      stream.print(", ")
    }
    // TODO: remap Ref instead of explicitly adding generated.scala
    if (vars.length > 0){
      stream.print(vars.map(v => quote(v) + ":" + "generated.scala.Ref[" + remap(v.Type) +"]").mkString(","))
    }
    if (resultIsVar){
      stream.print("): " + "generated.scala.Ref[" + resultType + "] = {")
    }
    else {
      stream.print("): " + resultType + " = {")
    }

    stream.println("")
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    val kernelName = syms.map(quote).mkString("")
    stream.println(kernelName)
    stream.println("}}")
  }

  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
  
  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs + (if ((sourceinfo < 2) || sym.pos.isEmpty) "" else {
      val context = sym.pos(0)
      "      // " + relativePath(context.fileName) + ":" + context.line
    }))
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.Type) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }
}

trait ScalaNestedCodegen extends GenericNestedCodegen with ScalaCodegen {
  val IR: Expressions with Effects
  import IR._
  
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    super.emitSource[A,B](x => reifyEffects(f(x)), className, stream)
  }

  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }

}


trait ScalaFatCodegen extends GenericFatCodegen with ScalaCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
  
  override def emitFatNodeKernelExtra(syms: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = {
    val kernelName = syms.map(quote).mkString("")
    stream.println("final class activation_" + kernelName + " {")
    for (s <- syms) {
      stream.println("var " + quote(s) + ": " + remap(s.Type) + " = _")
    }
    stream.println("}")
  }
}
