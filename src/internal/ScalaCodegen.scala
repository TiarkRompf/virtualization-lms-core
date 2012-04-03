package scala.virtualization.lms
package internal

import java.io.{File, FileWriter, PrintWriter}

trait ScalaCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "scala"

  override def toString = "scala"

  override def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      e.printStackTrace()
      kstream.close()
      outFile.delete
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, out: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {

    val x = fresh[A]
    val y = reifyBlock(f(x))

    val sA = mA.toString
    val sB = mB.toString

    val staticData = getFreeDataBlock(y)

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  Emitting Generated Code                  \n"+
                     "*******************************************/")
                   
      // TODO: separate concerns, should not hard code "pxX" name scheme for static data here
      stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tp).mkString(",")+")")+" extends (("+sA+")=>("+sB+")) {")
      stream.println("def apply("+quote(x)+":"+sA+"): "+sB+" = {")
    
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
    
      stream.println("}")
    
      stream.println("}")
      stream.println("/*****************************************\n"+
                     "  End of Generated Code                  \n"+
                     "*******************************************/")
    }
    
    staticData
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    val kernelName = syms.map(quote).mkString("")
    
    stream.println("package generated." + this.toString)
    stream.println("object kernel_" + kernelName + " {")
    stream.print("def apply(")
    stream.print(vals.map(p => quote(p) + ":" + remap(p.tp)).mkString(","))

    // variable name mangling
    if (vals.length > 0 && vars.length > 0){
      stream.print(", ")
    }
    if (vars.length > 0){
      stream.print(vars.map(v => quote(v) + ":" + "generated.scala.Ref[" + remap(v.tp) +"]").mkString(","))
    }
    if (resultIsVar){
      stream.print("): " + "generated.scala.Ref[" + resultType + "] = {")
    }
    else {
      stream.print("): " + resultType + " = {")
    }

    stream.println("")
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    val kernelName = syms.map(quote).mkString("")
    stream.println(kernelName)
    stream.println("}}")
  }


  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs) // + "        //" + sym.tp.debugInfo)
  }
  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String): Unit = {
    stream.println(lhs + " = " + rhs)
  }
}

trait ScalaNestedCodegen extends GenericNestedCodegen with ScalaCodegen {
  val IR: Expressions with Effects
  import IR._
  
  // emit forward decls for recursive vals
  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
    recursive foreach emitForwardDef
    super.traverseStmsInBlock(stms)
  }
  
  def emitForwardDef(sym: Sym[Any]): Unit = {
    stream.println("var " + quote(sym) + /*": " + remap(sym.tp) +*/ " = null.asInstanceOf[" + remap(sym.tp) + "]")
  }
  
  // special case for recursive vals
  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (recursive contains sym)
      stream.println(quote(sym) + " = " + rhs) // we have a forward declaration above.
    else
      super.emitValDef(sym,rhs)
  }
  
}


trait ScalaFatCodegen extends GenericFatCodegen with ScalaCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
  
  override def emitFatNodeKernelExtra(syms: List[Sym[Any]], rhs: FatDef): Unit = {
    val kernelName = syms.map(quote).mkString("")
    stream.println("final class activation_" + kernelName + " {")
    for (s <- syms) {
      stream.println("var " + quote(s) + ": " + remap(s.tp) + " = _")
    }
    stream.println("}")
  }
}