package scala.virtualization.lms
package internal

import java.io.PrintWriter

trait CLikeCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._
/*
  //TODO: is sym of type Any or Variable[Any] ?
  def emitConstDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit
*/
  def emitVarDef(sym: Sym[Variable[Any]], rhs: String)(implicit stream: PrintWriter): Unit
  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit
  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit
  
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    val List(sym) = syms // TODO

    if( (vars.length>0) || (resultIsVar) ) throw new GenerationFailedException("Var is not supported for CPP kernels")

    val paramStr = vals.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    stream.println("%s kernel_%s(%s) {".format(resultType, quote(sym), paramStr))
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {
    val List(sym) = syms // TODO
    
    if(resultType != "void")
      stream.println("return " + quote(sym) + ";")
    stream.println("}")
  }
  
  def isObjectType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case _ => false
    }
  }

}

trait CLikeNestedCodegen extends GenericNestedCodegen with CLikeCodegen {
  val IR: Expressions with Effects
  import IR._
}

trait CLikeFatCodegen extends GenericFatCodegen with CLikeCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._

  def emitMultiLoopCond(sym: Sym[Any], funcs:List[Block[Any]], idx: Sym[Int], postfix: String="", stream:PrintWriter):(String,List[Exp[Any]])

}
