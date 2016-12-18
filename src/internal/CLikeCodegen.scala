package scala.virtualization.lms
package internal

import java.io.PrintWriter

trait CLikeCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._
/*
  //TODO: is sym of type Any or Variable[Any] ?
  def emitConstDef(sym: Sym[Any], rhs: String): Unit
*/
  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit
  def emitValDef(sym: Sym[Any], rhs: String): Unit
  def emitAssignment(sym: Sym[Any], lhs:String, rhs: String): Unit
  
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    val List(sym) = syms // TODO

    if( (vars.length>0) || (resultIsVar) ) throw new GenerationFailedException("Var is not supported for CPP kernels")

    val paramStr = vals.map(ele=>remap(ele.tp) + " " + quote(ele)).mkString(", ")
    stream.println("%s kernel_%s(%s) {".format(resultType, quote(sym), paramStr))
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
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

  def remapToJNI[A](m: Manifest[A]) : String = {
    remap(m) match {
      case "bool" => "Boolean"
      case "char" => "Byte"
      case "CHAR" => "Char"
      case "short" => "Short"
      case "int" => "Int"
      case "long" => "Long"
      case "float" => "Float"
      case "double" => "Double"
      case _ => throw new GenerationFailedException("GPUGen: Cannot get array creation JNI function for this type " + remap(m))
    }
  }


  // Map a scala primitive type to JNI type descriptor
  def JNITypeDescriptor[A](m: Manifest[A]) : String = m.toString match {
    case "Boolean" => "Z"
    case "Byte" => "B"
    case "Char" => "C"
    case "Short" => "S"
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case _ => throw new GenerationFailedException("Undefined GPU type")
  }



}

trait CLikeNestedCodegen extends GenericNestedCodegen with CLikeCodegen {
  val IR: Expressions with Effects with LoweringTransform
  import IR._
}

trait CLikeFatCodegen extends GenericFatCodegen with CLikeCodegen {
  val IR: Expressions with Effects with FatExpressions with LoweringTransform
  import IR._

  def emitMultiLoopCond(sym: Sym[Any], funcs:List[Block[Any]], idx: Sym[Int], postfix: String="", stream:PrintWriter):(String,List[Exp[Any]])

}
