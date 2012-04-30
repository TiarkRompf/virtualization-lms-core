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

  // Map a scala primitive type to JNI type descriptor
  def JNITypeDescriptor[A](m: Manifest[A]) : String = m.toString match {
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case "Boolean" => "Z"
    case _ => throw new GenerationFailedException("Undefined GPU type")
  }

  def isObjectType[A](m: Manifest[A]) : Boolean = {
    if (m.erasure == classOf[Variable[Any]] ) {
      true
    }
    else {
      m.toString match {
        case "scala.collection.immutable.List[Int]" => true
        case _ => false
      }
    }
  }

  def isPrimitiveType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Int" | "Long" | "Float" | "Double" | "Boolean"  => true
      case _ => false
    }
  }

  def isVoidType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Unit" => true
      case _ => false
    }
  }

  def isVariableType[A](m: Manifest[A]) : Boolean = {
    if(m.erasure == classOf[Variable[AnyVal]]) true
    else false
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit
  def emitValDef(sym: Sym[Any], rhs: String): Unit
  def emitAssignment(lhs:String, rhs: String): Unit
  
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {

    val paramStr = vals.map(ele=>remap(ele.tp) + " " + quote(ele)).mkString(", ")
    stream.println("%s kernel_%s(%s) {".format(resultType, syms.map(quote).mkString(""), paramStr))
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    
    if(resultType != "void")
      stream.println("return " + quote(syms(0)) + ";")
    stream.println("}")
  }
  
}
