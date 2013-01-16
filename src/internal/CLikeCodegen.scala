package scala.virtualization.lms
package internal

import java.io.PrintWriter
import collection.mutable.HashSet

trait CLikeCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._

  def mangledName(name: String) = name.map(c => if(!c.isDigit && !c.isLetter) '_' else c) 

  // List of datastructure types that requires transfer functions to be generated for this target
  protected val dsTypesList = HashSet[Manifest[Any]]()

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if(!isVoidType(sym.tp)) 
      stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String): Unit = {
    stream.println(lhs + " = " + rhs + ";")
  }

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[AnyVal]])
      remap(m.typeArguments.head)
    else if (m.erasure == classOf[List[Any]]) { // Use case: Delite Foreach sync list 
      "List< " + remap(m.typeArguments.head) + " >"
    }
    else {
      m.toString match {
        case "scala.collection.immutable.List[Float]" => "List"
        case "Boolean" => "bool"
        case "Byte" => "char"
        case "Char" => "CHAR"
        case "Short" => "short"
        case "Int" => "int"
        case "Long" => "long"
        case "Float" => "float"
        case "Double" => "double"
        case "Unit" => "void"
        case _ => throw new GenerationFailedException("CLikeGen: remap(m) : Type %s cannot be remapped.".format(m.toString))
      }
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
