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

  // Streams for helper functions and its header
  protected var helperFuncStream: PrintWriter = _
  protected var headerStream: PrintWriter = _
  
  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if(!isVoidType(sym.tp)) 
      stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
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

  private def addRef[A](m: Manifest[A]): String = {
    if (!isPrimitiveType(m) && !isVoidType(m)) " *"
    else " "
  }
 
  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {

    stream.append("#include \"helperFuncs.h\"\n")
    
    def kernelSignature: String = {
      val out = new StringBuilder
      if(resultIsVar)
        out.append("Ref< " + resultType + " >")
      else
        out.append(resultType)
      if (!external) out.append(addRef(syms(0).tp))
      out.append(" kernel_" + syms.map(quote).mkString("") + "(")
      out.append(vals.map(p=>remap(p.tp) + addRef(p.tp) + quote(p)).mkString(", "))
      if (vals.length > 0 && vars.length > 0){
        out.append(", ")
      }
      if (vars.length > 0){
        out.append(vars.map(v => "Ref< " + remap(v.tp) + " > " + addRef(v.tp) + quote(v)).mkString(","))
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

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    //TODO: Remove the dependency to Multiloop to Delite
    if(resultType != "void" && !resultType.startsWith("DeliteOpMultiLoop"))
      stream.println("return " + quote(syms(0)) + ";")

    if(!resultType.startsWith("DeliteOpMultiLoop"))
      stream.println("}")

    dsTypesList ++= (syms++vals++vars).map(_.tp)
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
