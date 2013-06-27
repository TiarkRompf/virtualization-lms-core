package scala.virtualization.lms
package internal

import java.io.PrintWriter
import collection.mutable.HashSet

trait CLikeCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._

  def mangledName(name: String) = name.replaceAll("\\s","").map(c => if(!c.isDigit && !c.isLetter) '_' else c) 

  // List of datastructure types that requires transfer functions to be generated for this target
  protected val dsTypesList = HashSet[Manifest[Any]]()

  // Streams for helper functions and its header
  protected var helperFuncStream: PrintWriter = _
  protected var headerStream: PrintWriter = _
  protected var actRecordStream: PrintWriter = _
  protected var typesStream: PrintWriter = _

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = emitValDef(sym, rhs)

  def emitValDef(sym: Sym[Any], rhs: String): Unit = emitValDef(quote(sym), sym.tp, rhs)

  def emitValDef(sym: String, tpe: Manifest[_], rhs: String): Unit = {
    if(remap(tpe) != "void") stream.println(remap(tpe) + " " + sym + " = " + rhs + ";")
  }

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[AnyVal]])
      remap(m.typeArguments.head)
    else if (m.erasure == classOf[List[Any]]) { // Use case: Delite Foreach sync list 
      deviceTarget.toString + "List< " + remap(m.typeArguments.head) + " >"
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
        case "Nothing" => "void"
        case _ => throw new GenerationFailedException("CLikeGen: remap(m) : Type %s cannot be remapped.".format(m.toString))
      }
    }
  }

  def addRef[A](m: Manifest[A]): String = addRef(remap(m))

  def addRef(tpe: String): String = {
    if (!isPrimitiveType(tpe) && !isVoidType(tpe)) " *"
    else " " 
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {

    stream.append("#include \"" + deviceTarget + "helperFuncs.h\"\n")
    
    def kernelSignature: String = {
      val out = new StringBuilder
      if(resultIsVar)
        out.append(hostTarget + "Ref< " + resultType + addRef(resultType) + " > ")
      else
        out.append(resultType)
      if (!external) {
        if(resultIsVar) out.append(" *")
        else out.append(addRef(resultType))
      }
      out.append(" kernel_" + syms.map(quote).mkString("") + "(")
      out.append(vals.map(p=>remap(p.tp) + addRef(p.tp) + quote(p)).mkString(", "))
      if (vals.length > 0 && vars.length > 0){
        out.append(", ")
      }
      if (vars.length > 0){
        out.append(vars.map(v => hostTarget + "Ref< " + remap(v.tp) + addRef(v.tp) + " > *" + quote(v)).mkString(","))
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

  def isPrimitiveType(tpe: String) : Boolean = {
    tpe match {
      case "bool" | "char" | "CHAR" | "short" | "int" | "long" | "float" | "double" => true
      case _ => false
    }
  }

  def isVoidType(tpe: String) : Boolean = {
    if(tpe == "void") true
    else false
  }

  
  def CLikeConsts(x:Exp[Any], s:String): String = {
    s match {
      case "Infinity" => "std::numeric_limits<%s>::max()".format(remap(x.tp))
      case _ => super.quote(x)
    }
  }
  
  override def quote(x: Exp[Any]) = x match {
    case Const(s: Unit) => ""
    case Const(s: Float) => s+"f"
    case Const(null) => "NULL"
    case Const(z) => CLikeConsts(x, z.toString)
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
}

trait CLikeNestedCodegen extends GenericNestedCodegen with CLikeCodegen {
  val IR: Expressions with Effects
  import IR._
}

trait CLikeFatCodegen extends GenericFatCodegen with CLikeCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
}
