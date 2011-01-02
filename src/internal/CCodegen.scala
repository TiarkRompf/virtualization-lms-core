package scala.virtualization.lms
package internal

import java.io.PrintWriter


trait CCodegen extends CLikeCodegen with GenericCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cpp"

  override def toString = "c"

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    val x = fresh[A]
    val y = f(x)

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting C Generated Code                  \n"+
                   "*******************************************/\n" +
                   "#include <stdio.h>\n" +
                   "#include <stdlib.h>"
    )

    //stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("int main(int argc, char** argv) {")

    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))

    //stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of C Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }  

  def emitConstDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }

  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(remap(sym.Type) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs + ";")
  }

  override def emitKernelHeader(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {

    if( (vars.length>0) || (resultIsVar) ) throw new RuntimeException("Var is not supported for CPP kernels")

    val paramStr = vals.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")
    stream.println("%s kernel_%s(%s) {".format(resultType, quote(sym), paramStr))
  }

  override def emitKernelFooter(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {
    if(resultType != "void")
      stream.println("return " + quote(sym) + ";")
    stream.println("}")
  }

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Unit" => "void"
    case "java.lang.String" => "char *"
    case _ => throw new RuntimeException("CGen: remap(m) : Unknown data type (%s)".format(m.toString))
  }
  
}

// TODO: do we need this for each target?
trait CNestedCodegen extends GenericNestedCodegen with CCodegen {
  val IR: Expressions with Effects
  import IR._
  
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    super.emitSource[A,B](x => reifyEffects(f(x)), className, stream)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}

trait CGenBase extends CCodegen {
  import IR._

}

trait CGenEffect extends CNestedCodegen with CGenBase {

}