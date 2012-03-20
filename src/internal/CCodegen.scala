package scala.virtualization.lms
package internal

import java.io.PrintWriter


trait CCodegen extends CLikeCodegen {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "cpp"

  override def toString = "c"

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, out: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val x = fresh[A]
    val y = reifyBlock(f(x))

    val sA = mA.toString
    val sB = mB.toString

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  Emitting C Generated Code                  \n"+
                     "*******************************************/\n" +
                     "#include <stdio.h>\n" +
                     "#include <stdlib.h>"
      )

      //stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
      stream.println("int main(int argc, char** argv) {")

      emitBlock(y)
      //stream.println(quote(getBlockResult(y)))

      //stream.println("}")
      stream.println("}")
      stream.println("/*****************************************\n"+
                     "  End of C Generated Code                  \n"+
                     "*******************************************/")
    }
    Nil
  }  

/*
  //TODO: is sym of type Any or Variable[Any] ?
  def emitConstDef(sym: Sym[Any], rhs: String): Unit = {
    stream.print("const ")
    emitVarDef(sym, rhs)
  }
*/
  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String): Unit = {
    stream.println(lhs + " = " + rhs + ";")
  }

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Unit" => "void"
    case "java.lang.String" => "char *"
    case _ => throw new GenerationFailedException("CGen: remap(m) : Unknown data type (%s)".format(m.toString))
  }
  
}

// TODO: do we need this for each target?
trait CNestedCodegen extends GenericNestedCodegen with CCodegen {
  val IR: Expressions with Effects
  import IR._
  
}

trait CFatCodegen extends GenericFatCodegen with CCodegen {
  val IR: Expressions with Effects with FatExpressions
}