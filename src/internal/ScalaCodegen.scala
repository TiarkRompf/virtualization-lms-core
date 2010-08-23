package scala.virtualization.lms
package internal

import java.io.PrintWriter


trait ScalaCodegen extends GenericCodegen {

  def emitScalaSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

    val x = fresh
    val y = f(x)

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting Generated Code                  \n"+
                   "*******************************************/")
    stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("def apply("+quote(x)+":"+sA+"): "+sB+" = {")
    
    emitBlock(y)(stream)
    stream.println(quote(getBlockResult(y)))
    
    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }


  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }

}

trait ScalaNestedCodegen extends GenericNestedCodegen with ScalaCodegen {
  
  override def emitScalaSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    super.emitScalaSource[A,B](x => reifyEffects(f(x)), className, stream)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}