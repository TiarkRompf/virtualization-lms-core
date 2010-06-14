package test5

import test1._
import test2._

import java.io.PrintWriter

trait JSCodegen extends GenericCodegen {
  def emitHTMLPage[B](f: () => Exp[B], stream: PrintWriter): Unit = {
    stream.println("<html><head><title>Scala2JS</title><script type=\"text/JavaScript\">")
    
    emitJSSource((x:Exp[Int]) => f(), "main", stream)
    
    stream.println("</script><body onload=\"main(0)\">")
    stream.println("</body></html>")
    stream.flush
  }

  def emitJSSource[A,B](f: Exp[A] => Exp[B], methName: String, stream: PrintWriter): Unit = {
    val x = fresh
    val y = f(x)

    stream.println("function "+methName+"("+quote(x)+") {")
    
    emitBlock(y, stream)
    stream.println("return "+quote(getBlockResult(y)))
    
    stream.println("}")
    stream.flush
  }
  def emitValDef(sym: Sym[_], rhs: String, stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
}

trait JSNestedCodegen extends GenericNestedCodegen with JSCodegen {
  override def emitJSSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter): Unit = {
    super.emitJSSource[A,B](x => reifyEffects(f(x)), className, stream)
  }
  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => error("Sym(-1) not supported")
    case _ => super.quote(x)
  }
}

trait JSCodegenBlockEffect extends JSNestedCodegen with BlockExpEffect {
  override def syms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }
  override def emitNode(sym: Sym[_], rhs: Def[_], stream: PrintWriter) = rhs match {
    case IfThenElse(c,a,b) =>  
      stream.println("var " + quote(sym))
      stream.println("if (" + quote(c) + ") {")
      emitBlock(a, stream)
      stream.println(quote(sym) + "=" + quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b, stream)
      stream.println(quote(sym) + "=" + quote(getBlockResult(b)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs, stream)
  }
}

trait JSCodegenArith extends JSCodegen { this: ArithExp => // TODO: define a generic one
  override def emitNode(sym: Sym[_], rhs: Def[_], stream: PrintWriter) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b), stream)
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b), stream)
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b), stream)
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b), stream)
    case _ => super.emitNode(sym, rhs, stream)
  }
} 
