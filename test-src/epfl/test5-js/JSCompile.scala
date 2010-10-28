package scala.virtualization.lms
package epfl
package test5

import common._
import internal._
import test1._

import java.io.PrintWriter

trait JSCodegen extends GenericCodegen {
  import IR._
  
  def emitHTMLPage[B](f: () => Exp[B], stream: PrintWriter)(implicit mB: Manifest[B]): Unit = {
    stream.println("<html><head><title>Scala2JS</title><script type=\"text/JavaScript\">")
    
    emitSource((x:Exp[Int]) => f(), "main", stream)
    
    stream.println("</script><body onload=\"main(0)\">")
    stream.println("</body></html>")
    stream.flush
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], methName: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    val x = fresh
    val y = f(x)

    stream.println("function "+methName+"("+quote(x)+") {")
    
    emitBlock(y)(stream)
    stream.println("return "+quote(getBlockResult(y)))
    
    stream.println("}")
    stream.flush
  }
  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
}

trait JSNestedCodegen extends GenericNestedCodegen with JSCodegen {
  import IR._

  // TODO: we shouldn't need the manifests here (aks)
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    super.emitSource[A,B](x => reifyEffects(f(x)), className, stream)
  }
  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => error("Sym(-1) not supported")
    case _ => super.quote(x)
  }
}

trait JSGenBase extends JSCodegen {
  val IR: BaseExp
}

trait JSGenEffect extends JSNestedCodegen with JSGenBase {
  val IR: EffectExp  
}


trait JSGenIfThenElse extends JSGenEffect { // it's more or less generic...
  val IR: IfThenElseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case IfThenElse(c,a,b) =>  
      stream.println("var " + quote(sym))
      stream.println("if (" + quote(c) + ") {")
      emitBlock(a)
      stream.println(quote(sym) + "=" + quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b)
      stream.println(quote(sym) + "=" + quote(getBlockResult(b)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenArith extends JSGenBase { // TODO: define a generic one
  val IR: ArithExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
} 
