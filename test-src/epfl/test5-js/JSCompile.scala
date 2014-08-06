/*TODO DISABLED
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

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], methName: String, out: PrintWriter) = {
    withStream(out) {
      stream.println("function "+methName+"("+args.map(quote).mkString(", ")+") {")
    
      emitBlock(body)
      stream.println("return "+quote(getBlockResult(body)))
    
      stream.println("}")
    }
    Nil
  }
  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
}

trait JSNestedCodegen extends GenericNestedCodegen with JSCodegen {
  import IR._

}

trait JSGenBase extends JSCodegen {
  val IR: BaseExp
}

trait JSGenEffect extends JSNestedCodegen with JSGenBase {
  val IR: EffectExp  
}


trait JSGenIfThenElse extends BaseGenIfThenElse with JSGenEffect { // it's more or less generic...
  val IR: IfThenElseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
} 
*/
