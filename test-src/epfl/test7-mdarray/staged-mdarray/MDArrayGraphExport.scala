package scala.virtualization.lms
package epfl
package test7

import internal.GraphVizExport
import java.io.PrintWriter
import common.BaseGenIfThenElse

trait MDArrayGraphExport extends BaseGenIfThenElse with TypedGenMDArray {

  import IR._

  var indent: Int = -1
  val MAX_INDENT = 5

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    emitIndentNode(sym, rhs)
    // now, for more complex stuff:
    rhs match {
      case GenArrayWith(wList, shape) =>
        for (withExpr <- wList)
          emitBlock(withExpr)
      case ModArrayWith(wList, array) =>
        for (withExpr <- wList)
          emitBlock(withExpr)
      case FoldArrayWith(wExpr, neutral, foldTerm1, foldTerm2, foldExpression) =>
        emitBlock(wExpr)
        emitBlock(foldExpression)
      case wn: WithNode[_] =>
        emitBlock(wn.expr)
      case ite: IfThenElse[_] =>
        TY.withinDifferentScopes(
          (ite.thenp.asInstanceOf[Sym[_]], () => {emitBlock(ite.thenp)})::
          (ite.elsep.asInstanceOf[Sym[_]], () => {emitBlock(ite.elsep)})::
          Nil)
      case _ =>
        // do nothing :)
    }
  }

  override def emitBlock(result: Exp[Any])(implicit stream: PrintWriter): Unit = {
    indent = indent + 1
    super.emitBlock(result)
    indent = indent - 1
  }


  def emitIndentNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    stream.println("\"" + sym + "\" [")
    stream.println("label=\"" + sym + " with " + TY.getTypingString(sym) + " \\n " + rhs + "\"")
    stream.println("shape=box\nstyle=filled")
    val colorValue = (indent < MAX_INDENT) match {
      case true => 1.0 - indent.toDouble / (2 * MAX_INDENT)
      case false => 0.5
    }
    val colorValue255 = (colorValue * 255).toInt.asInstanceOf[AnyRef]
    val colorString = String.format("#%02x%02x%02x", colorValue255, colorValue255, colorValue255)
    stream.println("color=black\nfillcolor=\"" + colorString + "\"")
    stream.println("]")
    for (d <- dep(rhs))
      stream.println("\"" + d + "\" -> \"" + sym + "\"")
  }

  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit =
    sys.error("Not implemented!")

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit =
    sys.error("Not implemented!")


  def emitDepGraph(start: Exp[Any], stream: PrintWriter, landscape: Boolean): Unit = {

    stream.println("digraph G {")

    emitBlock(start)(stream)

    stream.println("}")
    stream.close()
  }


}