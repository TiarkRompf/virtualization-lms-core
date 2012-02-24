package scala.virtualization.lms
package internal

import java.io.{PrintWriter, FileOutputStream}

trait GraphVizExport extends Scheduling {
  val IR: Expressions
  import IR._

  def quote(x: Any) = "\""+x+"\""
  
  def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    stream.println("label=" + quote(sym + " \\n " + rhs))
    stream.println("shape=box")
  }

  def emitDeps(sym: Sym[Any], rhs: Def[Any], deps: List[Sym[Any]])(implicit stream: PrintWriter) = {
    for (dep <- deps) {
      stream.println("\"" + dep + "\" -> \"" + sym + "\"")
    }
  }

  def emitDepGraph(start: Exp[Any], file: String, landscape: Boolean = false): Unit =
    emitDepGraph(start, new java.io.PrintWriter(new java.io.FileOutputStream(file)), landscape)

  def emitDepGraph(start: Exp[Any], stream: PrintWriter, landscape: Boolean): Unit = {

    stream.println("digraph G {")

    val deflist = buildScheduleForResult(start)

    if (landscape)
      stream.println("rankdir=LR")

    for (TP(sym, rhs) <- deflist) {

      val deps = dep(rhs)

      stream.println(quote(sym) + " [")

      // all

      emitNode(sym, rhs)(stream)

      stream.println("]")
      
      emitDeps(sym, rhs, deps)(stream)

    }

    stream.println("}")
    stream.close()
  }
 
  
  
}