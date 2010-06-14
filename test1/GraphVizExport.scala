package test1

import java.io.{PrintWriter, FileOutputStream}

trait GraphVizExport extends Expressions with Scheduling {

  def quote(x: Any) = "\""+x+"\""
  
  def emitNode(sym: Sym[_], rhs: Def[_], stream: PrintWriter) = {
    stream.println("label=" + quote(sym + " \\n " + rhs))
    stream.println("shape=box")
  }

  def emitDeps(sym: Sym[_], rhs: Def[_], deps: List[Sym[Any]], stream: PrintWriter) = {
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

      emitNode(sym, rhs, stream)

      stream.println("]")
      
      emitDeps(sym, rhs, deps, stream)

    }

    stream.println("}")
    stream.close()
  }
 
  
  
}