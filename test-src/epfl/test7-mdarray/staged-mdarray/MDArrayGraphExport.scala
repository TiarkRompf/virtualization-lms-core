package scala.virtualization.lms
package epfl
package test7

import internal.GraphVizExport
import java.io.PrintWriter

trait MDArrayGraphExport extends GraphVizExport {

  import IR._

  def emitTypingString(i: Int): String

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    stream.println("label=" + quote(sym + " with " + emitTypingString(sym.id) + " \\n " + rhs))
    stream.println("shape=box")
  }
}