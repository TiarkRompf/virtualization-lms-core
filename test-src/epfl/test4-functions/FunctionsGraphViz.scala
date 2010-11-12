package scala.virtualization.lms
package epfl
package test4

import internal.GraphVizExport

import test2._
import test3._

trait FunctionsGraphViz extends GraphVizExport {
  val IR: FunctionsExternalDef0
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: java.io.PrintWriter) = rhs match {
/*
    case Result(x) =>
      super.emitNode(sym, rhs)
      stream.println("shape=point")
    case Test(x, y) =>
      super.emitNode(sym, rhs)
      stream.println("color=red")
*/
    case DefineFun(x,arg) =>
      super.emitNode(sym, rhs)
      stream.println("color=green")
      stream.println("style=filled")
    case Apply(x, y) =>
      super.emitNode(sym, rhs)
      stream.println("color=blue")
      stream.println("style=filled")
    case _ =>
      super.emitNode(sym, rhs)
//      stream.println("shape=point")
  }
  
/*
  override def emitDeps(sym: Sym[_], rhs: Def[_], deps: List[Sym[Any]], stream: java.io.PrintWriter) = rhs match {
    case AndAlso(x, effects) =>
      super.emitDeps(sym, rhs, deps -- effects.asInstanceOf[List[Sym[Any]]]) // TODO: cast
      for (dep <- effects) {
        stream.println("\"" + dep + "\" -> \"" + sym + "\"" + " [color=red]")
      }
    case _ =>
      super.emitDeps(sym, rhs, deps)
  }
*/

}
