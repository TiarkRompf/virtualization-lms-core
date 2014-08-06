/*TODO DISABLED
package scala.virtualization.lms
package epfl
package test3

import common._
import internal.GraphVizExport

import test1._
import test2._


trait ExtractorsGraphViz extends GraphVizExport {
  val IR: MatchingExtractorsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: java.io.PrintWriter) = rhs match {
/*
    case Result(x) =>
      super.emitNode(sym, rhs)
      stream.println("shape=point")
*/
    case Test(x, y) =>
      super.emitNode(sym, rhs)
      stream.println("color=red")
    case Deconstruct(x, y) =>
      super.emitNode(sym, rhs)
      stream.println("color=red")
//    case OrElse(_) =>
//      super.emitNode(sym, rhs)
//      stream.println("color=red")
    case _ =>
      super.emitNode(sym, rhs)
  }
  
  override def emitDeps(sym: Sym[Any], rhs: Def[Any], deps: List[Sym[Any]])(implicit stream: java.io.PrintWriter) = rhs match {

    case Reify(x, effects) =>
      super.emitDeps(sym, rhs, deps filterNot (effects.contains(_))) // TODO: use diff, but might have duplicates
      for (dep <- effects) {
        stream.println("\"" + dep + "\" -> \"" + sym + "\"" + " [color=red]")
      }
/*
      case OrElse(alts) =>
        val effects = alts flatMap { case Reify(p, es) => es }
        super.emitDeps(sym, rhs, deps filterNot (effects.contains(_))) // TODO: use diff, but might have duplicates
        for (dep <- effects) {
          stream.println("\"" + dep + "\" -> \"" + sym + "\"" + " [color=red]")
        }
*/

/*
    case Bind(x, effects) =>
      super.emitDeps(sym, rhs, deps filterNot (_ == effects)) // TODO: use diff, but might have duplicates
      for (dep <- List(effects)) {
        stream.println("\"" + dep + "\" -> \"" + sym + "\"" + " [color=red]")
      }
*/

/*
    case AndAlso(x, effects) =>
      super.emitDeps(sym, rhs, deps filterNot (effects.contains(_))) // TODO: use diff, but might have duplicates
      for (dep <- effects) {
        stream.println("\"" + dep + "\" -> \"" + sym + "\"" + " [color=red]")
      }
*/

    case _ =>
      super.emitDeps(sym, rhs, deps)
  }

}
*/
