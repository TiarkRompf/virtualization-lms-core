package scala.virtualization.lms
package ppl

import java.io.{BufferedReader, FileReader, PrintWriter}

import common._
import util.OverloadHack

trait ScalaOps extends Base with OverloadHack {
  
  /**
   * Other things that need to get lifted like exit, there should be
   * a better way to do this
   */

  def print(x: Rep[Any]): Rep[Unit]
  def println(x: Rep[Any]): Rep[Unit]

  // TODO: there is no way to override this behavior
  def exit(status: Int): Rep[Nothing] = exit(unit(status))
  def exit(): Rep[Nothing] = exit(0)
  def exit(status: Rep[Int]): Rep[Nothing]  
}



trait ScalaOpsExp extends ScalaOps with BaseExp { this: FunctionsExp =>

  case class Print(x: Exp[Any]) extends Def[Unit]
  case class Exit(s: Exp[Int]) extends Def[Nothing]

  def print(x: Rep[Any]) = reflectEffect(Print(x))
  def println(x: Rep[Any]) = reflectEffect(Print(x))
  def exit(s: Rep[Int]) = reflectEffect(Exit(s))
}

  
trait ScalaGenScalaOps extends ScalaGenEffect  { this: ScalaOpsExp =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Print(s) => emitValDef(sym, "println(" + quote(s) + ")")
    case Exit(a) => emitValDef(sym, "exit(" + quote(a) + ")")

    case _ => super.emitNode(sym, rhs)
  }
                                         
}
