package scala.virtualization.lms
package common.embedded.scala

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CGenBase, ScalaGenBase, Effects}
import scala.virtualization.lms.common.{EffectExp, Base}

trait MiscOps extends Base {
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



trait MiscOpsExp extends MiscOps with EffectExp {
  case class Print(x: Exp[Any]) extends Def[Unit]
  case class PrintLn(x: Exp[Any]) extends Def[Unit]
  case class Exit(s: Exp[Int]) extends Def[Nothing]

  def print(x: Rep[Any]) = reflectEffect(Print(x))
  def println(x: Rep[Any]) = reflectEffect(PrintLn(x))
  def exit(s: Rep[Int]) = reflectEffect(Exit(s))
}

trait ScalaGenMiscOps extends ScalaGenBase {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case PrintLn(s) => emitValDef(sym, "println(" + quote(s) + ")")
    case Print(s) => emitValDef(sym, "print(" + quote(s) + ")")
    case Exit(a) => emitValDef(sym, "exit(" + quote(a) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


//todo factor out commonality
trait CGenMiscOps extends CGenBase {
  val IR: MiscOpsExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case PrintLn(s) => stream.println("printf(\"%s\\n\"," + quote(s) + ");")
    case Print(s) => stream.println("printf(\"%s\"," + quote(s) + ");")
    case Exit(a) => stream.println("exit(" + quote(a) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}
