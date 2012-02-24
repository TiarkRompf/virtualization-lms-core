package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.reflect.SourceContext

trait MiscOps extends Base {
  /**
   * Other things that need to get lifted like exit, there should be
   * a better way to do this
   */

  def print(x: Rep[Any])(implicit ctx: SourceContext): Rep[Unit]
  def println(x: Rep[Any])(implicit ctx: SourceContext): Rep[Unit]

  // TODO: there is no way to override this behavior
  def exit(status: Int)(implicit ctx: SourceContext): Rep[Nothing] = exit(unit(status))
  def exit()(implicit ctx: SourceContext): Rep[Nothing] = exit(0)
  def exit(status: Rep[Int])(implicit ctx: SourceContext): Rep[Nothing]
  def error(s: Rep[String])(implicit ctx: SourceContext): Rep[Nothing]
  def returnL(x: Rep[Any])(implicit ctx: SourceContext): Rep[Unit]
}



trait MiscOpsExp extends MiscOps with EffectExp {
  case class Print(x: Exp[Any]) extends Def[Unit]
  case class PrintLn(x: Exp[Any]) extends Def[Unit]
  case class Exit(s: Exp[Int]) extends Def[Nothing]
  case class Error(s: Exp[String]) extends Def[Nothing]
  case class Return(x: Exp[Any]) extends Def[Unit]

  def print(x: Exp[Any])(implicit ctx: SourceContext) = reflectEffect(Print(x)) // TODO: simple effect
  def println(x: Exp[Any])(implicit ctx: SourceContext) = reflectEffect(PrintLn(x)) // TODO: simple effect
  def exit(s: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(Exit(s))
  def error(s: Exp[String])(implicit ctx: SourceContext) = reflectEffect(Error(s))
  def returnL(x: Exp[Any])(implicit ctx: SourceContext) = reflectEffect(Return(x))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(Print(x), u, es) => reflectMirrored(Reflect(Print(f(x)), mapOver(f,u), f(es)))
    case Reflect(PrintLn(x), u, es) => reflectMirrored(Reflect(PrintLn(f(x)), mapOver(f,u), f(es)))
    case Reflect(Exit(x), u, es) => reflectMirrored(Reflect(Exit(f(x)), mapOver(f,u), f(es)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenMiscOps extends ScalaGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case PrintLn(s) => emitValDef(sym, "println(" + quote(s) + ")")
    case Print(s) => emitValDef(sym, "print(" + quote(s) + ")")
    case Exit(a) => emitValDef(sym, "exit(" + quote(a) + ")")
    case Return(x) => emitValDef(sym, "return " + quote(x))
    case Error(s) => emitValDef(sym, "error(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CGenMiscOps extends CGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case PrintLn(s) => stream.println("printf(\"%s\\n\"," + quote(s) + ");")
    case Print(s) => stream.println("printf(\"%s\"," + quote(s) + ");")
    case Exit(a) => stream.println("exit(" + quote(a) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMiscOps extends CudaGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}


trait OpenCLGenMiscOps extends OpenCLGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
