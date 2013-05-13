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

  def print(x: Rep[Any])(implicit pos: SourceContext): Rep[Unit]
  def println(x: Rep[Any])(implicit pos: SourceContext): Rep[Unit]
  def printf(f: String, x: Rep[Any]*)(implicit pos: SourceContext): Rep[Unit]

  // TODO: there is no way to override this behavior
  def exit(status: Int)(implicit pos: SourceContext): Rep[Nothing] = exit(unit(status))
  def exit()(implicit pos: SourceContext): Rep[Nothing] = exit(0)
  def exit(status: Rep[Int])(implicit pos: SourceContext): Rep[Nothing]
  def error(s: Rep[String])(implicit pos: SourceContext): Rep[Nothing]
  def returnL(x: Rep[Any])(implicit pos: SourceContext): Rep[Unit]
}



trait MiscOpsExp extends MiscOps with EffectExp {
  case class Print(x: Exp[Any]) extends Def[Unit]
  case class PrintLn(x: Exp[Any]) extends Def[Unit]
  case class PrintF(f: String, x: List[Exp[Any]]) extends Def[Unit]
  case class Exit(s: Exp[Int]) extends Def[Nothing]
  case class Error(s: Exp[String]) extends Def[Nothing]
  case class Return(x: Exp[Any]) extends Def[Unit]

  def print(x: Exp[Any])(implicit pos: SourceContext) = reflectEffect(Print(x)) // TODO: simple effect
  def println(x: Exp[Any])(implicit pos: SourceContext) = reflectEffect(PrintLn(x)) // TODO: simple effect
  def printf(f: String, x: Rep[Any]*)(implicit pos: SourceContext): Rep[Unit] = reflectEffect(PrintF(f, x.toList))
  def exit(s: Exp[Int])(implicit pos: SourceContext) = reflectEffect(Exit(s))
  def error(s: Exp[String])(implicit pos: SourceContext) = reflectEffect(Error(s))
  def returnL(x: Exp[Any])(implicit pos: SourceContext) = {
    printlog("warning: staged return statements are unlikely to work because the surrounding source method does not exist in the generated code.")
    printsrc("in " + quotePos(x))
    reflectEffect(Return(x))
  }
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(Error(x), u, es) => reflectMirrored(Reflect(Error(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(Print(x), u, es) => reflectMirrored(Reflect(Print(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(PrintLn(x), u, es) => reflectMirrored(Reflect(PrintLn(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(PrintF(fm,x), u, es) => reflectMirrored(Reflect(PrintF(fm,f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(Exit(x), u, es) => reflectMirrored(Reflect(Exit(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(Return(x), u, es) => reflectMirrored(Reflect(Return(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenMiscOps extends ScalaGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintF(f,x) => emitValDef(sym, "printf(" + (f::x.map(quote)).mkString(",") + ")")
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

  private def format(s: Exp[Any]): String = {
    remap(s.tp) match {
      case "CHAR" => "%c"
      case "bool" | "char" | "short" | "int" => "%d"
      case "long" => "%ld"
      case "float" | "double" => "%f"
      case "string" => "%s" 
      case _ => throw new GenerationFailedException("CGenMiscOps: cannot print type " + remap(s.tp))
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintF(f,x) => emitValDef(sym, "printf(" + ((Const(f:String)::x).map(quote)).mkString(",") + ")")
    case PrintLn(s) => stream.println("printf(\"" + format(s) + "\\n\"," + quote(s) + ");")
    case Print(s) => stream.println("printf(\"" + format(s) + "\"," + quote(s) + ");")
    case Exit(a) => stream.println("exit(" + quote(a) + ");")
    case Return(x) => stream.println("return " + quote(x) + ";")
    case Error(s) => stream.println("error(-1,0,\"%s\"," + quote(s) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMiscOps extends CudaGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}


trait OpenCLGenMiscOps extends OpenCLGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
