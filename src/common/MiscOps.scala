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
  def printf(f: String, x: List[Rep[Any]])(implicit pos: SourceContext): Rep[Unit] = reflectEffect(PrintF(f, x))
  def exit(s: Exp[Int])(implicit pos: SourceContext) = reflectEffect(Exit(s))
  def error(s: Exp[String])(implicit pos: SourceContext) = reflectEffect(Error(s))
  def returnL(x: Exp[Any])(implicit pos: SourceContext) = {
    printlog("warning: staged return statements are unlikely to work because the surrounding source method does not exist in the generated code.")
    printsrc(raw"in ${quotePos(x)}")
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
    case PrintF(f,xs) => 
		stream.print("printf(\"\"\"" + f.replace("\\n","\n") + "\"\"\"")
		if (xs.size != 0) stream.print(xs.map(x => quote(x)).mkString(",",",",""))
		stream.println(")")
    case PrintLn(s) => gen"println($s)"
    case Print(s) => gen"print($s)"
    case Exit(a) => gen"exit($a)"
    case Return(x) => gen"return $x"
    case Error(s) => gen"error($s)"
    case _ => super.emitNode(sym, rhs)
  }
}


trait CGenMiscOps extends CGenEffect with GenericNestedCodegen {
  val IR: MiscOpsExp
  import IR._
  
/*  override def lowerNode[A:Manifest](sym: Sym[A], rhs: Def[A]) = rhs match {
	//TR case PrintLn(x) => sym.atPhase(LIRLowering) { println(LIRLowering(x)).asInstanceOf[Exp[A]] }
	case _ => super.lowerNode(sym, rhs)
  }
*/

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintF(f,xs) => gen"printf(${Const(f:String)::xs});"
    case PrintLn(s) => gen"""printf("%s\n",$s);"""
    case Print(s) => gen"""printf("%s",$s);"""
    case Exit(a) => gen"exit($a);"
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
