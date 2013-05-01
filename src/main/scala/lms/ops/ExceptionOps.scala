package scala.lms
package ops

//import internal._

import java.io.PrintWriter
import scala.reflect.SourceContext

trait ExceptionOps extends Variables {
  // TODO: support virtualization of try-catch-finally blocks
  // for now, we only allow fatal errors (the exception will never be caught in generated code)

  def fatal(m: Rep[String]) = throw_exception(m)

  def throw_exception(m: Rep[String]): Rep[Nothing]
}

trait ExceptionOpsExp extends ExceptionOps with EffectExp {
  case class ThrowException(m: Rep[String]) extends Def[Nothing]

  def throw_exception(m: Exp[String]) = reflectEffect(ThrowException(m), Global())

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ThrowException(s), u, es) => reflectMirrored(Reflect(ThrowException(f(s)), mapOver(f,u), f(es)))(mtype(typeRep[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenExceptionOps extends ScalaGenBase {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ThrowException(m) => emitValDef(sym, "throw new Exception(" + quote(m) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

