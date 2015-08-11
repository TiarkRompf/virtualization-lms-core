package scala.lms
package common

import java.io.PrintWriter
import internal._
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
  
  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ThrowException(s), u, es) => reflectMirrored(Reflect(ThrowException(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)     
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  
}

trait ScalaGenExceptionOps extends ScalaGenBase {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ThrowException(m) => emitValDef(sym, src"throw new Exception($m)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenExceptionOps extends CLikeGenBase {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ThrowException(m) => 
      stream.println("printf(" + quote(m) + ".c_str());")
      stream.println("assert(false);")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenExceptionOps extends CGenBase with CLikeGenExceptionOps
trait CudaGenExceptionOps extends CudaGenBase with CLikeGenExceptionOps {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ThrowException(m) =>
      stream.println("printf(" + quote(m) + ");")
      stream.println("assert(false);")
    case _ => super.emitNode(sym, rhs)
  }
}
//OpenCL does not support printf within a kernel
//trait OpenCLGenExceptionOps extends OpenCLGenBase with CLikeGenExceptionOps
