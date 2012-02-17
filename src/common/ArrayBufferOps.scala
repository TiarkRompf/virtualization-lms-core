package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.internal.GenericNestedCodegen

trait ArrayBufferOps extends Base {

  object ArrayBuffer {
    def apply[A:Manifest](xs: Rep[A]*) = arraybuffer_new(xs)
  }

  def infix_mkString[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String] = unit("")) = arraybuffer_mkstring(l, sep)
  def infix_+=[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A]) = arraybuffer_append(l, e)
  def infix_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A]) = arraybuffer_append(l, e)

  def arraybuffer_mkstring[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String]): Rep[String]
  def arraybuffer_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A]): Rep[Unit]
  def arraybuffer_new[A:Manifest](xs: Seq[Rep[A]]): Rep[ArrayBuffer[A]]
}

trait ArrayBufferOpsExp extends ArrayBufferOps with EffectExp {
  case class ArrayBufferNew[A:Manifest](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    val mA = manifest[A]
  }
  case class ArrayBufferMkString[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class ArrayBufferAppend[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]

  def arraybuffer_new[A:Manifest](xs: Seq[Exp[A]]) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_mkstring[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) = ArrayBufferMkString(l, sep)
  def arraybuffer_append[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) = reflectWrite(l)(ArrayBufferAppend(l, e))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(ArrayBufferMkString(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferMkString(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ArrayBufferAppend(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferAppend(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait BaseGenArrayBufferOps extends GenericNestedCodegen {
  val IR: ArrayBufferOpsExp
  import IR._
}

trait ScalaGenArrayBufferOps extends BaseGenArrayBufferOps with ScalaGenEffect {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayBufferNew(xs) => emitValDef(sym, "scala.collection.mutable.ArrayBuffer[" + remap(a.mA) + "](" + (xs map {quote}).mkString(",") + ")")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, quote(l) + ".mkString(" + quote(sep) + ")")
    case ArrayBufferAppend(l, e) => emitValDef(sym, quote(l) + " += " + quote(e))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayBufferOps extends BaseGenArrayBufferOps with CLikeGenBase {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayBufferOps extends CudaGenEffect with CLikeGenArrayBufferOps
trait OpenCLGenArrayBufferOps extends OpenCLGenEffect with CLikeGenArrayBufferOps
trait CGenArrayBufferOps extends CGenEffect with CLikeGenArrayBufferOps

