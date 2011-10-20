package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen
import collection.mutable.ArrayBuffer
import scala.reflect.SourceContext

trait SynchronizedArrayBufferOps extends Base {

  object ArrayBuffer {
    def apply[A:Manifest](xs: Rep[A]*)(implicit ctx: SourceContext) = arraybuffer_new(xs)
  }

  def infix_mkString[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String] = unit(""))(implicit ctx: SourceContext) = arraybuffer_mkstring(l, sep)
  def infix_+=[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit ctx: SourceContext) = arraybuffer_append(l, e)
  def infix_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit ctx: SourceContext) = arraybuffer_append(l, e)

  def arraybuffer_mkstring[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def arraybuffer_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def arraybuffer_new[A:Manifest](xs: Seq[Rep[A]])(implicit ctx: SourceContext): Rep[ArrayBuffer[A]]
}

trait SynchronizedArrayBufferOpsExp extends SynchronizedArrayBufferOps with EffectExp {
  case class ArrayBufferNew[A:Manifest](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    val mA = manifest[A]
  }
  case class ArrayBufferMkString[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class ArrayBufferAppend[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]

  def arraybuffer_new[A:Manifest](xs: Seq[Exp[A]])(implicit ctx: SourceContext) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_mkstring[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit ctx: SourceContext) = ArrayBufferMkString(l, sep)
  def arraybuffer_append[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit ctx: SourceContext) = reflectWrite(l)(ArrayBufferAppend(l, e))

}

trait BaseGenSynchronizedArrayBufferOps extends GenericNestedCodegen {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._
}

trait ScalaGenSynchronizedArrayBufferOps extends BaseGenSynchronizedArrayBufferOps with ScalaGenEffect {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case a@ArrayBufferNew(xs) => emitValDef(sym, "(new scala.collection.mutable.ArrayBuffer[" + remap(a.mA) + "] with scala.collection.mutable.SynchronizedBuffer[" + remap(a.mA) + "]) ++= List(" + (xs map {quote}).mkString(",") + ")")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, quote(l) + ".mkString(" + quote(sep) + ")")
    case ArrayBufferAppend(l, e) => emitValDef(sym, quote(l) + " += " + quote(e))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSynchronizedArrayBufferOps extends BaseGenSynchronizedArrayBufferOps with CLikeGenBase {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenSynchronizedArrayBufferOps extends CudaGenEffect with CLikeGenSynchronizedArrayBufferOps
trait CGenSynchronizedArrayBufferOps extends CGenEffect with CLikeGenSynchronizedArrayBufferOps

