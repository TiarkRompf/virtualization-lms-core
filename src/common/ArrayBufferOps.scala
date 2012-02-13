package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen
import collection.mutable.ArrayBuffer
import scala.reflect.SourceContext

trait ArrayBufferOps extends Base {

  object ArrayBuffer {
    def apply[A:Manifest](xs: Rep[A]*) = arraybuffer_new(xs)
  }

  implicit def repToArrayBufferOps[A:Manifest](l: Rep[ArrayBuffer[A]]) = new ArrayBufferOpsCls(l)
  
  class ArrayBufferOpsCls[A:Manifest](l: Rep[ArrayBuffer[A]]) {
    def +=(e: Rep[A])(implicit ctx: SourceContext) = arraybuffer_append(l,e)
    def mkString(sep: Rep[String] = unit(""))(implicit ctx: SourceContext) = arraybuffer_mkstring(l,sep)
    def append(l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit ctx: SourceContext) = arraybuffer_append(l,e)
    def toArray(implicit ctx: SourceContext) = arraybuffer_toarray(l)
    def toSeq(implicit ctx: SourceContext) = arraybuffer_toseq(l)
  }
  
  /* when mixed in with OptiML, one of these infix operations causes an NPE in the scala-virtualized compiler */
  /*
  def infix_mkString[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String] = unit(""))(implicit ctx: SourceContext) = arraybuffer_mkstring(l, sep)
  def infix_+=[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit ctx: SourceContext) = arraybuffer_append(l, e)
  def infix_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit ctx: SourceContext) = arraybuffer_append(l, e)
  def infix_toArray[A:Manifest](l: Rep[ArrayBuffer[A]])(implicit ctx: SourceContext) = arraybuffer_toarray(l)
  def infix_toSeq[A:Manifest](l: Rep[ArrayBuffer[A]])(implicit ctx: SourceContext) = arraybuffer_toseq(l)
  */
  
  def arraybuffer_mkstring[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def arraybuffer_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def arraybuffer_new[A:Manifest](xs: Seq[Rep[A]])(implicit ctx: SourceContext): Rep[ArrayBuffer[A]]
  def arraybuffer_toarray[A:Manifest](x: Rep[ArrayBuffer[A]])(implicit ctx: SourceContext): Rep[Array[A]]
  def arraybuffer_toseq[A:Manifest](x: Rep[ArrayBuffer[A]])(implicit ctx: SourceContext): Rep[Seq[A]]
}

trait ArrayBufferOpsExp extends ArrayBufferOps with EffectExp {
  case class ArrayBufferNew[A:Manifest](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    val mA = manifest[A]
  }
  case class ArrayBufferMkString[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class ArrayBufferAppend[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]
  case class ArrayBufferToArray[A:Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Array[A]]
  case class ArrayBufferToSeq[A:Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Seq[A]]

  def arraybuffer_new[A:Manifest](xs: Seq[Exp[A]])(implicit ctx: SourceContext) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_mkstring[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit ctx: SourceContext) = ArrayBufferMkString(l, sep)
  def arraybuffer_append[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit ctx: SourceContext) = reflectWrite(l)(ArrayBufferAppend(l, e))
  def arraybuffer_toarray[A:Manifest](x: Exp[ArrayBuffer[A]])(implicit ctx: SourceContext) = ArrayBufferToArray(x)
  def arraybuffer_toseq[A:Manifest](x: Exp[ArrayBuffer[A]])(implicit ctx: SourceContext) = ArrayBufferToSeq(x)

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

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case a@ArrayBufferNew(xs) => emitValDef(sym, "scala.collection.mutable.ArrayBuffer[" + remap(a.mA) + "](" + (xs map {quote}).mkString(",") + ")")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, quote(l) + ".mkString(" + quote(sep) + ")")
    case ArrayBufferAppend(l, e) => emitValDef(sym, quote(l) + " += " + quote(e))
    case ArrayBufferToArray(x) => emitValDef(sym, quote(x) + ".toArray")
    case ArrayBufferToSeq(x) => emitValDef(sym, quote(x) + ".toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayBufferOps extends BaseGenArrayBufferOps with CLikeGenBase {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayBufferOps extends CudaGenEffect with CLikeGenArrayBufferOps
trait OpenCLGenArrayBufferOps extends OpenCLGenEffect with CLikeGenArrayBufferOps
trait CGenArrayBufferOps extends CGenEffect with CLikeGenArrayBufferOps

