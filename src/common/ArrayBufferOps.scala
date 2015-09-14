package scala.lms
package common

import scala.reflect.SourceContext
import scala.lms.codegen.GenericCodegen
import scala.lms.internal._

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

trait ArrayBufferOps extends Base with StringOps with ArrayOps {

  object ArrayBuffer {
    def apply[A:Typ](xs: Rep[A]*)(implicit ctx: SourceContext) = arraybuffer_new(xs)
  }

  implicit def arrayBufferTyp[T:Typ]: Typ[ArrayBuffer[T]]
  implicit def seqTyp[T:Typ]: Typ[Seq[T]] // TODO: remove?

  implicit def repToArrayBufferOps[A:Typ](l: Rep[ArrayBuffer[A]]) = new ArrayBufferOpsCls(l)

  class ArrayBufferOpsCls[A:Typ](l: Rep[ArrayBuffer[A]]) {
    def +=(e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l,e)
    def mkString(sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l,sep)
    def append(l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l,e)
    def clear() = arraybuffer_clear(l)
    def toArray(implicit pos: SourceContext) = arraybuffer_toarray(l)
    def toSeq(implicit pos: SourceContext) = arraybuffer_toseq(l)
  }

  def arraybuffer_mkstring[A:Typ](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit pos: SourceContext): Rep[String]
  def arraybuffer_append[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_new[A:Typ](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[ArrayBuffer[A]]
  def arraybuffer_clear[A:Typ](l: Rep[ArrayBuffer[A]]): Rep[Unit]
  def arraybuffer_toarray[A:Typ](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def arraybuffer_toseq[A:Typ](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Seq[A]]
}

trait ArrayBufferOpsExp extends ArrayBufferOps with BaseExp {
  implicit def arrayBufferTyp[T:Typ]: Typ[ArrayBuffer[T]] = {
    implicit val ManifestTyp(m) = typ[T]
    manifestTyp
  }

  case class ArrayBufferNew[A:Typ](xs: Seq[Exp[A]]) extends Def2[A, ArrayBuffer[A]]
  case class ArrayBufferMkString[A:Typ](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def2[A, String]
  case class ArrayBufferAppend[A:Typ](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def2[A, Unit]
  case class ArrayBufferClear[A:Typ](l: Exp[ArrayBuffer[A]]) extends Def2[A, Unit]
  case class ArrayBufferToArray[A:Typ](x: Exp[ArrayBuffer[A]]) extends Def2[A,Array[A]]
  case class ArrayBufferToSeq[A:Typ](x: Exp[ArrayBuffer[A]]) extends Def2[A, Seq[A]]

  def arraybuffer_new[A:Typ](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_mkstring[A:Typ](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit pos: SourceContext) = toAtom(ArrayBufferMkString(l, sep))
  def arraybuffer_append[A:Typ](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferAppend(l, e))
  def arraybuffer_clear[A:Typ](l: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferClear(l))
  def arraybuffer_toarray[A:Typ](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = toAtom(ArrayBufferToArray(x))
  def arraybuffer_toseq[A:Typ](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = toAtom(ArrayBufferToSeq(x))

  //////////////
  // mirroring

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@ArrayBufferNew(xs) => arraybuffer_new(f(xs))(mtype(e.mA),pos)
    case e@ArrayBufferMkString(l,r) => arraybuffer_mkstring(f(l),f(r))(mtype(e.mA),pos)
    case e@ArrayBufferAppend(l,r) => arraybuffer_append(f(l),f(r))(mtype(e.mA),pos)
    case e@ArrayBufferClear(l) => arraybuffer_clear(f(l))(mtype(e.mA),pos)
    case e@ArrayBufferToArray(x) => arraybuffer_toarray(f(x))(mtype(e.mA),pos)
    case e@ArrayBufferToSeq(x) => arraybuffer_toseq(f(x))(mtype(e.mA),pos)
    case _ => super.mirror(e,f)
  }
}

trait BaseGenArrayBufferOps extends GenericCodegen {
  val IR: ArrayBufferOpsExp
  import IR._
}

trait ScalaGenArrayBufferOps extends BaseGenArrayBufferOps with ScalaGenBase {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayBufferNew(xs) => emitValDef(sym, src"scala.collection.mutable.ArrayBuffer[${a.mA}](${(xs map {quote}).mkString(",")})")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, src"$l.mkString($sep)")
    case ArrayBufferAppend(l, e) => emitValDef(sym, src"$l += $e")
    case ArrayBufferClear(l) => emitValDef(sym, src"$l.clear()")
    case ArrayBufferToArray(x) => emitValDef(sym, src"$x.toArray")
    case ArrayBufferToSeq(x) => emitValDef(sym, src"$x.toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayBufferOps extends BaseGenArrayBufferOps with CLikeGenBase {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenArrayBufferOps extends CudaGenBase with CLikeGenArrayBufferOps
trait OpenCLGenArrayBufferOps extends OpenCLGenBase with CLikeGenArrayBufferOps
trait CGenArrayBufferOps extends CGenBase with CLikeGenArrayBufferOps

