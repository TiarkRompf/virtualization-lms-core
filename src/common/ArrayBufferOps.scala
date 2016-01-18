package scala.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.lms.internal.GenericNestedCodegen
import collection.mutable.ArrayBuffer
import scala.reflect.SourceContext

trait ArrayBufferOps extends Base with StringOps with ArrayOps {

  object ArrayBuffer {
    def apply[A:Typ](xs: Rep[A]*) = arraybuffer_new(xs)
  }

  implicit def arrayBufferTyp[T:Typ]: Typ[ArrayBuffer[T]]
  implicit def seqTyp[T:Typ]: Typ[Seq[T]] // TODO: remove?

  implicit def repToArrayBufferOps[A:Typ](l: Rep[ArrayBuffer[A]]) = new ArrayBufferOpsCls(l)
  
  class ArrayBufferOpsCls[A:Typ](l: Rep[ArrayBuffer[A]]) {
    def +=(e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l,e)
    def ++=(e: Rep[Array[A]])(implicit pos: SourceContext) = arraybuffer_append_array(l,e)
    def ++=(e: Rep[Seq[A]])(implicit o: Overloaded1, pos: SourceContext) = arraybuffer_append_seq(l,e)
    def mkString(sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l,sep)
    def append(e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l,e)
    def appendAll(e: Rep[Array[A]])(implicit pos: SourceContext) = arraybuffer_append_array(l,e)
    def appendAll(e: Rep[Seq[A]])(implicit o: Overloaded1, pos: SourceContext) = arraybuffer_append_seq(l,e)
    def clear() = arraybuffer_clear(l)
    def toArray(implicit pos: SourceContext) = arraybuffer_toarray(l)
    def toSeq(implicit pos: SourceContext) = arraybuffer_toseq(l)
  }
  
  def infix_+=[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)

  /* when mixed in with OptiML, one of these infix operations causes an NPE in the scala-virtualized compiler */ //TR: still the case?
  /*
  def infix_mkString[A:Typ](l: Rep[ArrayBuffer[A]], sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l, sep)
  def infix_+=[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
  def infix_append[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
  def infix_toArray[A:Typ](l: Rep[ArrayBuffer[A]])(implicit pos: SourceContext) = arraybuffer_toarray(l)
  def infix_toSeq[A:Typ](l: Rep[ArrayBuffer[A]])(implicit pos: SourceContext) = arraybuffer_toseq(l)
  */
  
  def arraybuffer_mkstring[A:Typ](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit pos: SourceContext): Rep[String]
  def arraybuffer_append[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_append_array[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[Array[A]])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_append_seq[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[Seq[A]])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_new[A:Typ](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[ArrayBuffer[A]]
  def arraybuffer_clear[A:Typ](l: Rep[ArrayBuffer[A]]): Rep[Unit]
  def arraybuffer_toarray[A:Typ](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def arraybuffer_toseq[A:Typ](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Seq[A]]
}

trait ArrayBufferOpsExp extends ArrayBufferOps with EffectExp {
  implicit def arrayBufferTyp[T:Typ]: Typ[ArrayBuffer[T]] = {
    implicit val ManifestTyp(m) = typ[T]
    manifestTyp
  }
  
  case class ArrayBufferNew[A:Typ](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    def mA = typ[A]
  }
  case class ArrayBufferMkString[A:Typ](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class ArrayBufferAppend[A:Typ](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]
  case class ArrayBufferAppendArray[A:Typ](l: Exp[ArrayBuffer[A]], e: Exp[Array[A]]) extends Def[Unit]
  case class ArrayBufferAppendSeq[A:Typ](l: Exp[ArrayBuffer[A]], e: Exp[Seq[A]]) extends Def[Unit]
  case class ArrayBufferClear[A:Typ](l: Exp[ArrayBuffer[A]]) extends Def[Unit]
  case class ArrayBufferToArray[A:Typ](x: Exp[ArrayBuffer[A]]) extends Def[Array[A]]
  case class ArrayBufferToSeq[A:Typ](x: Exp[ArrayBuffer[A]]) extends Def[Seq[A]]

  def arraybuffer_new[A:Typ](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_mkstring[A:Typ](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit pos: SourceContext) = ArrayBufferMkString(l, sep)
  def arraybuffer_append[A:Typ](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferAppend(l, e))
  def arraybuffer_append_array[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[Array[A]])(implicit pos: SourceContext): Rep[Unit] = reflectWrite(l)(ArrayBufferAppendArray(l, e))
  def arraybuffer_append_seq[A:Typ](l: Rep[ArrayBuffer[A]], e: Rep[Seq[A]])(implicit pos: SourceContext): Rep[Unit] = reflectWrite(l)(ArrayBufferAppendSeq(l, e))
  def arraybuffer_clear[A:Typ](l: Exp[ArrayBuffer[A]]) = reflectWrite(l)(ArrayBufferClear(l))
  def arraybuffer_toarray[A:Typ](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToArray(x)
  def arraybuffer_toseq[A:Typ](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToSeq(x)

  //////////////
  // mirroring

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayBufferMkString(l,r) => ArrayBufferMkString(f(l),f(r))(
      mtyp1[A])
    case ArrayBufferAppend(l,r) => ArrayBufferAppend(f(l),f(r))(mtyp1[A])
    case ArrayBufferAppendArray(l,r) => ArrayBufferAppendArray(f(l),f(r))(mtyp1[A])
    case ArrayBufferAppendSeq(l,r) => ArrayBufferAppendSeq(f(l),f(r))(mtyp1[A])
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??
  
/*
  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ArrayBufferMkString(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferMkString(f(l),f(r)), mapOver(f,u), f(es)))(mtyp1[A])
    case Reflect(ArrayBufferAppend(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferAppend(f(l),f(r)), mapOver(f,u), f(es)))(mtyp1[A])
    case Reflect(ArrayBufferAppendArray(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferAppendArray(f(l),f(r)), mapOver(f,u), f(es)))(mtyp1[A])
    case Reflect(ArrayBufferAppendSeq(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferAppendSeq(f(l),f(r)), mapOver(f,u), f(es)))(mtyp1[A])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
*/
}

trait BaseGenArrayBufferOps extends GenericNestedCodegen {
  val IR: ArrayBufferOpsExp
  import IR._
}

trait ScalaGenArrayBufferOps extends BaseGenArrayBufferOps with ScalaGenEffect {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayBufferNew(xs) => emitValDef(sym, src"scala.collection.mutable.ArrayBuffer[${a.mA}](${(xs map {quote}).mkString(",")})")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, src"$l.mkString($sep)")
    case ArrayBufferAppend(l, e) => emitValDef(sym, src"$l += $e")
    case ArrayBufferAppendArray(l, e) => emitValDef(sym, src"$l ++= $e")
    case ArrayBufferAppendSeq(l, e) => emitValDef(sym, src"$l ++= $e")
    case ArrayBufferClear(l) => emitValDef(sym, src"$l.clear()")
    case ArrayBufferToArray(x) => emitValDef(sym, src"$x.toArray")
    case ArrayBufferToSeq(x) => emitValDef(sym, src"$x.toSeq")
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

