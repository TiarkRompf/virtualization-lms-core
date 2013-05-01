package scala.lms
package ops

import internal.GenericNestedCodegen

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.reflect.SourceContext

trait LiftArrayBufferType { this: Base =>
  implicit def liftArrayBuffer[T](implicit t: TypeRep[T]): TypeRep[ArrayBuffer[T]] = {
    implicit val mf = t.mf
    typeRep[ArrayBuffer[T]]
  }
}

trait ArrayBufferOps extends Base with LiftSeqType with LiftArrayBufferType with LiftArrayType {

  object ArrayBuffer {
    def apply[A:TypeRep](xs: Rep[A]*) = arraybuffer_new(xs)
  }

  implicit def repToArrayBufferOps[A:TypeRep](l: Rep[ArrayBuffer[A]]) = new ArrayBufferOpsCls(l)

  class ArrayBufferOpsCls[A:TypeRep](l: Rep[ArrayBuffer[A]]) {
    def +=(e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l,e)
    def mkString(sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l,sep)
    def append(l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l,e)
    def clear() = arraybuffer_clear(l)
    def toArray(implicit pos: SourceContext) = arraybuffer_toarray(l)
    def toSeq(implicit pos: SourceContext) = arraybuffer_toseq(l)
  }

  def infix_+=[A:TypeRep](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)

  /* when mixed in with OptiML, one of these infix operations causes an NPE in the scala-virtualized compiler */ //TR: still the case?
  /*
  def infix_mkString[A:TypeRep](l: Rep[ArrayBuffer[A]], sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l, sep)
  def infix_+=[A:TypeRep](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
  def infix_append[A:TypeRep](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
  def infix_toArray[A:TypeRep](l: Rep[ArrayBuffer[A]])(implicit pos: SourceContext) = arraybuffer_toarray(l)
  def infix_toSeq[A:TypeRep](l: Rep[ArrayBuffer[A]])(implicit pos: SourceContext) = arraybuffer_toseq(l)
  */

  def arraybuffer_mkstring[A:TypeRep](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit pos: SourceContext): Rep[String]
  def arraybuffer_append[A:TypeRep](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_new[A:TypeRep](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[ArrayBuffer[A]]
  def arraybuffer_clear[A:TypeRep](l: Rep[ArrayBuffer[A]]): Rep[Unit]
  def arraybuffer_toarray[A:TypeRep](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def arraybuffer_toseq[A:TypeRep](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Seq[A]]
}

trait ArrayBufferOpsExp extends ArrayBufferOps with EffectExp {
  case class ArrayBufferNew[A:TypeRep](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    val mA = typeRep[A]
  }
  case class ArrayBufferMkString[A:TypeRep](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class ArrayBufferAppend[A:TypeRep](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]
  case class ArrayBufferClear[A:TypeRep](l: Exp[ArrayBuffer[A]]) extends Def[Unit]
  case class ArrayBufferToArray[A:TypeRep](x: Exp[ArrayBuffer[A]]) extends Def[Array[A]]
  case class ArrayBufferToSeq[A:TypeRep](x: Exp[ArrayBuffer[A]]) extends Def[Seq[A]]

  def arraybuffer_new[A:TypeRep](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_mkstring[A:TypeRep](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit pos: SourceContext) = ArrayBufferMkString(l, sep)
  def arraybuffer_append[A:TypeRep](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferAppend(l, e))
  def arraybuffer_clear[A:TypeRep](l: Exp[ArrayBuffer[A]]) = reflectWrite(l)(ArrayBufferClear(l))
  def arraybuffer_toarray[A:TypeRep](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToArray(x)
  def arraybuffer_toseq[A:TypeRep](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToSeq(x)

  //////////////
  // mirroring

  override def mirrorDef[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayBufferMkString(l,r) => ArrayBufferMkString(f(l),f(r))
    case ArrayBufferAppend(l,r) => ArrayBufferAppend(f(l),f(r))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??

/*
  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ArrayBufferMkString(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferMkString(f(l),f(r)), mapOver(f,u), f(es)))(mtype(typeRep[A]))
    case Reflect(ArrayBufferAppend(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferAppend(f(l),f(r)), mapOver(f,u), f(es)))(mtype(typeRep[A]))
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
    case a@ArrayBufferNew(xs) => emitValDef(sym, "scala.collection.mutable.ArrayBuffer[" + remap(a.mA) + "](" + (xs map {quote}).mkString(",") + ")")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, quote(l) + ".mkString(" + quote(sep) + ")")
    case ArrayBufferAppend(l, e) => emitValDef(sym, quote(l) + " += " + quote(e))
    case ArrayBufferClear(l) => emitValDef(sym, quote(l) + ".clear()")
    case ArrayBufferToArray(x) => emitValDef(sym, quote(x) + ".toArray")
    case ArrayBufferToSeq(x) => emitValDef(sym, quote(x) + ".toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}
