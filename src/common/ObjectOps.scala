package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

trait ObjectOps extends Variables with OverloadHack {
  def infix_toString(lhs: Rep[Any])(implicit ctx: SourceContext) = object_tostring(lhs)
  def infix_toStringL(lhs: Rep[Any])(implicit ctx: SourceContext) = object_tostring(lhs)
  def infix_unsafeImmutable[A:Manifest](lhs: Rep[A])(implicit ctx: SourceContext) = object_unsafe_immutable(lhs)

  def object_tostring(lhs: Rep[Any])(implicit ctx: SourceContext): Rep[String]
	def object_unsafe_immutable[A:Manifest](lhs: Rep[A])(implicit ctx: SourceContext): Rep[A]
}

trait ObjectOpsExp extends ObjectOps with VariablesExp {
  case class ObjectToString(o: Exp[Any]) extends Def[String]
  case class ObjectUnsafeImmutable[A](o: Exp[A]) extends Def[A]

  def object_tostring(lhs: Exp[Any])(implicit ctx: SourceContext) = ObjectToString(lhs)
  def object_unsafe_immutable[A:Manifest](lhs: Exp[A])(implicit ctx: SourceContext) = ObjectUnsafeImmutable(lhs)

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case ObjectUnsafeImmutable(a) => object_unsafe_immutable(f(a))
    case Reflect(ObjectUnsafeImmutable(a), u, es) => reflectMirrored(Reflect(ObjectUnsafeImmutable(f(a)), mapOver(f,u), f(es)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => syms(a)
    case _ => super.copySyms(e)
  }
}

trait ScalaGenObjectOps extends ScalaGenBase {
  val IR: ObjectOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ObjectToString(lhs) => emitValDef(sym, "(" + quote(lhs) + ").toString()")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenObjectOps extends CLikeGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ObjectToString(lhs) => emitValDef(sym, "(" + quote(lhs) + ").toString()")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenObjectOps extends CudaGenBase with CLikeGenObjectOps
trait OpenCLGenObjectOps extends OpenCLGenBase with CLikeGenObjectOps
trait CGenObjectOps extends CGenBase with CLikeGenObjectOps
