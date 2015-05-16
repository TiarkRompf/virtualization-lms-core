package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

trait ObjectOps extends Variables with OverloadHack {
  def infix_toString(lhs: Rep[Any])(implicit pos: SourceContext) = object_tostring(lhs)
  def infix_ToString(lhs: Rep[Any])(implicit pos: SourceContext) = object_tostring(lhs)
  def infix_unsafeImmutable[A:Manifest](lhs: Rep[A])(implicit pos: SourceContext) = object_unsafe_immutable(lhs)
  def infix_unsafeMutable[A:Manifest](lhs: Rep[A])(implicit pos: SourceContext) = object_unsafe_mutable(lhs)

  def object_tostring(lhs: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def object_unsafe_immutable[A:Manifest](lhs: Rep[A])(implicit pos: SourceContext): Rep[A]
  def object_unsafe_mutable[A:Manifest](lhs: Rep[A])(implicit pos: SourceContext): Rep[A]
}

trait ObjectOpsExp extends ObjectOps with VariablesExp {
  case class ObjectToString(o: Exp[Any]) extends Def[String]
  case class ObjectUnsafeImmutable[A:Manifest](o: Exp[A]) extends Def[A] {
    val m = manifest[A]
  }
 case class ObjectUnsafeMutable[A:Manifest](o: Exp[A]) extends Def[A] {
   val m = manifest[A]
 }

  def object_tostring(lhs: Exp[Any])(implicit pos: SourceContext) = ObjectToString(lhs)
  def object_unsafe_immutable[A:Manifest](lhs: Exp[A])(implicit pos: SourceContext) = lhs match {
    // INVESTIGATE: there was an issue where Const(0).unsafeImmutable == Const(0.0). How is this possible? CSE with primitive widening?
    case c@Const(x) => c
    case _ => ObjectUnsafeImmutable(lhs)
  }
  def object_unsafe_mutable[A:Manifest](lhs: Exp[A])(implicit pos: SourceContext) = reflectMutable(ObjectUnsafeMutable(lhs))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ObjectUnsafeImmutable(a) => object_unsafe_immutable(f(a))(mtype(e.m),pos)
    case e@ObjectToString(a) => object_tostring(f(a))
    case Reflect(e@ObjectUnsafeImmutable(a), u, es) => reflectMirrored(Reflect(ObjectUnsafeImmutable(f(a))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@ObjectUnsafeMutable(a), u, es) => reflectMirrored(Reflect(ObjectUnsafeMutable(f(a))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
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

trait ObjectOpsExpOpt extends ObjectOpsExp {
  override def object_tostring(lhs: Exp[Any])(implicit pos: SourceContext) = {
    if (lhs.tp <:< manifest[String]) lhs.asInstanceOf[Exp[String]]
    else super.object_tostring(lhs)
  }
}

// This trait provides some useful additional short-circuits when calling object methods on structs.
// Since it brings in both Structs and Strings, we factor it out as a separate trait.
trait ObjectOpsStructExp extends ObjectOpsExpOpt with StructExp with StringOpsExp {

  override def object_tostring(x: Exp[Any])(implicit pos: SourceContext): Exp[String] = x match {
    case Def(s@Struct(tag, elems)) => //tag(elem1, elem2, ...)
      val e = elems.map(e=>string_plus(unit(e._1 + " = "), object_tostring(e._2))).reduceLeft((l,r)=>string_plus(string_plus(l,unit(", ")),r))
      string_plus(unit(structName(s.tp)+"("),string_plus(e,unit(")")))
    case _ => super.object_tostring(x)
  }

  def imm_field(struct: Exp[Any], name: String, f: Exp[Any])(implicit pos: SourceContext): Exp[Any] = {
    if (f.tp.erasure.getSimpleName == "Variable") {
      field(struct,name)(mtype(f.tp.typeArguments(0)),pos)
    }
    else {
      object_unsafe_immutable(f)(mtype(f.tp),pos)
    }
  }

  // don't let unsafeImmutable hide struct-ness
  override def object_unsafe_immutable[A:Manifest](lhs: Exp[A])(implicit pos: SourceContext) = lhs match {
    case Def(Struct(tag,elems)) => struct[A](tag, elems.map(t => (t._1, imm_field(lhs, t._1, t._2))))
    case Def(d@Reflect(Struct(tag, elems), u, es)) => struct[A](tag, elems.map(t => (t._1, imm_field(lhs, t._1, t._2))))
    case _ => super.object_unsafe_immutable(lhs)
  }
}

trait ScalaGenObjectOps extends ScalaGenBase {
  val IR: ObjectOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(lhs) => emitValDef(sym, src"($lhs).toString()")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, src"$x// unsafe immutable")
    case ObjectUnsafeMutable(x) => emitValDef(sym, src"$x// unsafe mutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait GPUGenObjectOps extends GPUGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectUnsafeImmutable(x) => 
      emitValDef(sym, quote(x) + "; // unsafe immutable")
      emitPtrDef(sym, x)
    case ObjectUnsafeMutable(x) => 
      emitValDef(sym, quote(x) + "; // unsafe mutable")
      emitPtrDef(sym, x)
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenObjectOps extends CLikeGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(x) => emitValDef(sym, src"($x).toString()")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, src"$x; // unsafe immutable")
    case ObjectUnsafeMutable(x) => emitValDef(sym, src"$x; // unsafe mutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenObjectOps extends CudaGenBase with CLikeGenObjectOps
trait OpenCLGenObjectOps extends OpenCLGenBase with CLikeGenObjectOps
trait CGenObjectOps extends CGenBase with CLikeGenObjectOps 
