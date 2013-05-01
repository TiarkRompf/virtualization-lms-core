package scala.lms
package ops

import internal._

import java.io.PrintWriter
import scala.collection.mutable.Set
import scala.reflect.SourceContext

trait LiftSetType { this: Base =>
  implicit def liftSet[T](implicit t: TypeRep[T]): TypeRep[Set[T]] = {
       implicit val mf = t.mf
       typeRep[Set[T]]
  }
 /* implicit def liftSetMutable[T](implicit t: TypeRep[T]): TypeRep[scala.collection.mutable.Set[T]] = {
     implicit val mf = t.mf
     typeRep[scala.collection.mutable.Set[T]]
  }*/
}

trait SetOps extends Base with LiftSeqType with LiftSetType {
  object Set {
    def apply[A:TypeRep](xs: Rep[A]*)(implicit pos: SourceContext) = set_new[A](xs)
  }

  implicit def repSetToSetOps[A:TypeRep](v: Rep[Set[A]]) = new setOpsCls(v)

  class setOpsCls[A:TypeRep](s: Rep[Set[A]]) {
    def contains(i: Rep[A])(implicit pos: SourceContext) = set_contains(s, i)
    def add(i: Rep[A])(implicit pos: SourceContext) = set_add(s, i)
    def remove(i: Rep[A])(implicit pos: SourceContext) = set_remove(s, i)
    def size(implicit pos: SourceContext) = set_size(s)
    def clear()(implicit pos: SourceContext) = set_clear(s)
    def toSeq(implicit pos: SourceContext) = set_toseq(s)
    def toArray(implicit pos: SourceContext) = set_toarray(s)
  }

  def set_new[A:TypeRep](xs: Seq[Rep[A]])(implicit pos: SourceContext) : Rep[Set[A]]
  def set_contains[A:TypeRep](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Boolean]
  def set_add[A:TypeRep](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Unit]
  def set_remove[A:TypeRep](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Unit]
  def set_size[A:TypeRep](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Int]
  def set_clear[A:TypeRep](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Unit]
  def set_toseq[A:TypeRep](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def set_toarray[A:TypeRep](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Array[A]]
}

trait SetOpsExp extends SetOps with ArrayOps with EffectExp {
  case class SetNew[A:TypeRep](xs: Seq[Exp[A]], mA: TypeRep[A]) extends Def[Set[A]]
  case class SetContains[A:TypeRep](s: Exp[Set[A]], i: Exp[A]) extends Def[Boolean]
  case class SetAdd[A:TypeRep](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetRemove[A:TypeRep](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetSize[A:TypeRep](s: Exp[Set[A]]) extends Def[Int]
  case class SetClear[A:TypeRep](s: Exp[Set[A]]) extends Def[Unit]
  case class SetToSeq[A:TypeRep](s: Exp[Set[A]]) extends Def[Seq[A]]
  case class SetToArray[A:TypeRep](s: Exp[Set[A]]) extends Def[Array[A]] {
    //val array = unit(typeRep[A].newArray(0))
    val array = NewArray[A](s.size)
  }

  def set_new[A:TypeRep](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(SetNew(xs, typeRep[A]))
  def set_contains[A:TypeRep](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = SetContains(s, i)
  def set_add[A:TypeRep](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = reflectWrite(s)(SetAdd(s, i))
  def set_remove[A:TypeRep](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = reflectWrite(s)(SetRemove(s, i))
  def set_size[A:TypeRep](s: Exp[Set[A]])(implicit pos: SourceContext) = SetSize(s)
  def set_clear[A:TypeRep](s: Exp[Set[A]])(implicit pos: SourceContext) = reflectWrite(s)(SetClear(s))
  def set_toseq[A:TypeRep](s: Exp[Set[A]])(implicit pos: SourceContext) = SetToSeq(s)
  def set_toarray[A:TypeRep](s: Exp[Set[A]])(implicit pos: SourceContext) = SetToArray(s)
}

trait BaseGenSetOps extends GenericNestedCodegen {
  val IR: SetOpsExp
  import IR._

}

trait ScalaGenSetOps extends BaseGenSetOps with ScalaGenEffect {
  val IR: SetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SetNew(xs, mA) => emitValDef(sym, "collection.mutable.HashSet[" + remap(mA) + "](" + (xs map {quote}).mkString(",") + ")")
    case SetContains(s,i) => emitValDef(sym, quote(s) + ".contains(" + quote(i) + ")")
    case SetAdd(s,i) => emitValDef(sym, quote(s) + ".add(" + quote(i) + ")")
    case SetRemove(s,i) => emitValDef(sym, quote(s) + ".remove(" + quote(i) + ")")
    case SetSize(s) => emitValDef(sym, quote(s) + ".size")
    case SetClear(s) => emitValDef(sym, quote(s) + ".clear()")
    case SetToSeq(s) => emitValDef(sym, quote(s) + ".toSeq")
    case n@SetToArray(s) => //emitValDef(sym, quote(s) + ".toArray")
      stream.println("// workaround for refinedManifest problem")
      stream.println("val " + quote(sym) + " = {")
      stream.println("val out = " + quote(n.array))
      stream.println("val in = " + quote(s) + ".toSeq")
      stream.println("var i = 0")
      stream.println("while (i < in.length) {")
      stream.println("out(i) = in(i)")
      stream.println("i += 1")
      stream.println("}")
      stream.println("out")
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}
