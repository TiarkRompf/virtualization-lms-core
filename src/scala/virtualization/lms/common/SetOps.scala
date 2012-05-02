package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.collection.mutable.Set
import scala.reflect.SourceContext

trait SetOps extends Base {
  object Set {
    def apply[A:Manifest](xs: Rep[A]*)(implicit ctx: SourceContext) = set_new[A](xs)
  }

  implicit def repSetToSetOps[A:Manifest](v: Rep[Set[A]]) = new setOpsCls(v)

  class setOpsCls[A:Manifest](s: Rep[Set[A]]) {
    def contains(i: Rep[A])(implicit ctx: SourceContext) = set_contains(s, i)
    def add(i: Rep[A])(implicit ctx: SourceContext) = set_add(s, i)
    def remove(i: Rep[A])(implicit ctx: SourceContext) = set_remove(s, i)
    def size(implicit ctx: SourceContext) = set_size(s)
    def clear()(implicit ctx: SourceContext) = set_clear(s)
  }

  def set_new[A:Manifest](xs: Seq[Rep[A]])(implicit ctx: SourceContext) : Rep[Set[A]]
  def set_contains[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit ctx: SourceContext) : Rep[Boolean]
  def set_add[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit ctx: SourceContext) : Rep[Unit]
  def set_remove[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit ctx: SourceContext) : Rep[Unit]
  def set_size[A:Manifest](s: Rep[Set[A]])(implicit ctx: SourceContext) : Rep[Int]
  def set_clear[A:Manifest](s: Rep[Set[A]])(implicit ctx: SourceContext) : Rep[Unit]
}

trait SetOpsExp extends SetOps with EffectExp {
  case class SetNew[A:Manifest](xs: Seq[Exp[A]], mA: Manifest[A]) extends Def[Set[A]]
  case class SetContains[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Boolean]
  case class SetAdd[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetRemove[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetSize[A:Manifest](s: Exp[Set[A]]) extends Def[Int]
  case class SetClear[A:Manifest](s: Exp[Set[A]]) extends Def[Unit]

  def set_new[A:Manifest](xs: Seq[Exp[A]])(implicit ctx: SourceContext) = reflectMutable(SetNew(xs, manifest[A]))
  def set_contains[A:Manifest](s: Exp[Set[A]], i: Exp[A])(implicit ctx: SourceContext) = SetContains(s, i)
  def set_add[A:Manifest](s: Exp[Set[A]], i: Exp[A])(implicit ctx: SourceContext) = reflectWrite(s)(SetAdd(s, i))
  def set_remove[A:Manifest](s: Exp[Set[A]], i: Exp[A])(implicit ctx: SourceContext) = reflectWrite(s)(SetRemove(s, i))
  def set_size[A:Manifest](s: Exp[Set[A]])(implicit ctx: SourceContext) = SetSize(s)
  def set_clear[A:Manifest](s: Exp[Set[A]])(implicit ctx: SourceContext) = reflectWrite(s)(SetClear(s))
}

trait BaseGenSetOps extends GenericNestedCodegen {
  val IR: SetOpsExp
  import IR._

}

trait ScalaGenSetOps extends BaseGenSetOps with ScalaGenEffect {
  val IR: SetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case SetNew(xs, mA) => emitValDef(sym, "collection.mutable.HashSet[" + remap(mA) + "](" + (xs map {quote}).mkString(",") + ")")
    case SetContains(s,i) => emitValDef(sym, quote(s) + ".contains(" + quote(i) + ")")
    case SetAdd(s,i) => emitValDef(sym, quote(s) + ".add(" + quote(i) + ")")
    case SetRemove(s,i) => emitValDef(sym, quote(s) + ".remove(" + quote(i) + ")")
    case SetSize(s) => emitValDef(sym, quote(s) + ".size")
    case SetClear(s) => emitValDef(sym, quote(s) + ".clear()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSetOps extends BaseGenSetOps with CLikeCodegen {
  val IR: SetOpsExp
  import IR._

//  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
}

trait CudaGenSetOps extends CudaGenEffect with CLikeGenSetOps
trait OpenCLGenSetOps extends OpenCLGenEffect with CLikeGenSetOps
trait CGenSetOps extends CGenEffect with CLikeGenSetOps
