package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal._

trait SetOps extends Base {
  object Set {
    def apply[A:Manifest](xs: Rep[A]*) = set_new(xs)
  }

  implicit def repSetToSetOps[A:Manifest](v: Rep[Set[A]]) = new setOpsCls(v)

  class setOpsCls[A:Manifest](s: Rep[Set[A]]) {
    def contains(i: Rep[A]) = set_contains(s, i)
    def add(i: Rep[A]) = set_add(s, i)
    def remove(i: Rep[A]) = set_remove(s, i)
    def clear() = set_clear(s)
  }

  def set_new[A:Manifest](xs: Seq[Rep[A]]) : Rep[Set[A]]
  def set_contains[A:Manifest](s: Rep[Set[A]], i: Rep[A]) : Rep[Boolean]
  def set_add[A:Manifest](s: Rep[Set[A]], i: Rep[A]) : Rep[Unit]
  def set_remove[A:Manifest](s: Rep[Set[A]], i: Rep[A]) : Rep[Unit]
  def set_clear[A:Manifest](s: Rep[Set[A]]) : Rep[Unit]
}

trait SetOpsExp extends SetOps with EffectExp {
  case class SetNew[A:Manifest](xs: Seq[Rep[A]]) extends Def[Set[A]]
  case class SetContains[A:Manifest](s: Rep[Set[A]], i: Rep[A]) extends Def[Boolean]
  case class SetAdd[A:Manifest](s: Rep[Set[A]], i: Rep[A]) extends Def[Unit]
  case class SetRemove[A:Manifest](s: Rep[Set[A]], i: Rep[A]) extends Def[Unit]
  case class SetClear[A:Manifest](s: Rep[Set[A]]) extends Def[Unit]

  def set_new[A:Manifest](xs: Seq[Rep[A]]) = reflectEffect(SetNew(xs))
  def set_contains[A:Manifest](s: Rep[Set[A]], i: Rep[A]) = reflectEffect(SetContains(s, i))
  def set_add[A:Manifest](s: Rep[Set[A]], i: Rep[A]) = reflectEffect(SetAdd(s, i))
  def set_remove[A:Manifest](s: Rep[Set[A]], i: Rep[A]) = reflectEffect(SetRemove(s, i))
  def set_clear[A:Manifest](s: Rep[Set[A]]) = reflectEffect(SetClear(s))
}

trait BaseGenSetOps extends GenericNestedCodegen {
  val IR: SetOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case SetNew(xs) => (xs flatMap { syms }).toList
    case _ => super.syms(e)
  }

}

trait ScalaGenSetOps extends BaseGenSetOps with ScalaGenEffect {
  val IR: SetOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case SetNew(xs) => emitValDef(sym, "HashSet(" + (xs map {quote}).mkString(",") + ")")
    case SetContains(s,i) => emitValDef(sym, quote(s) + ".contains(" + quote(i) + ")")
    case SetAdd(s,i) => emitValDef(sym, quote(s) + ".add(" + quote(i) + ")")
    case SetRemove(s,i) => emitValDef(sym, quote(s) + ".remove(" + quote(i) + ")")
    case SetClear(s) => emitValDef(sym, quote(s) + ".clear()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSetOps extends BaseGenSetOps with CLikeCodegen {
  val IR: SetOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenSetOps extends CudaGenEffect with CLikeGenSetOps
trait CGenSetOps extends CGenEffect with CLikeGenSetOps