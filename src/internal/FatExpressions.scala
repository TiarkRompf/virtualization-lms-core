package scala.virtualization.lms
package internal

trait FatExpressions extends Expressions {

  abstract class FatDef

  //case class ThinDef(rhs: Def[Any]) extends FatDef

  case class TTP(override val lhs: List[Sym[Any]], val mhs: List[Def[Any]], override val rhs: FatDef) extends Stm {
    def defines[A](s: Sym[A]): Option[Def[A]] = {
      val idx = lhs.indexOf(s)
      if (idx >= 0) Some(mhs(idx).asInstanceOf[Def[A]]) else None
    }
    def defines[A](d: Def[A]): Option[Sym[A]] = {
      val idx = mhs.indexOf(d)
      if (idx >= 0) Some(lhs(idx).asInstanceOf[Sym[A]]) else None
    }
  }

  case class Combine(a: List[Exp[Any]]) extends Exp[Any] //TODO: get rid of. used by emitFatBlock

  case class Forward[A](x: Exp[A]) extends Def[A] // TODO: get rid of. used by SimplifyTransform

}
