package scala.lms
package internal

trait FatExpressions extends Expressions {
  
  abstract class FatDef

  //case class ThinDef(rhs: Def[Any]) extends FatDef

  case class TTP(tps: List[TP[Any]], rhs: FatDef) extends Stm

  override def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TTP(tps, _) => tps.map(_.sym)
    case _ => super.infix_lhs(stm)
  }

  override def infix_rhs(stm: Stm): Any = stm match {
    case TTP(_, rhs) => rhs
    case _ => super.infix_rhs(stm)
  }

  override def infix_defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TTP(tps, rhs) =>
      tps.flatMap {
        case tp => tp.defines(sym)
      }.headOption
    case _ => super.infix_defines(stm, sym)
  }

  override def infix_defines[A: Manifest](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TTP(tps, _) =>
      tps.flatMap {
        // compiler fails to resolve the correct infix_defines if manifest[A] is left implicit
        case tp => tp.defines(rhs)(manifest[A])
      }.headOption
    case _ => super.infix_defines(stm, rhs)
  }


  case class Combine(a: List[Exp[Any]]) extends Exp[Any] //TODO: get rid of. used by emitFatBlock

  case class Forward[A](x: Exp[A]) extends Def[A] // TODO: get rid of. used by SimplifyTransform
  
}
