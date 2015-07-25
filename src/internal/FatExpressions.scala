package scala.virtualization.lms
package internal

trait FatExpressions extends Expressions {
  
  abstract class FatDef

  //case class ThinDef(rhs: Def[Any]) extends FatDef

  case class TTP(val lhs: List[Sym[Any]], val mhs: List[Def[Any]], val rhs: FatDef) extends Stm

  override def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TTP(lhs, mhs, rhs) => lhs
    case _ => super.infix_lhs(stm)
  }

  override def infix_rhs(stm: Stm): Any = stm match {
    case TTP(lhs, mhs, rhs) => rhs
    case _ => super.infix_rhs(stm)
  }

  override def infix_defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TTP(lhs, mhs, rhs) => lhs.indexOf(sym) match { case idx if idx >= 0 => Some(mhs(idx).asInstanceOf[Def[A]]) case _ => None }
    case _ => super.infix_defines(stm, sym)
  }

  override def infix_defines[A](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TTP(lhs, mhs, rhs) => mhs.indexOf(rhs) match { case idx if idx >= 0 => Some(lhs(idx).asInstanceOf[Sym[A]]) case _ => None }
    case _ => super.infix_defines(stm, rhs)
  }



  case class Combine(a: List[Exp[Any]]) extends Exp[Any] //TODO: get rid of. used by emitFatBlock

  case class Forward[A](x: Exp[A]) extends Def[A] // TODO: get rid of. used by SimplifyTransform
  
  // TODO: No CSE here :(
  // TODO: Vera's code likely has some similar support for this (I assume)
  def createFatDefinition(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef): Stm = {
    val f = TTP(lhs, mhs, rhs)
    reflectSubGraph(List(f))
    f
  }

}
