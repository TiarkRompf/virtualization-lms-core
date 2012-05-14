package scala.virtualization.lms
package internal

trait FatExpressions extends Expressions {
  
  abstract class FatDef

  case class ThinDef(rhs: Def[Any]) extends FatDef

  case class TTP(val lhs: List[Sym[Any]], val rhs: FatDef)



  case class Combine(a: List[Exp[Any]]) extends Exp[Any] //TODO: get rid of. used by emitFatBlock

  case class Forward[A](x: Exp[A]) extends Def[A] // TODO: get rid of. used by SimplifyTransform
  
}
