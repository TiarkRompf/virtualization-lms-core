package scala.virtualization.lms
package internal

trait FatExpressions extends Expressions {
  
  abstract class FatDef

  case class ThinDef(rhs: Def[Any]) extends FatDef

  case class TTP(val lhs: List[Sym[Any]], val rhs: FatDef)
  
}
