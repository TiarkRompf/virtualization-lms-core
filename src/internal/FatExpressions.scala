package scala.virtualization.lms
package internal

trait FatExpressions extends Expressions {
  
  class FatDef(val elems: List[Def[_]])

  case class ThinDef(rhs: Def[_]) extends FatDef(List(rhs))

  case class TTP(val lhs: List[Sym[_]], val rhs: FatDef)  
  
}
