package scala.virtualization.lms
package internal

import util.GraphUtil


trait FatScheduling extends Scheduling {
  val IR: FatExpressions
  import IR._  
  
  import util.GraphUtil

  def fatten(e: TP[_]): TTP = TTP(List(e.sym), ThinDef(e.rhs))
  
  def getSchedule(scope: List[TP[_]])(result: Any): List[TP[_]] = {
    def deps(st: List[Sym[_]]): List[TP[_]] =
      scope.filter(st contains _.sym)

    GraphUtil.stronglyConnectedComponents[TP[_]](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatSchedule(scope: List[TTP])(result: Any): List[TTP] = {
    def deps(st: List[Sym[_]]): List[TTP] =
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[TTP](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatDependentStuff(scope: List[TTP])(st: List[Sym[_]]): List[TTP] = {
    st.flatMap(getFatDependentStuff0(scope)(_)).distinct
  }
    
  def getFatDependentStuff0(scope: List[TTP])(st: Sym[_]): List[TTP] = {
    def uses(s: Sym[_]): List[TTP] = {
      scope.filter { d =>
        (d.lhs contains s) || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    GraphUtil.stronglyConnectedComponents[TTP](uses(st), t => t.lhs flatMap uses).flatten
  }
    
}