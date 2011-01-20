package scala.virtualization.lms
package internal

import util.GraphUtil


trait FatScheduling extends Scheduling {
  val IR: FatExpressions
  import IR._  
  
  import util.GraphUtil

  def fatten(e: TP[Any]): TTP = TTP(List(e.sym), ThinDef(e.rhs))
  
  def getSchedule(scope: List[TP[Any]])(result: Any): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      scope.filter(st contains _.sym)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatSchedule(scope: List[TTP])(result: Any): List[TTP] = {
    def deps(st: List[Sym[Any]]): List[TTP] =
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[TTP](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatDependentStuff(scope: List[TTP])(st: List[Sym[Any]]): List[TTP] = {
    st.flatMap(getFatDependentStuff0(scope)(_)).distinct
  }
    
  def getFatDependentStuff0(scope: List[TTP])(st: Sym[Any]): List[TTP] = {
    def uses(s: Sym[Any]): List[TTP] = {
      scope.filter { d =>
        (d.lhs contains s) || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    GraphUtil.stronglyConnectedComponents[TTP](uses(st), t => t.lhs flatMap uses).flatten
  }
    
}