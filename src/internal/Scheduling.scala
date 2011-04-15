package scala.virtualization.lms
package internal

import util.GraphUtil

trait Scheduling {
  val IR: Expressions
  import IR._
  
  def dep(e: Def[Any]): List[Sym[Any]] = e match { // only used by GraphVizExport currently
    case d: Product => syms(d)
    case _ => Nil
  }


  def availableDefs: List[TP[Any]] = globalDefs
  


  def buildScheduleForResult(start: Exp[Any]): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      availableDefs.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(syms(start)), t => deps(syms(t.rhs))).flatten.reverse
  }  

  def buildScheduleForResultM(defs: List[TP[Any]])(start: Any, cold: Boolean, hot: Boolean): List[TP[Any]] = {
    def mysyms(st: Any) = if (cold && hot) syms(st)
      else if (cold && !hot) syms(st) diff hotSyms(st)
      else if (!cold && hot) syms(st) diff coldSyms(st)
      else syms(st) diff coldSyms(st) diff hotSyms(st)
    
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      defs.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(mysyms(start,cold,hot)), t => deps(mysyms(t.rhs,cold,hot))).flatten.reverse
  }  



  def getDependentStuff(st: List[Sym[Any]]): List[TP[Any]] = {
    // TODO: expensive! should do scc calculation only once
    st.distinct.flatMap(getDependentStuff).distinct
  }
    
  def getDependentStuff(st: Sym[Any]): List[TP[Any]] = {
    def uses(s: Sym[Any]): List[TP[Any]] = {
      availableDefs.filter { d =>
        d.sym == s || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    GraphUtil.stronglyConnectedComponents[TP[Any]](uses(st), t => uses(t.sym)).flatten
  }

}