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

  def buildScheduleForResultM(scope: List[TP[Any]])(start: Any, cold: Boolean, hot: Boolean): List[TP[Any]] = {
    def mysyms(st: Any) = {
      val db = symsFreq(st).groupBy(_._1).mapValues(_.map(_._2).sum).toList
      assert(syms(st).toSet == db.map(_._1).toSet, "different list of syms: "+syms(st)+"!="+db)
      if (cold && hot) db.map(_._1)
      else if (cold && !hot) db.withFilter(_._2 < 100.0).map(_._1)
      else if (!cold && hot) db.withFilter(_._2 > 0.75).map(_._1)
      else db.withFilter(p=>p._2 > 0.75 && p._2 < 100.0).map(_._1)
    }
    
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      scope.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(mysyms(start)), t => deps(mysyms(t.rhs))).flatten.reverse
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
