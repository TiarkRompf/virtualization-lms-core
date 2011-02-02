package scala.virtualization.lms
package internal

import util.GraphUtil

trait Scheduling {
  val IR: Expressions
  import IR._
  
  // these were previously in Expressions
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }


  def dep(e: Exp[Any]): List[Sym[Any]] = e match { // only used by GraphVizExport currently
    case Def(d: Product) => syms(d)
    case _ => Nil
  }


  def availableDefs: List[TP[Any]] = globalDefs
  


  def buildScheduleForResult(start: Exp[Any]): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      availableDefs.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(syms(start)), t => deps(syms(t.rhs))).flatten.reverse
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