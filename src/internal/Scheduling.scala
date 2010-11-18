package scala.virtualization.lms
package internal

import util.GraphUtil

trait Scheduling {
  val IR: Expressions
  import IR._
  
  // these were previously in Expressions
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[_] => List(s)
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


  def availableDefs: List[TP[_]] = globalDefs
  

  def buildScheduleForResult(start: Exp[_]): List[TP[_]] = {
    def deps(st: List[Sym[_]]): List[TP[_]] =
      availableDefs.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    GraphUtil.stronglyConnectedComponents[TP[_]](deps(syms(start)), t => deps(syms(t.rhs))).flatten.reverse
  }  

  def getDependentStuff(st: List[Sym[_]]): List[TP[_]] = {
    st.flatMap(getDependentStuff).distinct
  }
    
  def getDependentStuff(st: Sym[_]): List[TP[_]] = {    
    def uses(s: Sym[_]): List[TP[_]] = {
      availableDefs.filter { d =>
        d.sym == s || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    GraphUtil.stronglyConnectedComponents[TP[_]](uses(st), t => uses(t.sym)).flatten
  }
  
}