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


  def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }

  def buildScheduleForResult(start: Exp[_]): List[TP[_]] = {
    val st = syms(start)
    GraphUtil.stronglyConnectedComponents[TP[_]](st.flatMap(e => findDefinition(e).toList), { d =>
      //println("dep"+d +"="+dep(d.rhs))
      dep(d.rhs).flatMap { e => // TODO: what is dep(d.rhs), really? d.rhs is a Def, not an Exp! should it be syms(d.rhs) ?
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }).flatten.reverse // inefficient!
  }
  

  def getDependentStuff(st: List[Sym[_]]): List[TP[_]] = {
    st.flatMap(getDependentStuff).distinct
  }
    
  def getDependentStuff(st: Sym[_]): List[TP[_]] = {    
    def uses(s: Sym[_]): List[TP[_]] = {
      globalDefs.filter { d =>
        d.sym == s || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    
    GraphUtil.stronglyConnectedComponents[TP[_]](uses(st), { d =>
      uses(d.sym)
    }).flatten
  }
  
}