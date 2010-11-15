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

  def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }

  def inputs(rhs: Def[_]) : List[Any] = rhs match {
    case p: Product => p.productIterator.toList
    case _ => Nil
  }

  def buildScheduleForResult(start: Exp[_]): List[TP[_]] = {
    val st = syms(start)
    GraphUtil.stronglyConnectedComponents[TP[_]](st.flatMap(e => findDefinition(e).toList), { d =>
      //println("dep"+d +"="+dep(d.rhs))
      dep(d.rhs).flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }).flatten.reverse // inefficient!
  }
    
}