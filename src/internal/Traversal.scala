package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


// traversals are stateful (scheduling is stateless)

trait GraphTraversal extends Scheduling {
  val IR: Expressions
  import IR._
  
  def availableDefs: List[Stm] = globalDefs
  
  def buildScheduleForResult(result: Any, sort: Boolean = true): List[Stm] = 
    getSchedule(availableDefs)(result, sort)

  def getDependentStuff(st: List[Sym[Any]]): List[Stm] = {
    getFatDependentStuff(availableDefs)(st)
  }

  def getDependentStuff(st: Sym[Any]): List[Stm] = {
    getDependentStuff(List(st))
  }

}


trait NestedGraphTraversal extends GraphTraversal with CodeMotion {
  val IR: Expressions with Effects /* effects just for sanity check */
  import IR._
  
  // ----- stateful focus management

//  var outerScope: List[TP[Any]] = Nil
//  var levelScope: List[TP[Any]] = Nil
  var innerScope: List[Stm] = null  // no, it's not a typo

  def initialDefs = super.availableDefs

  override def availableDefs = if (innerScope ne null) innerScope else initialDefs

  def withInnerScope[A](scope: List[Stm])(body: => A): A = {
//    val saveOuter = outerScope
//    val saveLevel = levelScope
    val saveInner = innerScope

//    outerScope = outerScope ::: levelScope
//    levelScope = Nil
    innerScope = scope

    var rval = null.asInstanceOf[A]
    try {
      rval = body
    }
    catch {
      case e => throw e
    }
    finally {
      innerScope = saveInner
    }

//    outerScope = saveOuter
//    levelScope = saveLevel
//    innerScope = saveInner

    rval
  }


  // ----- stateful focus management

  def focusSubGraph[A](result: List[Exp[Any]])(body: => A): A = {
    withInnerScope(buildScheduleForResult(result, false)) { // deep list of deps (unsorted, possibly recursive)
      body
    }
  }
  
  def focusExactScopeSubGraph[A](result: List[Exp[Any]])(body: List[Stm] => A): A = {
    val availDefs = availableDefs
    val levelScope = getExactScope(availDefs)(result)
    withInnerScope(availDefs diff levelScope) { // delay everything that remains
      body(levelScope)
    }
  }

}