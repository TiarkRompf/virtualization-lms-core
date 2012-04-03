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
  
  // strong order for levelScope (as obtained by code motion), taking care of recursive dependencies.
  def getStronglySortedSchedule2(scope: List[Stm], level: List[Stm], result: Any): (List[Stm], List[Sym[Any]]) = {
    import util.GraphUtil
    import scala.collection.{mutable,immutable}

    def deps(st: List[Sym[Any]]): List[Stm] = 
      scope.filter(d => (st intersect d.lhs).nonEmpty)
    val fixed = new mutable.HashMap[Any,List[Sym[Any]]]
    def allSyms(r: Any) = fixed.getOrElse(r, syms(r) ++ softSyms(r))


    val inner = scope diff level // TODO: restrict to things referenced by functions (not ifs) ?

    var recursive: List[Sym[Any]] = Nil

    var xx = GraphUtil.stronglyConnectedComponents[Stm](deps(allSyms(result)), t => deps(allSyms(t.rhs)))    
    xx.foreach { xs => 
      if (xs.length > 1) {
        printdbg("warning: recursive schedule for result " + result + ": " + xs)

        // find things residing on top level
        val fs = (xs intersect level) flatMap (_.lhs)

        recursive = fs ::: recursive

        // eliminate all outward dependencies
        // CAVEAT: this *only* works for lambdas
        // problematic if sym is used both in a lambda and an if branch (may lead to NPE) 
        // TODO: restrict 'inner' to functions
        // CAVEAT: even for lambdas, this works *only* if the initialization happens before the first call
        // TODO: can we check that somehow? -- maybe insert a dep from the call
        (inner intersect xs) foreach {
          case stm if allSyms(stm.rhs) exists (fs contains _) => 
            fixed(stm.rhs) = allSyms(stm.rhs) filterNot (fs contains _)
            printdbg("fixing deps of " + stm.rhs + " to " + fixed(stm.rhs))
          case _ =>
        }
        
        // also remove direct inner deps (without inner stms): x1 = Lambda { x2 => Block(x3) }
        (level intersect xs) foreach {
          case stm if allSyms(blocks(stm.rhs)) exists (fs contains _) => 
            fixed(stm.rhs) = allSyms(stm.rhs) filterNot (fs contains _)
            printdbg("fixing deps of " + stm.rhs + " to " + fixed(stm.rhs))
          case _ =>
        }
      }
    }
    xx = GraphUtil.stronglyConnectedComponents[Stm](deps(allSyms(result) ++ allSyms(recursive)), t => deps(allSyms(t.rhs)))
    xx.foreach { xs => 
      if (xs.length > 1) {
        printerr("error: recursive schedule did not go away for result " + result + ": " + xs)
      }
    }
    val xxf = xx.flatten.reverse
    (xxf filter (level contains _), recursive)
  }
  
  var recursive: List[Sym[Any]] = Nil // FIXME: should propagate differently
  
  def focusExactScopeSubGraph[A](result: List[Exp[Any]])(body: List[Stm] => A): A = {
    val availDefs = availableDefs//getStronglySortedSchedule(availableDefs)(result) // resolve anti-dependencies (may still be recursive -- not sure whether that's a problem)
    val levelScope = getExactScope(availDefs)(result)
    val (levelScope2,recursive) = getStronglySortedSchedule2(availDefs,levelScope,result) // resolve anti-dependencies and recursive declarations
    withInnerScope(availDefs diff levelScope2) { // delay everything that remains
      val save = this.recursive
      this.recursive = recursive
      val r = body(levelScope2)
      this.recursive = save
      r
    }
  }

}