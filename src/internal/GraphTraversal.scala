package scala.lms
package internal

// Traversals are stateful (scheduling is stateless)
trait GraphTraversal extends Scheduling with CodeMotion {
  val IR: BaseExp
  import IR._
  
  // --- State
  var recursive: List[Sym[Any]] = Nil // FIXME: Should propagate differently
  var innerScope: List[Stm] = null
  
  def availableDefs = if (innerScope ne null) innerScope else globalDefs
  
  // --- State-Dependent Scheduling Shortcuts
  def getDependents(syms: List[Sym[Any]]): List[Stm] = getFatDependents(availableDefs)(syms)
  def getDependents(sym: Sym[Any]): List[Stm] = getDependents(List(sym))
  def buildScheduleForResult(result: Any, sort: Boolean = true): List[Stm] 
    = getSchedule(availableDefs)(result, sort)
  
  /**
   * Execute body statement with given Stm list as inner scope
   **/ 
  def withInnerScope[A](scope: List[Stm])(body: => A): A = {
    val saveInner = innerScope
    innerScope = scope
    var rval = null.asInstanceOf[A]
    try { rval = body }
    catch {case e: Throwable => throw e }
    finally { innerScope = saveInner }
    (rval)
  }
   
  def focusSubGraph[A](result: List[Exp[Any]])(body: => A): A = {
    val schedule = buildScheduleForResult(result, false)
    if (schedule.isEmpty && result.exists{case Def(Reify(_,_,_)) => true case _ => false}) {
      cwarn("Empty schedule for syms: \n\t" + result.map(strDef(_)).mkString("\n\t"))
      cwarn("In scope: \n\t" + (if (availableDefs ne null) availableDefs.mkString("\n\t") else "null"))
    }
    withInnerScope(schedule) { body }
  }
  
  def focusExactScopeSubGraph[A](result: List[Exp[Any]])(body: List[Stm] => A): A = {
    val availDefs = availableDefs
    val levelScope = getExactScope(availDefs)(result)
    val (levelScope2,recurse2) = getStronglySortedSchedule(availDefs,levelScope,result)
    withInnerScope(availDefs diff levelScope2) {
      val save = recursive
      recursive = recurse2
      val r = body(levelScope2)
      recursive = save
      (r)
    }
  }
}
