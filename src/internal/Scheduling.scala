package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable.HashMap





trait Scheduling {
  val IR: Expressions
  import IR._

  def getUnsortedSchedule(scope: List[Stm])(result: Any): List[Stm] = {
    getSchedule(scope)(result, false)
  }
  
  def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true): List[Stm] = {
    def deps(st: List[Sym[Any]]): List[Stm] = 
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    val xx = GraphUtil.stronglyConnectedComponents[Stm](deps(syms(result)), t => deps(syms(t.rhs)))
    if (sort) xx.foreach { x => 
      if (x.length > 1) {
        printerr("warning: recursive schedule for result " + result + ": " + x)
        (new Exception) printStackTrace
      }
    }
    xx.flatten.reverse
  }

  def getScheduleM(scope: List[Stm])(result: Any, cold: Boolean, hot: Boolean): List[Stm] = {
    def mysyms(st: Any) = {
      val db = symsFreq(st).groupBy(_._1).mapValues(_.map(_._2).sum).toList
      assert(syms(st).toSet == db.map(_._1).toSet, "different list of syms: "+syms(st)+"!="+db+" for "+st)
      if (cold && hot) db.map(_._1)
      else if (cold && !hot) db.withFilter(_._2 < 100.0).map(_._1)
      else if (!cold && hot) db.withFilter(_._2 > 0.75).map(_._1)
      else db.withFilter(p=>p._2 > 0.75 && p._2 < 100.0).map(_._1)
    }

    def deps(st: List[Sym[Any]]): List[Stm] =
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[Stm](deps(mysyms(result)), t => deps(mysyms(t.rhs))).flatten.reverse
  }
    

  def getFatDependentStuff(scope: List[Stm])(sts: List[Sym[Any]]): List[Stm] = {
    /*
     precompute:
     s => all d in scope such that: d.lhs contains s || syms(d.rhs).contains(s)
     st => all d in scope such that: boundSyms(d.rhs) contains st
    */

    val lhsCache = new HashMap[Sym[Any], List[Stm]]
    val symsCache = new HashMap[Sym[Any], List[Stm]]
    val boundSymsCache = new HashMap[Sym[Any], List[Stm]]

    def putDef(map: HashMap[Sym[Any], List[Stm]], s: Sym[Any], d: Stm): Unit = {
      map.getOrElse(s, Nil) match {
        case `d`::ds =>
        case ds => map(s) = d::ds
      }
    }

    for (d <- scope) {
      d.lhs.foreach(s => putDef(lhsCache, s, d))
      syms(d.rhs).foreach(s => putDef(symsCache, s, d))
      boundSyms(d.rhs).foreach(st => putDef(boundSymsCache, st, d))
    }

    sts.distinct.flatMap { st =>
      // could precompute uses as well...
      def uses(s: Sym[Any]): List[Stm] = {
        lhsCache.getOrElse(s, Nil) ++ (symsCache.getOrElse(s, Nil) filterNot (boundSymsCache.getOrElse(st, Nil) contains _))
      }
      GraphUtil.stronglyConnectedComponents[Stm](uses(st), t => t.lhs flatMap uses).flatten
    }.distinct

//    st.distinct.flatMap(getFatDependentStuff0(scope)(_)).distinct // this is expensive!!
  }

}