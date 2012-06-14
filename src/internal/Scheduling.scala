package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import java.util.IdentityHashMap
import scala.collection.JavaConversions._

trait Scheduling {
  val IR: Expressions
  import IR._

  def getUnsortedSchedule(scope: List[Stm])(result: Any): List[Stm] = {
    getSchedule(scope)(result, false)
  }
  
  // PERFORMANCE: 'intersect' calls appear to be a hotspot

  // checks if a and b share at least one element. O(N^2), but with no allocation and possible early exit.
  def containsAny(a: List[Sym[Any]], b: List[Sym[Any]]): Boolean = {
    var aIter = a.iterator
    while (aIter.hasNext) {
      val aElem = aIter.next()
      val bIter = b.iterator
      while (bIter.hasNext) {
        if (bIter.next() eq aElem) return true
      }
    }
    false
  }
   
  def getStronglySortedSchedule(scope: List[Stm])(result: Any): List[Stm] = {
    def deps(st: List[Sym[Any]]): List[Stm] = 
      scope.filter(d => containsAny(st, d.lhs))
      // scope.filter(d => (st intersect d.lhs).nonEmpty)
    def allSyms(r: Any) = syms(r) ++ softSyms(r)
    
    val xx = GraphUtil.stronglyConnectedComponents[Stm](deps(allSyms(result)), t => deps(allSyms(t.rhs)))
    xx.foreach { x => 
      if (x.length > 1) {
        printerr("warning: recursive schedule for result " + result + ": " + x)
        (new Exception) printStackTrace
      }
    }
    xx.flatten.reverse
  }

  def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true): List[Stm] = {
    def deps(st: List[Sym[Any]]): List[Stm] = 
      scope.filter(d => containsAny(st, d.lhs))
      // scope.filter(d => (st intersect d.lhs).nonEmpty)

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
      scope.filter(d => containsAny(st, d.lhs))
      // scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[Stm](deps(mysyms(result)), t => deps(mysyms(t.rhs))).flatten.reverse
  }
    
  
  /** begin performance hotspot **/
  
  def getFatDependentStuff(scope: List[Stm])(sts: List[Sym[Any]]): List[Stm] = {
    /*
     precompute:
     s => all d in scope such that: d.lhs contains s || syms(d.rhs).contains(s)
     st => all d in scope such that: boundSyms(d.rhs) contains st
    */
    
    // IdentityHashMap appears faster than scala.collection.mutable.HashMap here (based on perf. testing)
    val lhsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val symsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val boundSymsCache = new IdentityHashMap[Sym[Any], Set[Stm]]()
    
    def getOrElse[K,V](map: IdentityHashMap[K, V], s: K)(f: => V) = {
      var res = map.get(s)
      if (res == null) res = f
      res
    }
    
    def putDef(map: IdentityHashMap[Sym[Any], List[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map.get(s)
      if (res == null) res = Nil
      // map.getOrElse(s, Nil) match {
      res match {
        case `d`::ds =>
        case ds => map.put(s,d::ds)
      }
    }
    
    def putDefSet(map: IdentityHashMap[Sym[Any], Set[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map.get(s)
      if (res == null) {
        res = Set[Stm]()
        map.put(s,res)
      }
      res += d
    }
    
    for (d <- scope) {
      d.lhs.foreach(s => putDef(lhsCache, s, d))
      syms(d.rhs).foreach(s => putDef(symsCache, s, d))      
      boundSyms(d.rhs).foreach(st => putDefSet(boundSymsCache, st, d))      
    }
            
    sts.distinct.flatMap { st =>
      // could also precalculate uses, but computing all combinations eagerly is also expensive
      def uses(s: Sym[Any]): List[Stm] = {
        getOrElse(lhsCache,s)(Nil) ::: getOrElse(symsCache,s)(Nil) filterNot (getOrElse(boundSymsCache,st)(Set.empty) contains _)
      }
      GraphUtil.stronglyConnectedComponents[Stm](
        uses(st),
        t => t match {
          case TP(s,d) => uses(s)
          case _ => t.lhs flatMap uses
        }).flatten
    }.distinct    
  }
  
  /** end performance hotspot **/

}