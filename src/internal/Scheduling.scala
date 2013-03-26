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
    var aIter = a
    while (aIter.nonEmpty) {
      val aElem = aIter.head
      aIter = aIter.tail
      var bIter = b
      while (bIter.nonEmpty) {
        if (bIter.head eq aElem) return true
        bIter = bIter.tail
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
  
  /*
  for each symbol s in sts, find all statements that depend on it.
  we need to stop when we reach the statement where s is bound.
  
  it would be tempting to do only one scc call but then we mix
  up the locations where different symbols are bound.
  */
  
  def getFatDependentStuff(scope: List[Stm])(sts: List[Sym[Any]]): List[Stm] = {
    /*
     precompute:
     s => all d in scope such that: d.lhs contains s || syms(d.rhs).contains(s)
     st => all d in scope such that: boundSyms(d.rhs) contains st
    */
    
    //type IdentityHashMap[K,V] = HashMap[K,V]
    
    // IdentityHashMap appears faster than scala.collection.mutable.HashMap here (based on perf. testing)
    // possible improvement: use an integer hashmap that works directly with sym ids
    
    val lhsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val symsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val boundSymsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    //val boundSymsCache = new IdentityHashMap[Sym[Any], Set[Stm]]()
    
    def infix_getOrElse[K,V](map: IdentityHashMap[K, V], s: K, f: => V) = {
      var res = map.get(s) //map(s)
      if (res == null) res = f
      res
    }
    
    def putDef(map: IdentityHashMap[Sym[Any], List[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map.get(s) //map(s)
      if (res == null) res = Nil
      //map.getOrElse(s, Nil) match {
      res match {
        case `d`::ds =>
        case ds => map.update(s,d::ds) //map.put(s,d::ds)
      }
    }
    
    def putDefSet(map: IdentityHashMap[Sym[Any], Set[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map(s) //map.get(s)
      if (res == null) {
        res = Set[Stm]()
        map.update(s,res) //map.put(s,res)
      }
      res += d
    }
    
    for (d <- scope) {
      d.lhs.foreach(s => putDef(lhsCache, s, d))
      syms(d.rhs).foreach(s => putDef(symsCache, s, d))      
      boundSyms(d.rhs).foreach(st => putDef(boundSymsCache, st, d))
      tunnelSyms(d.rhs).foreach(st => putDef(boundSymsCache, st, d)) // treat tunnel like bound
    }
    
    /*
    optimization idea:
      topologically sort sts
      find dependent stuff for first s in sts, save as R
      start looking at next s in sts, but stop when hittin a stm that is already in R
      continue
    
    hopefully this will prevent traversing lots of things multiple times.
    why does it work?

    the assumption is that a stm st when depending on sym s1 (that comes topologically 
    before s2) also entails all the dependent stuff it does when depending on sym s2. 

    this assumption would be violated if the frontier stopped earlier when triggered
    by s1 than when triggered from s2. 
    
    this could be the case if s1 in boundSyms(x1) and s2 in boundSyms(x2) (and only for x2) where
    there is a path s1 ... x1 ... x2

    a consequence is that the last one before x2 cannot already be in R (otherwise R would 
    contain all dep stuff)

    can this be tested (easily)?

    -----------
    
    new optimization idea, based on this observation:

      topologically sort sts
      find dependent stuff for first s in sts, save as R
      consider next s in sts
        if all stms with s in boundSyms can be reached in 1 step from R, do nothing
        else find dependent stuff, add to R
      continue


      condition: all stms with s in boundSyms can be reached in 1 step from R
                 R contains at least one sym from each stm with s in boundSyms
    */
    
    sts.distinct.flatMap { st =>
      // could also precalculate uses, but computing all combinations eagerly is also expensive
      def uses(s: Sym[Any]): List[Stm] = {
        lhsCache.getOrElse(s,Nil) ::: symsCache.getOrElse(s,Nil) filterNot (boundSymsCache.getOrElse(st, Nil) contains _)
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