package scala.lms
package internal

import scala.lms.util.GraphUtil
import scala.collection.mutable
import java.util.IdentityHashMap
import scala.collection.JavaConversions._

trait Scheduling {
  val IR: BaseExp
  import IR._
  
  def fatten(e: Stm): Stm = e
  def fattenAll(e: List[Stm]) = e.map(fatten)
  
  def getStronglySortedSchedule(scope: List[Stm], level: List[Stm], result: Any): (List[Stm], List[Sym[Any]]) = {
    val scopeIndex = buildScopeIndex(scope)
    
    val fixed = new collection.mutable.HashMap[Any,List[Sym[Any]]]
    def allSyms(r: Any) = fixed.getOrElse(r, syms(r) ++ softSyms(r))
    
    val inner = scope diff level
    var recursive: List[Sym[Any]] = Nil
    
    var xx = GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(allSyms(result), scopeIndex), 
    			t => scheduleDepsWithIndex(allSyms(t.rhs), scopeIndex))
    
    // Fix recursion in graph
    xx.foreach{ xs =>
      if (xs.length > 1 && (xs intersect level).nonEmpty) {
        cwarn(s"Recursive schedule for result $result: $xs")
        
        val fs = (xs intersect level) flatMap (_.lhs)
        recursive = fs ::: recursive
        
        // Eliminate all outward dependencies
        // CAVEAT: This *only* works for lambdas if initialization happens before first call
        // problematic if sym is used in both lambda and an if branch (may lead to NPE)
        // TODO: Restrict 'inner' to functions
        // TODO: Check order somehow? Maybe insert a dep from the call site?
        (inner intersect xs) foreach {
          case stm if allSyms(stm.rhs) exists (fs contains _) =>
            fixed(stm.rhs) = allSyms(stm.rhs) filterNot (fs contains _)
            cdbg("Fixing deps of " + stm.rhs + " to " + fixed(stm.rhs))
          case _ => 
        }
        (level intersect xs) foreach {
          case stm if allSyms(blocks(stm.rhs)) exists (fs contains _) => 
            fixed(stm.rhs) = allSyms(stm.rhs) filterNot (fs contains _)
            cdbg("Fixing deps of " + stm.rhs + " to " + fixed(stm.rhs)) 
          case _ =>
        }
      }
    }
    xx = GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(allSyms(result) ++ allSyms(recursive), scopeIndex), 
    			t => scheduleDepsWithIndex(allSyms(t.rhs), scopeIndex))
    
    xx.foreach{ xs =>
      if (xs.length > 1 && (xs intersect level).nonEmpty) {
        cerror(s"Recursive schedule did not go away for result $result: $xs")
      }
    }
    			
    val xxf = xx.flatten.reverse
    (xxf filter (level contains _), recursive)
  }
  
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

  //performance hotspot!
  //should be O(1) wrt 'scope' (nodes in graph), try to keep this as efficient as possible
  protected def scheduleDepsWithIndex(syms: List[Sym[Any]], cache: IdentityHashMap[Sym[Any], (Stm,Int)]): List[Stm] = {
    //syms.map(cache.get(_)).filter(_ ne null).distinct.sortBy(_._2).map(_._1)
    val sortedSet = new java.util.TreeSet[(Stm,Int)](
      new java.util.Comparator[(Stm,Int)] { def compare(a:(Stm,Int), b:(Stm,Int)) = if (b._2 < a._2) -1 else if (b._2 == a._2) 0 else 1 }
    )
    
    for (sym <- syms) {
      val stm = cache.get(sym)
      if (stm ne null) sortedSet.add(stm)
    }

    var res: List[Stm] = Nil
    val iter = sortedSet.iterator //return stms in the original order given by 'scope'
    while (iter.hasNext) {
      res ::= iter.next._1
    } 
    res
  }

  protected def buildScopeIndex(scope: List[Stm]): IdentityHashMap[Sym[Any], (Stm,Int)] = {
    val cache = new IdentityHashMap[Sym[Any], (Stm,Int)]
    var idx = 0
    for (stm <- scope) {
      for (s <- stm.lhs) cache.put(s, (stm,idx)) //remember the original order of the stms
      idx += 1
    }
    cache
  }

  /**
   * Build traversal schedule for result in given scope, returning a list of statements
   **/
  def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true): List[Stm] = {
    val scopeIndex = buildScopeIndex(scope)

    val xx = GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(syms(result), scopeIndex), t => scheduleDepsWithIndex(syms(t.rhs), scopeIndex))
    if (sort) xx.foreach { x => 
      if (x.length > 1) {
        cwarn("Recursive schedule for result " + result + ": " + x)
        (new Exception) printStackTrace
      }
    }
    xx.flatten.reverse
  }
  def getUnsortedSchedule(scope: List[Stm])(result: Any): List[Stm] = getSchedule(scope)(result, false)

  /**
   * Create traversal schedule for result in given scope, filtering by sym frequency
   * | cold |  hot |  freq. range
   * --------------------------------
   * |  T   |   T  |  (all)
   * |  T   |   F  |  x < 100
   * |  F   |   T  |  x > 0.75
   * |  F   |   F  |  0.75 < x < 100
   **/
  def getScheduleM(scope: List[Stm])(result: Any, cold: Boolean, hot: Boolean): List[Stm] = {
    def mysyms(st: Any) = {
      val db = symsFreq(st).groupBy(_._1).mapValues(_.map(_._2).sum).toList
      assert(syms(st).toSet == db.map(_._1).toSet, "different list of syms: "+syms(st)+"!="+db+" for "+st)
      if (cold && hot) db.map(_._1)
      else if (cold && !hot) db.withFilter(_._2 < 100.0).map(_._1)
      else if (!cold && hot) db.withFilter(_._2 > 0.75).map(_._1)
      else db.withFilter(p=>p._2 > 0.75 && p._2 < 100.0).map(_._1)
    }

    val scopeIndex = buildScopeIndex(scope)

    GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(mysyms(result), scopeIndex), t => scheduleDepsWithIndex(mysyms(t.rhs), scopeIndex)).flatten.reverse
  }
    
  
  /** begin performance hotspot **/
  
  /**
   * For each symbol s in sts, find all statements in scope that depend on it.
   * We need to stop when we reach the statement where s is bound.
   * 
   * It would be tempting to do only one scc call but then we mix
   * up the locations where different symbols are bound.
   **/
  def getFatDependents(scope: List[Stm])(sts: List[Sym[Any]]): List[Stm] = {
    if (sts.isEmpty) return Nil
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
    optimization:
      traverse syms by ascending id. if sym s1 is used by s2, do not evaluate further 
      uses of s2 because they are already there.

    CAVEAT: TRANSFORMERS !!!

    assumption: if s2 uses s1, the scope of s2 is completely included in s1's scope:

      val A = loop { s1 => ... val B = sum { s2 => ... val y = s2 + s1; .../* use y */ ... } }

      once we reach y the second time (from s2) we can stop, because the uses of
      y have been tracked up to A, which includes all of B
    */

    val seen = new mutable.HashSet[Sym[Any]]
    
    def getDepStuff(st: Sym[Any]) = {
      // could also precalculate uses, but computing all combinations eagerly is also expensive
      def uses(s: Sym[Any]): List[Stm] = if (seen(s)) Nil else { 
        //seen += s
        lhsCache.getOrElse(s,Nil) ::: symsCache.getOrElse(s,Nil) filterNot (boundSymsCache.getOrElse(st, Nil) contains _)
      }
      GraphUtil.stronglyConnectedComponents[Stm](
        uses(st),
        t => t.lhs flatMap uses
      ).flatten
    }
    
    // CAVEAT: TRANSFORMERS !!!  see CloseWorldRestage app in Delite
    //sts.sortBy(_.id).flatMap(getDepStuff)
    
    sts.flatMap(getDepStuff).distinct
  }
  
  /** end performance hotspot **/
}
