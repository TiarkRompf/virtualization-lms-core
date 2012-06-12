package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable.HashMap
import java.util.IdentityHashMap

trait Scheduling {
  val IR: Expressions
  import IR._
  
  def dep(e: Def[Any]): List[Sym[Any]] = e match { // only used by GraphVizExport currently
    case d: Product => syms(d)
    case _ => Nil
  }


  def availableDefs: List[TP[Any]] = globalDefs
  
  
  def buildScheduleForResult(start: Exp[Any]): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] = {
      availableDefs.filter(st contains _.sym)
    }

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(syms(start)), t => deps(syms(t.rhs))).flatten.reverse
  }  

  def buildScheduleForResultM(scope: List[TP[Any]])(start: Any, cold: Boolean, hot: Boolean): List[TP[Any]] = {
    def mysyms(st: Any) = {
      val db = symsFreq(st).groupBy(_._1).mapValues(_.map(_._2).sum).toList
      assert(syms(st).toSet == db.map(_._1).toSet, "different list of syms: "+syms(st)+"!="+db)
      if (cold && hot) db.map(_._1)
      else if (cold && !hot) db.withFilter(_._2 < 100.0).map(_._1)
      else if (!cold && hot) db.withFilter(_._2 > 0.75).map(_._1)
      else db.withFilter(p=>p._2 > 0.75 && p._2 < 100.0).map(_._1)
    }
    
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      scope.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(mysyms(start)), t => deps(mysyms(t.rhs))).flatten.reverse
  }  

  /** begin performance hotspot **/

  def getDependentCaches(st: List[Sym[Any]]): (Array[Set[Sym[Any]]], Array[Boolean]) = {
    val l = availableDefs.length
    val symsCache = new Array[Set[Sym[Any]]](l)
    val boundSymsCache = new Array[Boolean](l)

    var i = 0
    for (d <- availableDefs) {
      symsCache(i) = syms(d.rhs).toSet
      boundSymsCache(i) = (boundSyms(d.rhs) intersect st).nonEmpty 
      i += 1
    }    
    (symsCache, boundSymsCache)    
  }
  
  def cachedUses(s: Sym[Any])(symsCache: Array[Set[Sym[Any]]], boundSymsCache: Array[Boolean]): List[TP[Any]] = {
    // relies on sequential, left-to-right execution order for performance
    var i = -1
    availableDefs.filter { d =>              
      i += 1
      d.sym == s || // include the definition itself
      symsCache(i).contains(s) && !boundSymsCache(i) // don't extrapolate beyond the scope of *any* root. is this the right thing to do?
    }
  }    
  
  def getDependentStuff(st: List[Sym[Any]]): List[TP[Any]] = {
    val (symsCache, boundSymsCache) = getDependentCaches(st)
    def uses(s: Sym[Any]) = cachedUses(s)(symsCache,boundSymsCache)
    (GraphUtil.stronglyConnectedComponents[TP[Any]]((st.flatMap(uses).distinct), t => uses(t.sym)).flatten).distinct // final distinct necessary?
  }
  
  def getDependentStuff(st: Sym[Any]): List[TP[Any]] = {
    val (symsCache, boundSymsCache) = getDependentCaches(List(st))
    def uses(s: Sym[Any]) = cachedUses(s)(symsCache,boundSymsCache)
    GraphUtil.stronglyConnectedComponents[TP[Any]](uses(st), t => uses(t.sym)).flatten
  }
      
  /** end performance hotspot **/

}