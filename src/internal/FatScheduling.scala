package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable.HashMap

trait FatScheduling extends Scheduling {
  val IR: FatExpressions
  import IR._  
  
  import util.GraphUtil

  def fatten(e: TP[Any]): TTP = TTP(List(e.sym), ThinDef(e.rhs))
  
  def getSchedule(scope: List[TP[Any]])(result: Any): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      scope.filter(st contains _.sym)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatSchedule(scope: List[TTP])(result: Any): List[TTP] = {
    def deps(st: List[Sym[Any]]): List[TTP] =
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[TTP](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

/*
  def buildScheduleForResultM(defs: List[TP[Any]])(start: Any, cold: Boolean, hot: Boolean): List[TP[Any]] = {
    def mysyms(st: Any) = if (cold && hot) syms(st)
      else if (cold && !hot) syms(st) diff hotSyms(st)
      else if (!cold && hot) syms(st) diff coldSyms(st)
      else syms(st) diff coldSyms(st) diff hotSyms(st)
    
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      defs.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(mysyms(start,cold,hot)), t => deps(mysyms(t.rhs,cold,hot))).flatten.reverse
  }  
*/  
  def getFatScheduleM(scope: List[TTP])(start: Any, cold: Boolean, hot: Boolean): List[TTP] = {
    def mysyms(st: Any) = {
      val db = symsFreq(st).groupBy(_._1).mapValues(_.map(_._2).sum).toList
      assert(syms(st).toSet == db.map(_._1).toSet, "different list of syms: "+syms(st)+"!="+db)
      if (cold && hot) db.map(_._1)
      else if (cold && !hot) db.withFilter(_._2 < 100.0).map(_._1)
      else if (!cold && hot) db.withFilter(_._2 > 0.75).map(_._1)
      else db.withFilter(p=>p._2 > 0.75 && p._2 < 100.0).map(_._1)
    }

    def deps(st: List[Sym[Any]]): List[TTP] =
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[TTP](deps(mysyms(start)), t => deps(mysyms(t.rhs))).flatten.reverse
  }

  def getFatDependentStuff(scope: List[TTP])(sts: List[Sym[Any]]): List[TTP] = {
    /*
     precompute:
     s => all d in scope such that: d.lhs contains s || syms(d.rhs).contains(s)
     st => all d in scope such that: boundSyms(d.rhs) contains st
    */
    
    val lhsCache = new HashMap[Sym[Any], List[TTP]]
    val symsCache = new HashMap[Sym[Any], List[TTP]]
    val boundSymsCache = new HashMap[Sym[Any], List[TTP]]
    
    def putDef(map: HashMap[Sym[Any], List[TTP]], s: Sym[Any], d: TTP): Unit = {
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
      def uses(s: Sym[Any]): List[TTP] = {
        lhsCache.getOrElse(s, Nil) ++ (symsCache.getOrElse(s, Nil) filterNot (boundSymsCache.getOrElse(st, Nil) contains _))
      }
      GraphUtil.stronglyConnectedComponents[TTP](uses(st), t => t.lhs flatMap uses).flatten
    }.distinct

//    st.distinct.flatMap(getFatDependentStuff0(scope)(_)).distinct // this is expensive!!
  }
    
  def getFatDependentStuff1(scope: List[TTP])(sts: List[Sym[Any]]): List[TTP] = {
    sts.distinct.flatMap({ st => val res = getFatDependentStuff0(scope)(st) 
      printlog("--- dep on " + st)
      res.foreach(printlog(_))
      res
    }).distinct // this is expensive!!
  }
  def getFatDependentStuff0(scope: List[TTP])(st: Sym[Any]): List[TTP] = {
    def uses(s: Sym[Any]): List[TTP] = {
      scope.filter { d =>
        (d.lhs contains s) || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    GraphUtil.stronglyConnectedComponents[TTP](uses(st), t => t.lhs flatMap uses).flatten
  }
    
}