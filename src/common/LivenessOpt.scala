package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.NestedTraversal

import scala.collection.mutable

trait LivenessOpt extends NestedTraversal {
  import IR._  
  
  def calculateLivenessTransform[A](used: mutable.HashSet[Exp[Any]], t: TP[A]): List[TP[_]] = t match {
    case TP(sym, Reify(x, u, es)) if used(sym) => 
      used += x
      used ++= u.mayWrite // writes to external data
      List(t) // ignore effect dependencies!
    case TP(sym, rhs@Reflect(_, u, es)) if used(sym) || 
      mayWrite(u, used.toList.collect { case s:Sym[_] => s}) || maySimple(u) => 
      used ++= syms(rhs)
      List(t)
    case TP(sym, rhs) if used(sym) => 
      used ++= syms(rhs)
      List(t)
    case e => 
      printlog("dropping " + e)
      Nil
  }


  override def focusBlock[A](result: Block[Any])(body: => A): A = {
    super.focusBlock(result) {

      printdbg("gathering liveness info for block " + result)
      
      val used = mutable.HashSet[Exp[Any]](getBlockResultFull(result))

      val newInnerScope = innerScope.reverse.flatMap(calculateLivenessTransform(used,_).reverse)
      
      innerScope = newInnerScope.reverse
      
      body
    }
  }
  
}


trait DefUseAnalysis extends NestedTraversal {
  import IR._  
  
  var defUseMap: Map[Exp[Any], Set[Exp[Any]]] = _

  override def focusBlock[A](result: Block[Any])(body: => A): A = {
    super.focusBlock(result) {
      val saveDefUseMap = defUseMap
      defUseMap 
    
      var pairs = List[(Exp[Any],Exp[Any])]()
    
      for (TP(sym, rhs) <- innerScope) {
        for (use <- syms(rhs)) {
          pairs = (use, sym) :: pairs
        }
      }
    
      if (saveDefUseMap ne null)
        defUseMap = pairs.groupBy(_._1).map(p => (p._1, saveDefUseMap(p._1) ++ p._2.map(_._2)))
      else
        defUseMap = pairs.groupBy(_._1).map(p => (p._1, p._2.map(_._2).toSet))

      body
    }
  }

}