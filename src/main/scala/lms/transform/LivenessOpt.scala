package scala.lms
package transform

import internal.NestedBlockTraversal

import java.io.PrintWriter
import scala.collection.mutable

trait LivenessOpt extends NestedBlockTraversal {
  import IR._

  def calculateLivenessTransform(used: mutable.HashSet[Sym[Any]], t: Stm): List[Stm] = t match {
    case TP(sym, Reify(x, u, es)) if used(sym) =>
      used ++= syms(x) // ignore effect dependencies!
      used ++= u.mayWrite // writes to external data
      List(t)
    case TP(sym, rhs@Reflect(_, u, es)) if used(sym) ||
      mayWrite(u, used.toList) || maySimple(u) =>
      used ++= syms(rhs)
      List(t)
/*    case TP(sym, rhs) if used(sym) =>
      used ++= syms(rhs)
      //printlog("** add used at " + t + ": " + syms(rhs))
      List(t)*/
    case e =>
      if (e.lhs exists used) {
        used ++= syms(e.rhs)
        List(t)
      } else {
        printlog("dropping " + e)
        Nil
      }
  }


  override def focusBlock[A](result: Block[Any])(body: => A): A = {
    super.focusBlock(result) {

      printlog("gathering liveness info for block " + result)

      val used = new mutable.HashSet[Sym[Any]]
      used ++= syms(result)

      val newInnerScope = innerScope.reverse.flatMap(calculateLivenessTransform(used,_).reverse)

      innerScope = newInnerScope.reverse

      body
    }
  }

}


trait DefUseAnalysis extends NestedBlockTraversal {
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
        defUseMap = pairs.groupBy(_._1).map(p => (p._1, saveDefUseMap.getOrElse(p._1, Set()) ++ p._2.map(_._2)))
      else
        defUseMap = pairs.groupBy(_._1).map(p => (p._1, p._2.map(_._2).toSet))

      body
    }
  }

}