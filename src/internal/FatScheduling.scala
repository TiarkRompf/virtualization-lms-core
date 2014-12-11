package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable.HashMap

trait FatScheduling extends Scheduling {
  val IR: FatExpressions
  import IR._  
  
  def fatten(e: Stm): Stm = e

  def fattenAll(e: List[Stm]): List[Stm] = e.map(fatten)


  //  ------------------- these are needed by loop fusion. they should live elsewhere.
  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollect(e).map((_,Nil))

  // FIXME: should be Def[A] => Def[A], not Def[Any]
  def canApplyAddCondition(e: Def[Any]): Boolean = true
  def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]): Def[Any] = sys.error("not implemented")

  def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

  // -------------------

    
}