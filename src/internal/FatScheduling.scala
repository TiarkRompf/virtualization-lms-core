package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable.HashMap

trait FatScheduling extends Scheduling {
  val IR: FatExpressions
  import IR._  
  
  def fatten(e: Stm): Stm = e

  def fattenAll(e: List[Stm]): List[Stm] = e.map(fatten)

  def combineFat(list: List[TTP]): TTP = sys.error("ERROR: Don't know how to combineFat " + list.mkString("\n","\n","\n"))

  // The old loop fusion doesn't fatten some effectful loops and should
  // override this with false, but for the new fusion all loops and
  // ifs get fattened.
  def shouldFattenEffectfulLoops() = true

  //  ------------------- 
  // These are needed by the old loop fusion. they should live elsewhere.
  // The new loop fusion extractors are in trait LoopFusionExtractors.
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
