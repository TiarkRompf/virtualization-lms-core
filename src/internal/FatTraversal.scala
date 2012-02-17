package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


trait FatTraversal extends NestedTraversal with FatScheduling {
  val IR: Expressions with Effects with FatExpressions
  import IR._  


  //  ------------------- these are needed by loop fusion. they should live elsewhere.
  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollect(e).map((_,Nil))

  def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]): Def[Any] = sys.error("not implemented")

  def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

  // -------------------

  override def focusFatBlock[A](result: List[Block[Any]])(body: => A): A = {
    super.focusFatBlock(result) {
      innerScope = fattenAll(innerScope)
      body
    }
  }


}