package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


trait FatBlockTraversal extends NestedBlockTraversal with FatScheduling {
  val IR: Expressions with Effects with FatExpressions
  import IR._  


  // TODO (VJ) move these elsewhere
  def plugInHelper[A, T: Manifest, U: Manifest](oldGen: Exp[Gen[A]], context: Exp[Gen[T]], plug: Exp[Gen[U]]): Exp[Gen[U]] = sys.error("not implemented")
  def applyPlugIntoContext(d: Def[Any], r: Def[Any]): Def[Any] = sys.error("not implemented")
  def applyExtendGenerator[A](d: Def[Any], body: Def[Any]): (Exp[A], Exp[A]) = sys.error("not implemented")

  def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true
  // -------------------

}
