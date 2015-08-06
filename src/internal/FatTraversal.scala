package scala.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


trait FatBlockTraversal extends NestedBlockTraversal with FatScheduling {
  val IR: Expressions with Effects with FatExpressions
  import IR._  



  override def focusFatBlock[A](result: List[Block[Any]])(body: => A): A = {
    super.focusFatBlock(result) {
      innerScope = fattenAll(innerScope)
      body
    }
  }


}