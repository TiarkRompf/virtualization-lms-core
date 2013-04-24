package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.lms.internal._
import ops.{SetOpsExp}

trait CLikeGenSetOps extends BaseGenSetOps with CLikeCodegen {
  val IR: SetOpsExp
  import IR._

//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
}

trait CudaGenSetOps extends CudaGenEffect with CLikeGenSetOps
trait OpenCLGenSetOps extends OpenCLGenEffect with CLikeGenSetOps
trait CGenSetOps extends CGenEffect with CLikeGenSetOps
