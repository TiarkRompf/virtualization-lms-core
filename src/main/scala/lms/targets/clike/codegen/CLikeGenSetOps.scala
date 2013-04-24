package scala.lms
package targets.clike.codegen

import ops.{SetOpsExp, BaseGenSetOps}

import java.io.PrintWriter

trait CLikeGenSetOps extends BaseGenSetOps with CLikeGenBase {
  val IR: SetOpsExp
  import IR._

//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
}

trait CudaGenSetOps extends CudaGenEffect with CLikeGenSetOps
trait OpenCLGenSetOps extends OpenCLGenEffect with CLikeGenSetOps
trait CGenSetOps extends CGenEffect with CLikeGenSetOps
