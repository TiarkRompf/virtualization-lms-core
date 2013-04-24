package scala.lms
package targets.clike.codegen

import internal.GenericNestedCodegen
import ops.BaseGenWhile
import java.io.PrintWriter

trait CLikeGenWhile extends CLikeGenBase with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case While(c,b) =>
      stream.println("for (;;) {")
      emitBlock(c)
      stream.println("if (!"+quote(getBlockResult(c))+") break;")
      emitBlock(b)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenWhile extends CudaGenEffect with CLikeGenWhile

trait OpenCLGenWhile extends OpenCLGenEffect with CLikeGenWhile

trait CGenWhile extends CGenEffect with CLikeGenWhile
