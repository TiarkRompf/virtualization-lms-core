package scala.lms
package targets.clike.codegen

import internal.{GenerationFailedException}
import ops.ObjectOpsExp

import java.io.PrintWriter

trait CLikeGenObjectOps extends CLikeGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(lhs) => emitValDef(sym, "(" + quote(lhs) + ").toString()")
    case ObjectHashCode(lhs) => emitValDef(sym, "(" + quote(lhs) + ").##")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, quote(x) + "; // unsafe immutable")
    case ObjectUnsafeMutable(x) => emitValDef(sym, quote(x) + "; // unsafe mutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenObjectOps extends CudaGenBase with CLikeGenObjectOps
trait OpenCLGenObjectOps extends OpenCLGenBase with CLikeGenObjectOps

trait CGenObjectOps extends CGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectUnsafeImmutable(x) => stream.println("%s *%s = %s; // unsafe immutable".format(remap(sym.tp),quote(sym),quote(x)))
    case ObjectUnsafeMutable(x) => stream.println("%s *%s = %s; // unsafe mutable".format(remap(sym.tp),quote(sym),quote(x)))
    case _ => super.emitNode(sym, rhs)
  }
}
