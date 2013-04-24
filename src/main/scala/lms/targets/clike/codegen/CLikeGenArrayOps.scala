package scala.lms
package targets.clike.codegen

import ops.{BaseGenArrayOps, ArrayOpsExp}

trait CLikeGenArrayOps extends BaseGenArrayOps with CLikeGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ArrayLength(x) => emitValDef(sym, quote(x) + ".length")
        case ArrayApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
        case ArrayUpdate(x,n,y) => stream.println(quote(x) + ".update(" + quote(n) + "," + quote(y) + ");")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayOps extends CudaGenBase with CLikeGenArrayOps
trait OpenCLGenArrayOps extends OpenCLGenBase with CLikeGenArrayOps
trait CGenArrayOps extends CGenBase with BaseGenArrayOps {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ArrayLength(x) => emitValDef(sym, quote(x) + "->length")
        case ArrayApply(x,n) => emitValDef(sym, quote(x) + "->apply(" + quote(n) + ")")
        case ArrayUpdate(x,n,y) => stream.println(quote(x) + "->update(" + quote(n) + "," + quote(y) + ");")
        case _ => super.emitNode(sym, rhs)
      }
    }
}
