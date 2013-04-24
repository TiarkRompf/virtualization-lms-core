package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import internal._
import ops.UncheckedOpsExp


trait CGenUncheckedOps extends CGenBase {
  val IR: UncheckedOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Unchecked(xs) => 
      emitValDef(sym, xs map ((x:Any)=> x match { case x: Exp[_] => quote(x) case x => x.toString }) mkString "")
    case _ => super.emitNode(sym, rhs)
  }
}
