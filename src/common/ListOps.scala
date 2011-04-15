package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen

trait ListOps extends Base {

  object List {
    def apply[A:Manifest](xs: Rep[A]*) = list_new(xs)
  }

  def list_new[A:Manifest](xs: Seq[Rep[A]]) : Rep[List[A]]
}

trait ListOpsExp extends ListOps with EffectExp {
  case class ListNew[A:Manifest](xs: Seq[Rep[A]]) extends Def[List[A]]

  def list_new[A:Manifest](xs: Seq[Rep[A]]) = ListNew(xs)

/*
  override def syms(e: Any): List[Sym[Any]] = e match { // TODO: can do without override?
    case ListNew(xs) => (xs flatMap { syms }).toList
    case _ => super.syms(e)
  }
*/
}

trait BaseGenListOps extends GenericNestedCodegen {
  val IR: ListOpsExp
  import IR._

}

trait ScalaGenListOps extends BaseGenListOps with ScalaGenEffect {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ListNew(xs) => emitValDef(sym, "List(" + (xs map {quote}).mkString(",") + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenListOps extends BaseGenListOps with CLikeGenBase {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenListOps extends CudaGenEffect with CLikeGenListOps
trait CGenListOps extends CGenEffect with CLikeGenListOps

