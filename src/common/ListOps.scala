package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen
import scala.reflect.SourceContext

trait ListOps extends Base {

  object List {
    def apply[A:Manifest](xs: Rep[A]*)(implicit pos: SourceContext) = list_new(xs)
  }

  def list_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_concat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
}

trait ListOpsExp extends ListOps with EffectExp {
  case class ListNew[A:Manifest](xs: Seq[Rep[A]]) extends Def[List[A]]
  case class ListConcat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[List[A]]

  def list_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext) = ListNew(xs)
  def list_concat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]])(implicit pos: SourceContext) = ListConcat(xs,ys)
}

trait BaseGenListOps extends GenericNestedCodegen {
  val IR: ListOpsExp
  import IR._

}

trait ScalaGenListOps extends BaseGenListOps with ScalaGenEffect {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListNew(xs) => emitValDef(sym, "List(" + (xs map {quote}).mkString(",") + ")")
    case ListConcat(xs,ys) => emitValDef(sym, quote(xs) + " ::: " + quote(ys))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenListOps extends BaseGenListOps with CLikeGenBase {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenListOps extends CudaGenEffect with CLikeGenListOps
trait OpenCLGenListOps extends OpenCLGenEffect with CLikeGenListOps
trait CGenListOps extends CGenEffect with CLikeGenListOps

