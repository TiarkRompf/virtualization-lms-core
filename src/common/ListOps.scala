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
  def list_cons[A:Manifest](x: Rep[A], xs: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_head[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[A]
  def list_tail[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_isEmpty[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[Boolean]
}

trait ListOpsExp extends ListOps with EffectExp {
  case class ListNew[A:Manifest](xs: List[Rep[A]]) extends Def[List[A]]
  case class ListConcat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[List[A]]
  case class ListCons[A:Manifest](x: Rep[A], xs: Rep[List[A]]) extends Def[List[A]]
  case class ListHead[A:Manifest](xs: Rep[List[A]]) extends Def[A]
  case class ListTail[A:Manifest](xs: Rep[List[A]]) extends Def[List[A]]
  case class ListIsEmpty[A:Manifest](xs: Rep[List[A]]) extends Def[Boolean]

  def list_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext) = ListNew(xs.toList)
  def list_concat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]])(implicit pos: SourceContext) = ListConcat(xs,ys)
  def list_cons[A:Manifest](x: Rep[A], xs: Rep[List[A]])(implicit pos: SourceContext) = ListCons(x,xs)
  def list_head[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext) = ListHead(xs)
  def list_tail[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext) = ListTail(xs)
  def list_isEmpty[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext) = ListIsEmpty(xs)
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
    case ListCons(x, xs) => emitValDef(sym, quote(x) + " :: " + quote(xs))
    case ListHead(xs) => emitValDef(sym, quote(xs) + ".head")
    case ListTail(xs) => emitValDef(sym, quote(xs) + ".tail")
    case ListIsEmpty(xs) => emitValDef(sym, quote(xs) + ".isEmpty")
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

