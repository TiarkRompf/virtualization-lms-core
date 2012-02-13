package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen
import scala.reflect.SourceContext

trait ListOps extends Variables {

  object List {
    def apply[A:Manifest](xs: Rep[A]*)(implicit ctx: SourceContext) = list_new(xs)
  }

  implicit def varToListOps[T:Manifest](x: Var[List[T]]) = new ListOpsCls(readVar(x))
  implicit def repToListOps[T:Manifest](a: Rep[List[T]]) = new ListOpsCls(a)
  implicit def listToListOps[T:Manifest](a: List[T]) = new ListOpsCls(unit(a))
  
  class ListOpsCls[A:Manifest](l: Rep[List[A]]) {
    def map[B:Manifest](f: Rep[A] => Rep[B]) = list_map(l,f)
    def sortBy[B:Manifest:Ordering](f: Rep[A] => Rep[B]) = list_sortby(l,f)
    def ::(e: Rep[A]) = list_prepend(l,e)
    def toArray = list_toarray(l)
    def toSeq = list_toseq(l)
  }
  
  def list_new[A:Manifest](xs: Seq[Rep[A]])(implicit ctx: SourceContext): Rep[List[A]]  
  def list_fromseq[A:Manifest](xs: Rep[Seq[A]])(implicit ctx: SourceContext): Rep[List[A]]  
  def list_map[A:Manifest,B:Manifest](l: Rep[List[A]], f: Rep[A] => Rep[B]): Rep[List[B]]
  def list_sortby[A:Manifest,B:Manifest:Ordering](l: Rep[List[A]], f: Rep[A] => Rep[B]): Rep[List[A]]
  def list_prepend[A:Manifest](l: Rep[List[A]], e: Rep[A]): Rep[List[A]]
  def list_toarray[A:Manifest](l: Rep[List[A]]): Rep[Array[A]]
  def list_toseq[A:Manifest](l: Rep[List[A]]): Rep[Seq[A]]
}

trait ListOpsExp extends ListOps with EffectExp with VariablesExp {
  case class ListNew[A:Manifest](xs: Seq[Rep[A]]) extends Def[List[A]]
  case class ListFromSeq[A:Manifest](xs: Rep[Seq[A]]) extends Def[List[A]]
  case class ListMap[A:Manifest,B:Manifest](x: Exp[List[A]], block: Exp[B]) extends Def[List[B]]
  case class ListSortBy[A:Manifest,B:Manifest:Ordering](x: Exp[List[A]], block: Exp[B]) extends Def[List[A]]
  case class ListPrepend[A:Manifest](x: Exp[List[A]], e: Exp[A]) extends Def[List[A]]
  case class ListToArray[A:Manifest](x: Exp[List[A]]) extends Def[Array[A]]
  case class ListToSeq[A:Manifest](x: Exp[List[A]]) extends Def[Seq[A]]
  
  def list_new[A:Manifest](xs: Seq[Rep[A]])(implicit ctx: SourceContext) = ListNew(xs)
  def list_fromseq[A:Manifest](xs: Rep[Seq[A]])(implicit ctx: SourceContext) = ListFromSeq(xs)
  def list_map[A:Manifest,B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    val a = fresh[A]
    val b = reifyEffectsHere(f(a))
    reflectEffect(ListMap(l, b), summarizeEffects(b))    
  }
  def list_sortby[A:Manifest,B:Manifest:Ordering](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    val a = fresh[A]
    val b = reifyEffectsHere(f(a))
    reflectEffect(ListSortBy(l, b), summarizeEffects(b))
  }
  def list_toarray[A:Manifest](l: Exp[List[A]]) = ListToArray(l)
  def list_toseq[A:Manifest](l: Exp[List[A]]) = ListToSeq(l)
  def list_prepend[A:Manifest](l: Exp[List[A]], e: Exp[A]) = ListPrepend(l,e)
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
    case ListFromSeq(xs) => emitValDef(sym, "List(" + quote(xs) + ": _*)")
    case ListMap(l,blk) => 
      stream.println("val " + quote(sym) + " = {")
      stream.println(quote(l) + ".map(")
      emitBlock(blk)
      stream.println(")}")
    case ListSortBy(l,blk) =>
      stream.println("val " + quote(sym) + " = {")
      stream.println(quote(l) + ".sortBy(")
      emitBlock(blk)
      stream.println(")}")
    case ListPrepend(l,e) => emitValDef(sym, quote(e) + " :: " + quote(l))    
    case ListToArray(l) => emitValDef(sym, quote(l) + ".toArray")
    case ListToSeq(l) => emitValDef(sym, quote(l) + ".toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenListOps extends BaseGenListOps with CLikeGenBase {
  val IR: ListOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
  //     rhs match {
  //       case _ => super.emitNode(sym, rhs)
  //     }
  //   }
}

trait CudaGenListOps extends CudaGenEffect with CLikeGenListOps
trait OpenCLGenListOps extends OpenCLGenEffect with CLikeGenListOps
trait CGenListOps extends CGenEffect with CLikeGenListOps

