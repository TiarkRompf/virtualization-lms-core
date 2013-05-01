package scala.lms
package ops

import internal.GenericNestedCodegen

import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftListType { this: TypeRepBase =>
  implicit def liftList[T](implicit t: TypeRep[T]): TypeRep[List[T]] = {
    implicit val mf = t.mf
    typeRep[List[T]]
  }
}

trait ListOps extends Variables with LiftListType with LiftSeqType with LiftArrayType {

  object List {
    def apply[A:TypeRep](xs: Rep[A]*)(implicit pos: SourceContext) = list_new(xs)
  }

  implicit def varToListOps[T:TypeRep](x: Var[List[T]]) = new ListOpsCls(readVar(x)) // FIXME: dep on var is not nice
  implicit def repToListOps[T:TypeRep](a: Rep[List[T]]) = new ListOpsCls(a)
  implicit def listToListOps[T:TypeRep](a: List[T]) = new ListOpsCls(unit(a))

  class ListOpsCls[A:TypeRep](l: Rep[List[A]]) {
    def map[B:TypeRep](f: Rep[A] => Rep[B]) = list_map(l,f)
    def flatMap[B:TypeRep](f: Rep[A] => Rep[List[B]]) = list_flatMap(f)(l)
    def filter(f: Rep[A] => Rep[Boolean]) = list_filter(l, f)
    def sortBy[B:TypeRep:Ordering](f: Rep[A] => Rep[B]) = list_sortby(l,f)
    def ::(e: Rep[A]) = list_prepend(l,e)
    def ++ (l2: Rep[List[A]]) = list_concat(l, l2)
    def mkString = list_mkString(l)
    def head = list_head(l)
    def tail = list_tail(l)
    def isEmpty = list_isEmpty(l)
    def toArray = list_toarray(l)
    def toSeq = list_toseq(l)
  }

  def list_new[A:TypeRep](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_fromseq[A:TypeRep](xs: Rep[Seq[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_map[A:TypeRep,B:TypeRep](l: Rep[List[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[List[B]]
  def list_flatMap[A:TypeRep, B:TypeRep](f: Rep[A] => Rep[List[B]])(xs: Rep[List[A]])(implicit pos: SourceContext): Rep[List[B]]
  def list_filter[A:TypeRep](l: Rep[List[A]], f: Rep[A] => Rep[Boolean])(implicit pos: SourceContext): Rep[List[A]]
  def list_sortby[A:TypeRep,B:TypeRep:Ordering](l: Rep[List[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[List[A]]
  def list_prepend[A:TypeRep](l: Rep[List[A]], e: Rep[A])(implicit pos: SourceContext): Rep[List[A]]
  def list_toarray[A:TypeRep](l: Rep[List[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def list_toseq[A:TypeRep](l: Rep[List[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def list_concat[A:TypeRep](xs: Rep[List[A]], ys: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_cons[A:TypeRep](x: Rep[A], xs: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]] // FIXME remove?
  def list_mkString[A:TypeRep](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[String]
  def list_head[A:TypeRep](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[A]
  def list_tail[A:TypeRep](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_isEmpty[A:TypeRep](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[Boolean]
}

trait ListOpsExp extends ListOps with EffectExp with VariablesExp {
  case class ListNew[A:TypeRep](xs: Seq[Rep[A]]) extends Def[List[A]]
  case class ListFromSeq[A:TypeRep](xs: Rep[Seq[A]]) extends Def[List[A]]
  case class ListMap[A:TypeRep,B:TypeRep](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[List[B]]
  case class ListFlatMap[A:TypeRep, B:TypeRep](l: Exp[List[A]], x: Sym[A], block: Block[List[B]]) extends Def[List[B]]
  case class ListFilter[A:TypeRep](l: Exp[List[A]], x: Sym[A], block: Block[Boolean]) extends Def[List[A]]
  case class ListSortBy[A:TypeRep,B:TypeRep:Ordering](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[List[A]]
  case class ListPrepend[A:TypeRep](x: Exp[List[A]], e: Exp[A]) extends Def[List[A]]
  case class ListToArray[A:TypeRep](x: Exp[List[A]]) extends Def[Array[A]]
  case class ListToSeq[A:TypeRep](x: Exp[List[A]]) extends Def[Seq[A]]
  case class ListConcat[A:TypeRep](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[List[A]]
  case class ListCons[A:TypeRep](x: Rep[A], xs: Rep[List[A]]) extends Def[List[A]]
  case class ListMkString[A:TypeRep](l: Exp[List[A]]) extends Def[String]
  case class ListHead[A:TypeRep](xs: Rep[List[A]]) extends Def[A]
  case class ListTail[A:TypeRep](xs: Rep[List[A]]) extends Def[List[A]]
  case class ListIsEmpty[A:TypeRep](xs: Rep[List[A]]) extends Def[Boolean]

  def list_new[A:TypeRep](xs: Seq[Rep[A]])(implicit pos: SourceContext) = ListNew(xs)
  def list_fromseq[A:TypeRep](xs: Rep[Seq[A]])(implicit pos: SourceContext) = ListFromSeq(xs)
  def list_map[A:TypeRep,B:TypeRep](l: Exp[List[A]], f: Exp[A] => Exp[B])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListMap(l, a, b), summarizeEffects(b).star)
  }
  def list_flatMap[A:TypeRep, B:TypeRep](f: Exp[A] => Exp[List[B]])(l: Exp[List[A]])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFlatMap(l, a, b), summarizeEffects(b).star)
  }
  def list_filter[A:TypeRep](l: Exp[List[A]], f: Exp[A] => Exp[Boolean])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFilter(l, a, b), summarizeEffects(b).star)
  }
  def list_sortby[A:TypeRep,B:TypeRep:Ordering](l: Exp[List[A]], f: Exp[A] => Exp[B])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListSortBy(l, a, b), summarizeEffects(b).star)
  }
  def list_toarray[A:TypeRep](l: Exp[List[A]])(implicit pos: SourceContext) = ListToArray(l)
  def list_toseq[A:TypeRep](l: Exp[List[A]])(implicit pos: SourceContext) = ListToSeq(l)
  def list_prepend[A:TypeRep](l: Exp[List[A]], e: Exp[A])(implicit pos: SourceContext) = ListPrepend(l,e)
  def list_concat[A:TypeRep](xs: Rep[List[A]], ys: Rep[List[A]])(implicit pos: SourceContext) = ListConcat(xs,ys)
  def list_cons[A:TypeRep](x: Rep[A], xs: Rep[List[A]])(implicit pos: SourceContext) = ListCons(x,xs)
  def list_mkString[A:TypeRep](l: Exp[List[A]])(implicit pos: SourceContext) = ListMkString(l)
  def list_head[A:TypeRep](xs: Rep[List[A]])(implicit pos: SourceContext) = ListHead(xs)
  def list_tail[A:TypeRep](xs: Rep[List[A]])(implicit pos: SourceContext) = ListTail(xs)
  def list_isEmpty[A:TypeRep](xs: Rep[List[A]])(implicit pos: SourceContext) = ListIsEmpty(xs)

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case ListNew(xs) => list_new(f(xs))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => syms(a):::syms(body)
    case ListFlatMap(a, _, body) => syms(a) ::: syms(body)
    case ListFilter(a, _, body) => syms(a) ::: syms(body)
    case ListSortBy(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => x :: effectSyms(body)
    case ListFlatMap(_, x, body) => x :: effectSyms(body)
    case ListFilter(_, x, body) => x :: effectSyms(body)
    case ListSortBy(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ListMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ListFlatMap(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListFilter(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListSortBy(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait ListOpsExpOpt extends ListOpsExp {
  override def list_concat[A:TypeRep](xs1: Exp[List[A]], xs2: Exp[List[A]])(implicit pos: SourceContext): Exp[List[A]] = (xs1, xs2) match {
    case (Def(ListNew(xs1)), Def(ListNew(xs2))) => ListNew(xs1 ++ xs2)
    case (Def(ListNew(Seq())), xs2) => xs2
    case (xs1, Def(ListNew(Seq()))) => xs1
    case _ => super.list_concat(xs1, xs2)
  }
  override def list_isEmpty[A:TypeRep](xs: Exp[List[A]])(implicit pos: SourceContext) : Exp[Boolean] = xs match {
    case Def(ListNew(Seq())) => unit(true)
    case Def(ListNew(_)) => unit(false)
    case _ => super.list_isEmpty(xs)
  }
  override def list_head[A:TypeRep](xs: Exp[List[A]])(implicit pos: SourceContext): Exp[A] = xs match {
    case Def(ListNew(Seq(y, _*))) => y
    case _ => super.list_head(xs)
  }
  override def list_tail[A:TypeRep](xs: Exp[List[A]])(implicit pos: SourceContext): Exp[List[A]] = xs match {
    case Def(ListNew(Seq(y, ys@_*))) => list_new(ys)
    case _ => super.list_tail(xs)
  }

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
    case ListFromSeq(xs) => emitValDef(sym, "List(" + quote(xs) + ": _*)")
    case ListMkString(xs) => emitValDef(sym, quote(xs) + ".mkString")
    case ListMap(l,x,blk) =>
      stream.println("val " + quote(sym) + " = " + quote(l) + ".map{")
      stream.println(quote(x) + " => ")
      emitBlock(blk)
      stream.println(quote(getBlockResult(blk)))
      stream.println("}")
    case ListFlatMap(l, x, b) => {
      stream.println("val " + quote(sym) + " = " + quote(l) + ".flatMap { " + quote(x) + " => ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    }
    case ListFilter(l, x, b) => {
      stream.println("val " + quote(sym) + " = " + quote(l) + ".filter { " + quote(x) + " => ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    }
    case ListSortBy(l,x,blk) =>
      stream.println("val " + quote(sym) + " = " + quote(l) + ".sortBy{")
      stream.println(quote(x) + " => ")
      emitBlock(blk)
      stream.println(quote(getBlockResult(blk)))
      stream.println("}")
    case ListPrepend(l,e) => emitValDef(sym, quote(e) + " :: " + quote(l))
    case ListToArray(l) => emitValDef(sym, quote(l) + ".toArray")
    case ListToSeq(l) => emitValDef(sym, quote(l) + ".toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}
