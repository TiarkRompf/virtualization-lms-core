package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen
import scala.reflect.SourceContext

trait ListOps extends Variables {

  object NewList {
    def apply[A:Manifest](xs: Rep[A]*)(implicit pos: SourceContext) = list_new(xs)
  }

  implicit def varToListOps[T:Manifest](x: Var[List[T]]) = new ListOpsCls(readVar(x)) // FIXME: dep on var is not nice
  implicit def repToListOps[T:Manifest](a: Rep[List[T]]) = new ListOpsCls(a)
  implicit def listToListOps[T:Manifest](a: List[T]) = new ListOpsCls(unit(a))

  class ListOpsCls[A:Manifest](l: Rep[List[A]]) {
    def map[B:Manifest](f: Rep[A] => Rep[B]) = list_map(l,f)
    def foreach(f: Rep[A] => Rep[Unit]) = list_foreach(l,f)
    def flatMap[B : Manifest](f: Rep[A] => Rep[List[B]]) = list_flatMap(l,f)
    def filter(f: Rep[A] => Rep[Boolean]) = list_filter(l, f)
    def sortBy[B:Manifest:Ordering](f: Rep[A] => Rep[B]) = list_sortby(l,f)
    def ::(e: Rep[A]) = list_prepend(l,e)
    def ++ (l2: Rep[List[A]]) = list_concat(l, l2)
    def mkString = list_mkString(l)
    def mkString(s:Rep[String]) = list_mkString2(l,s)
    def head = list_head(l)
    def tail = list_tail(l)
    def isEmpty = list_isEmpty(l)
    def toArray = list_toarray(l)
    def toSeq = list_toseq(l)
    def reverse = list_reverse(l)
    def contains(e: Rep[A]) = list_contains(l,e)
  }

  def list_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_fromseq[A:Manifest](xs: Rep[Seq[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_map[A:Manifest,B:Manifest](l: Rep[List[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[List[B]]
  def list_foreach[A:Manifest](l: Rep[List[A]], f: Rep[A] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def list_flatMap[A : Manifest, B : Manifest](xs: Rep[List[A]], f: Rep[A] => Rep[List[B]])(implicit pos: SourceContext): Rep[List[B]]
  def list_filter[A : Manifest](l: Rep[List[A]], f: Rep[A] => Rep[Boolean])(implicit pos: SourceContext): Rep[List[A]]
  def list_sortby[A:Manifest,B:Manifest:Ordering](l: Rep[List[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[List[A]]
  def list_prepend[A:Manifest](l: Rep[List[A]], e: Rep[A])(implicit pos: SourceContext): Rep[List[A]]
  def list_toarray[A:Manifest](l: Rep[List[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def list_toseq[A:Manifest](l: Rep[List[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def list_concat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_cons[A:Manifest](x: Rep[A], xs: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]] // FIXME remove?
  def list_mkString[A : Manifest](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[String]
  def list_mkString2[A : Manifest](xs: Rep[List[A]], sep:Rep[String])(implicit pos: SourceContext): Rep[String]
  def list_head[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[A]
  def list_tail[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_isEmpty[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext): Rep[Boolean]
  def list_reverse[A:Manifest](l: Rep[List[A]])(implicit pos: SourceContext): Rep[List[A]]
  def list_contains[A:Manifest](xs: Rep[List[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Boolean]
}

trait ListOpsExp extends ListOps with EffectExp with VariablesExp {
  case class ListNew[A:Manifest](xs: Seq[Rep[A]]) extends Def[List[A]]
  case class ListFromSeq[A:Manifest](xs: Rep[Seq[A]]) extends Def[List[A]]
  case class ListMap[A:Manifest,B:Manifest](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[List[B]]
  case class ListForeach[A:Manifest](l: Exp[List[A]], x: Sym[A], block: Block[Unit]) extends Def[Unit]
  case class ListFlatMap[A:Manifest, B:Manifest](l: Exp[List[A]], x: Sym[A], block: Block[List[B]]) extends Def[List[B]]
  case class ListFilter[A : Manifest](l: Exp[List[A]], x: Sym[A], block: Block[Boolean]) extends Def[List[A]]
  case class ListSortBy[A:Manifest,B:Manifest:Ordering](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[List[A]]
  case class ListPrepend[A:Manifest](x: Exp[List[A]], e: Exp[A]) extends Def[List[A]]
  case class ListToArray[A:Manifest](x: Exp[List[A]]) extends Def[Array[A]]
  case class ListToSeq[A:Manifest](x: Exp[List[A]]) extends Def[Seq[A]]
  case class ListConcat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[List[A]]
  case class ListCons[A:Manifest](x: Rep[A], xs: Rep[List[A]]) extends Def[List[A]]
  case class ListMkString[A:Manifest](l: Exp[List[A]]) extends Def[String]
  case class ListMkString2[A:Manifest](l: Exp[List[A]], s: Exp[String]) extends Def[String]
  case class ListHead[A:Manifest](xs: Rep[List[A]]) extends Def[A]
  case class ListTail[A:Manifest](xs: Rep[List[A]]) extends Def[List[A]]
  case class ListIsEmpty[A:Manifest](xs: Rep[List[A]]) extends Def[Boolean]
  case class ListReverse[A:Manifest](xs: Rep[List[A]]) extends Def[List[A]]
  case class ListContains[A:Manifest](xs: Rep[List[A]], e: Rep[A]) extends Def[Boolean]

  def list_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext) = ListNew(xs)
  def list_fromseq[A:Manifest](xs: Rep[Seq[A]])(implicit pos: SourceContext) = ListFromSeq(xs)
  def list_map[A:Manifest,B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[B])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListMap(l, a, b), summarizeEffects(b).star)
  }
  def list_foreach[A:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[Unit])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListForeach(l, a, b), summarizeEffects(b).star)
  }
  def list_flatMap[A:Manifest, B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[List[B]])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFlatMap(l, a, b), summarizeEffects(b).star)
  }
  def list_filter[A : Manifest](l: Exp[List[A]], f: Exp[A] => Exp[Boolean])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFilter(l, a, b), summarizeEffects(b).star)
  }
  def list_sortby[A:Manifest,B:Manifest:Ordering](l: Exp[List[A]], f: Exp[A] => Exp[B])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListSortBy(l, a, b), summarizeEffects(b).star)
  }
  def list_toarray[A:Manifest](l: Exp[List[A]])(implicit pos: SourceContext) = ListToArray(l)
  def list_toseq[A:Manifest](l: Exp[List[A]])(implicit pos: SourceContext) = ListToSeq(l)
  def list_prepend[A:Manifest](l: Exp[List[A]], e: Exp[A])(implicit pos: SourceContext) = ListPrepend(l,e)
  def list_concat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]])(implicit pos: SourceContext) = ListConcat(xs,ys)
  def list_cons[A:Manifest](x: Rep[A], xs: Rep[List[A]])(implicit pos: SourceContext) = ListCons(x,xs)
  def list_mkString[A:Manifest](l: Exp[List[A]])(implicit pos: SourceContext) = ListMkString(l)
  def list_mkString2[A:Manifest](l: Rep[List[A]], sep:Rep[String])(implicit pos: SourceContext) = ListMkString2(l,sep)
  def list_head[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext) = ListHead(xs)
  def list_tail[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext) = ListTail(xs)
  def list_isEmpty[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext) = ListIsEmpty(xs)
  def list_reverse[A:Manifest](xs: Rep[List[A]])(implicit pos: SourceContext) = ListReverse(xs)
  def list_contains[A:Manifest](xs: Rep[List[A]], e: Rep[A])(implicit pos: SourceContext) = ListContains(xs,e)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case ListNew(xs) => list_new(f(xs))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => syms(a):::syms(body)
    case ListForeach(a, x, body) => syms(a):::syms(body)
    case ListFlatMap(a, _, body) => syms(a) ::: syms(body)
    case ListFilter(a, _, body) => syms(a) ::: syms(body)
    case ListSortBy(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => x :: effectSyms(body)
    case ListForeach(a, x, body) => x :: effectSyms(body)
    case ListFlatMap(_, x, body) => x :: effectSyms(body)
    case ListFilter(_, x, body) => x :: effectSyms(body)
    case ListSortBy(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ListMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ListForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case ListFlatMap(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListFilter(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListSortBy(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait ListOpsExpOpt extends ListOpsExp {
  override def list_concat[A : Manifest](xs1: Exp[List[A]], xs2: Exp[List[A]])(implicit pos: SourceContext): Exp[List[A]] = (xs1, xs2) match {
    case (Def(ListNew(xs1)), Def(ListNew(xs2))) => ListNew(xs1 ++ xs2)
    case (Def(ListNew(Seq())), xs2) => xs2
    case (xs1, Def(ListNew(Seq()))) => xs1
    case _ => super.list_concat(xs1, xs2)
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
    case ListNew(xs) => emitValDef(sym, src"List(${(xs map {quote}).mkString(",")})")
    case ListConcat(xs,ys) => emitValDef(sym, src"$xs ::: $ys")
    case ListCons(x, xs) => emitValDef(sym, src"$x :: $xs")
    case ListHead(xs) => emitValDef(sym, src"$xs.head")
    case ListTail(xs) => emitValDef(sym, src"$xs.tail")
    case ListIsEmpty(xs) => emitValDef(sym, src"$xs.isEmpty")
    case ListReverse(l) => emitValDef(sym, src"$l.reverse")
    case ListFromSeq(xs) => emitValDef(sym, src"List($xs: _*)")
    case ListMkString(xs) => emitValDef(sym, src"$xs.mkString")
    case ListMkString2(xs,s) => emitValDef(sym, src"$xs.mkString($s)")
    case ListMap(l,x,blk) =>
/* XXX:TO_MERGE
      gen"""val $sym = $l.map { $x =>
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case ListFlatMap(l, x, b) =>
      gen"""val $sym = $l.flatMap { $x =>
           |${nestedBlock(b)}
           |$b
           |}"""
    case ListFilter(l, x, b) =>
      gen"""val $sym = $l.filter { $x =>
           |${nestedBlock(b)}
           |$b
           |}"""
    case ListSortBy(l,x,blk) =>
      gen"""val $sym = $l.sortBy { $x =>
           |${nestedBlock(blk)}
           |$blk
           |}"""
*/
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        stream.println(quote(l) + ".map { " + quote(x) + " => ")
        emitBlock(blk)
        stream.println(quote(getBlockResult(blk)))
        stream.print("}")
      }
      emitValDef(sym, strWriter.toString)
    case ListForeach(l,x,blk) => {
      stream.println(quote(l) + ".foreach { " + quote(x) + " => ")
      emitBlock(blk)
      stream.println("}")
    }
    case ListFlatMap(l, x, b) => {
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        stream.println(quote(l) + ".flatMap { " + quote(x) + " => ")
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.print("}")
      }
      emitValDef(sym, strWriter.toString)
    }
    case ListFilter(l, x, b) => {
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        stream.println(quote(l) + ".filter { " + quote(x) + " => ")
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.print("}")
      }
      emitValDef(sym, strWriter.toString)
    }
    case ListSortBy(l,x,blk) => {
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        stream.println(quote(l) + ".sortBy { " + quote(x) + " => ")
        emitBlock(blk)
        stream.println(quote(getBlockResult(blk)))
        stream.print("}")
      }
      emitValDef(sym, strWriter.toString)
    }
    case ListPrepend(l,e) => emitValDef(sym, src"$e :: $l")
    case ListToArray(l) => emitValDef(sym, src"$l.toArray")
    case ListToSeq(l) => emitValDef(sym, src"$l.toSeq")
    case ListContains(l, e) => emitValDef(sym, src"$l.contains($e)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenListOps extends BaseGenListOps with CLikeGenBase {
  val IR: ListOpsExp
  import IR._

/*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
*/
}

trait CudaGenListOps extends CudaGenEffect with CLikeGenListOps
trait OpenCLGenListOps extends OpenCLGenEffect with CLikeGenListOps
trait CGenListOps extends CGenEffect with CLikeGenListOps

