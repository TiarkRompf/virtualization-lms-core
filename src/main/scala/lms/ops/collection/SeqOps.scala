package scala.lms
package ops

import internal._

import java.io.PrintWriter
import scala.reflect.SourceContext



trait LiftSeqType { this: Base =>
  implicit def liftSeq[T](implicit t: TypeRep[T]): TypeRep[Seq[T]] = {
     implicit val mf = t.mf
     typeRep[Seq[T]]
  }
}


trait SeqOps extends Variables with LiftSeqType {

  object Seq {
    def apply[A:TypeRep](xs: Rep[A]*)(implicit pos: SourceContext) = seq_new(xs)
  }

  implicit def varToSeqOps[A:TypeRep](x: Var[Seq[A]]) = new SeqOpsCls(readVar(x))
  implicit def repSeqToSeqOps[T:TypeRep](a: Rep[Seq[T]]) = new SeqOpsCls(a)
  implicit def seqToSeqOps[T:TypeRep](a: Seq[T]) = new SeqOpsCls(unit(a))

  class SeqOpsCls[T:TypeRep](a: Rep[Seq[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = seq_apply(a,n)
    def length(implicit pos: SourceContext) = seq_length(a)
  }

  def seq_new[A:TypeRep](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def seq_apply[T:TypeRep](x: Rep[Seq[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def seq_length[T:TypeRep](x: Rep[Seq[T]])(implicit pos: SourceContext): Rep[Int]
}

trait SeqOpsExp extends SeqOps with EffectExp {
  case class SeqNew[A:TypeRep](xs: List[Rep[A]]) extends Def[Seq[A]]
  case class SeqLength[T:TypeRep](a: Exp[Seq[T]]) extends Def[Int]
  case class SeqApply[T:TypeRep](x: Exp[Seq[T]], n: Exp[Int]) extends Def[T]

  def seq_new[A:TypeRep](xs: Seq[Rep[A]])(implicit pos: SourceContext) = SeqNew(xs.toList)
  def seq_apply[T:TypeRep](x: Exp[Seq[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = SeqApply(x, n)
  def seq_length[T:TypeRep](a: Exp[Seq[T]])(implicit pos: SourceContext): Exp[Int] = SeqLength(a)

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SeqNew(xs) => seq_new(f(xs))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  // TODO: need override? (missing data dependency in delite kernel without it...)
  override def syms(e: Any): List[Sym[Any]] = e match {
    case SeqNew(xs) => (xs flatMap { syms }).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case SeqNew(xs) => (xs flatMap { freqNormal }).toList
    case _ => super.symsFreq(e)
  }
}

trait BaseGenSeqOps extends GenericNestedCodegen {
  val IR: SeqOpsExp
  import IR._

}

trait ScalaGenSeqOps extends BaseGenSeqOps with ScalaGenEffect {
  val IR: SeqOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SeqNew(xs) => emitValDef(sym, "Seq(" + (xs map {quote}).mkString(",") + ")")
    case SeqLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case SeqApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
