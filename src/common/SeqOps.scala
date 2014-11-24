package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal._
import org.scala_lang.virtualized.SourceContext

trait SeqOps extends Variables {

  object Seq {
    def apply[A:Manifest](xs: Rep[A]*)(implicit pos: SourceContext) = seq_new(xs)
  }
  
  implicit def varToSeqOps[A:Manifest](x: Var[Seq[A]]) = new SeqOpsCls(readVar(x))
  implicit def repSeqToSeqOps[T:Manifest](a: Rep[Seq[T]]) = new SeqOpsCls(a)
  implicit def seqToSeqOps[T:Manifest](a: Seq[T]) = new SeqOpsCls(unit(a))

  class SeqOpsCls[T:Manifest](a: Rep[Seq[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = seq_apply(a,n)
    def length(implicit pos: SourceContext) = seq_length(a)
  }

  def seq_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def seq_apply[T:Manifest](x: Rep[Seq[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def seq_length[T:Manifest](x: Rep[Seq[T]])(implicit pos: SourceContext): Rep[Int]
}

trait SeqOpsExp extends SeqOps with EffectExp {
  case class SeqNew[A:Manifest](xs: List[Rep[A]]) extends Def[Seq[A]]
  case class SeqLength[T:Manifest](a: Exp[Seq[T]]) extends Def[Int]
  case class SeqApply[T:Manifest](x: Exp[Seq[T]], n: Exp[Int]) extends Def[T]
  
  def seq_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext) = SeqNew(xs.toList)
  def seq_apply[T:Manifest](x: Exp[Seq[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = SeqApply(x, n)
  def seq_length[T:Manifest](a: Exp[Seq[T]])(implicit pos: SourceContext): Exp[Int] = SeqLength(a)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
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
    case SeqNew(xs) => emitValDef(sym, src"Seq($xs)")
    case SeqLength(x) => emitValDef(sym, src"$x.length")
    case SeqApply(x,n) => emitValDef(sym, src"$x($n)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSeqOps extends BaseGenSeqOps with CLikeGenBase  {
  val IR: SeqOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenSeqOps extends CudaGenEffect with CLikeGenSeqOps
trait OpenCLGenSeqOps extends OpenCLGenEffect with CLikeGenSeqOps
trait CGenSeqOps extends CGenEffect with CLikeGenSeqOps

