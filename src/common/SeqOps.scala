package scala.lms
package common

import java.io.PrintWriter
import internal._
import scala.reflect.SourceContext

trait SeqOps extends Variables {

  implicit def seqTyp[T:Typ]: Typ[Seq[T]]

  object Seq {
    def apply[A:Typ](xs: Rep[A]*)(implicit pos: SourceContext) = seq_new(xs)
  }
  
  implicit def varToSeqOps[A:Typ](x: Var[Seq[A]]) = new SeqOpsCls(readVar(x))
  implicit def repSeqToSeqOps[T:Typ](a: Rep[Seq[T]]) = new SeqOpsCls(a)
  implicit def seqToSeqOps[T:Typ](a: Seq[T]) = new SeqOpsCls(unit(a))

  class SeqOpsCls[T:Typ](a: Rep[Seq[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = seq_apply(a,n)
    def length(implicit pos: SourceContext) = seq_length(a)
  }

  def seq_new[A:Typ](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def seq_apply[T:Typ](x: Rep[Seq[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def seq_length[T:Typ](x: Rep[Seq[T]])(implicit pos: SourceContext): Rep[Int]
}

trait SeqOpsExp extends SeqOps with PrimitiveOps with EffectExp {
  case class SeqNew[A:Typ](xs: List[Rep[A]]) extends Def[Seq[A]] {
    def mA = manifest[A]
  }
  case class SeqLength[T:Typ](a: Exp[Seq[T]]) extends Def[Int]
  case class SeqApply[T:Typ](x: Exp[Seq[T]], n: Exp[Int]) extends Def[T]
  
  def seq_new[A:Typ](xs: Seq[Rep[A]])(implicit pos: SourceContext) = SeqNew(xs.toList)
  def seq_apply[T:Typ](x: Exp[Seq[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = SeqApply(x, n)
  def seq_length[T:Typ](a: Exp[Seq[T]])(implicit pos: SourceContext): Exp[Int] = SeqLength(a)

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@SeqNew(xs) => seq_new(f(xs))(e.mA,pos)
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
