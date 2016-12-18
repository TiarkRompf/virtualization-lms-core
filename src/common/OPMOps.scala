
package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.reflect.SourceContext

trait OMPOps extends Base {
  def parallel_region(b: => Rep[Unit]): Rep[Unit]
  def critical_region(b: => Rep[Unit]): Rep[Unit]
}

trait OMPOpsExp extends OMPOps {

  case class ParallelRegion(b: Block[Unit]) extends Def[Unit]
  def parallel_region(b: => Exp[Unit]): Exp[Unit] = {
    val br = reifyEffects(b)
    reflectEffect(ParallelRegion(br))
  }

  case class CriticalRegion(b: Block[Unit]) extends Def[Unit]
  def critical_region(b: => Exp[Unit]): Exp[Unit] = {
    val br = reifyEffects(b)
    reflectEffect(CriticalRegion(br))
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ParallelRegion(b) => effectSyms(b)
    case CriticalRegion(b) => effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ParallelRegion(body) => syms(body)
    case CriticalRegion(body) => syms(body)
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ParallelRegion(body) => freqHot(body)
    case CriticalRegion(body) => freqHot(body)
    case _ => super.symsFreq(e)
  }

}

trait BaseGenOMPOps extends GenericNestedCodegen {
  val IR: OMPOpsExp
  import IR._
}

trait ScalaGenOMPOps extends ScalaGenEffect with BaseGenOMPOps {

}

trait CGenOMPOps extends CGenEffect with BaseGenOMPOps{
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ParallelRegion(body) =>
      gen"""#pragma omp parallel
      |{
      |${nestedBlock(body)}
      |}"""
    case CriticalRegion(body) =>
      gen"""#pragma omp critical
      |{
      |${nestedBlock(body)}
      |}"""
    case _ => super.emitNode(sym, rhs)
  }
}

