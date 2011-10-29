package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal._

trait IterableOps extends Variables {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToIterableOps[A:Manifest](x: Var[Iterable[A]]) = new IterableOpsCls(readVar(x))
  implicit def repIterableToIterableOps[T:Manifest](a: Rep[Iterable[T]]) = new IterableOpsCls(a)
  implicit def iterableToIterableOps[T:Manifest](a: Iterable[T]) = new IterableOpsCls(unit(a))

  class IterableOpsCls[T:Manifest](a: Rep[Iterable[T]]){
    def foreach(block: Rep[T] => Rep[Unit]) = iterable_foreach(a, block)
  }

  def iterable_foreach[T:Manifest](x: Rep[Iterable[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
}

trait IterableOpsExp extends IterableOps with EffectExp with VariablesExp {

  case class IterableForeach[T](a: Exp[Iterable[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]

  def iterable_foreach[T:Manifest](a: Exp[Iterable[T]], block: Exp[T] => Exp[Unit]): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(IterableForeach(a, x, b), summarizeEffects(b).star)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case IterableForeach(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case IterableForeach(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case IterableForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait BaseGenIterableOps extends GenericNestedCodegen {
  val IR: IterableOpsExp
  import IR._

}

trait ScalaGenIterableOps extends BaseGenIterableOps with ScalaGenBase {
  val IR: IterableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case IterableForeach(a,x,block) => stream.println("val " + quote(sym) + "=" + quote(a) + ".foreach{")
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenIterableOps extends BaseGenIterableOps with CLikeGenBase {
  val IR: IterableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenIterableOps extends CudaGenBase with CLikeGenIterableOps
trait OpenCLGenIterableOps extends OpenCLGenBase with CLikeGenIterableOps
trait CGenIterableOps extends CGenBase with CLikeGenIterableOps

