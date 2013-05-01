package scala.lms
package ops

import internal._

import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftIterableType { this: TypeRepBase =>
  implicit def liftIterable[T](implicit t: TypeRep[T]): TypeRep[Iterable[T]] = {
    implicit val mf = t.mf
    typeRep[Iterable[T]]
  }
}

trait IterableOps extends Variables with LiftIterableType with LiftArrayType {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToIterableOps[A:TypeRep](x: Var[Iterable[A]]) = new IterableOpsCls(readVar(x))
  implicit def repIterableToIterableOps[T:TypeRep](a: Rep[Iterable[T]]) = new IterableOpsCls(a)
  implicit def iterableToIterableOps[T:TypeRep](a: Iterable[T]) = new IterableOpsCls(unit(a))

  class IterableOpsCls[T:TypeRep](a: Rep[Iterable[T]]){
    def foreach(block: Rep[T] => Rep[Unit])(implicit pos: SourceContext) = iterable_foreach(a, block)
    def toArray(implicit pos: SourceContext) = iterable_toarray(a)
  }

  def iterable_foreach[T:TypeRep](x: Rep[Iterable[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def iterable_toarray[T:TypeRep](x: Rep[Iterable[T]])(implicit pos: SourceContext): Rep[Array[T]]
}

trait IterableOpsExp extends IterableOps with EffectExp with VariablesExp {

  case class IterableForeach[T:TypeRep](a: Exp[Iterable[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]
  case class IterableToArray[T:TypeRep](a: Exp[Iterable[T]]) extends Def[Array[T]] {
    val m = typeRep[T]
  }

  def iterable_foreach[T:TypeRep](a: Exp[Iterable[T]], block: Exp[T] => Exp[Unit])(implicit pos: SourceContext): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(IterableForeach(a, x, b), summarizeEffects(b).star)
  }
  def iterable_toarray[T:TypeRep](a: Exp[Iterable[T]])(implicit pos: SourceContext) = IterableToArray(a)

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case e@IterableToArray(x) => iterable_toarray(f(x))(e.m,pos)
      case Reflect(e@IterableForeach(x,y,b), u, es) => reflectMirrored(Reflect(IterableForeach(f(x),f(y).asInstanceOf[Sym[_]],f(b)), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case Reflect(e@IterableToArray(x), u, es) => reflectMirrored(Reflect(IterableToArray(f(x))(e.m), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IterableForeach(a,x,block) => stream.println("val " + quote(sym) + "=" + quote(a) + ".foreach{")
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("}")
    case IterableToArray(a) => emitValDef(sym, quote(a) + ".toArray")
    case _ => super.emitNode(sym, rhs)
  }
}
