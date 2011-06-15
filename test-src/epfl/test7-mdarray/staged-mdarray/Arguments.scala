package scala.virtualization.lms
package epfl
package test7

import java.io.PrintWriter
import scala.virtualization.lms.common.{ScalaGenBase, BaseExp, Base}

trait Arguments extends Base {
  abstract class Args

  class Args2[T: Manifest, U: Manifest](r: Rep[(T, U)]) {
    def _1: Rep[T] = argElt[T](r, 1)
    def _2: Rep[U] = argElt[U](r, 2)
  }

  class Args3[T: Manifest, U: Manifest, V: Manifest](r: Rep[(T, U, V)]) {
    def _1: Rep[T] = argElt[T](r, 1)
    def _2: Rep[U] = argElt[U](r, 2)
    def _3: Rep[V] = argElt[V](r, 3)
  }

  implicit def pairToArgs[T: Manifest, U: Manifest](r: Rep[(T, U)]) = new Args2(r)
  implicit def tup3ToArgs[T: Manifest, U: Manifest, V: Manifest](r: Rep[(T, U, V)]) = new Args3(r)

  def argElt[T: Manifest](base: Rep[_], index: Int): Rep[T]
}

trait ArgumentsExp extends Arguments with BaseExp {
  case class Argument[T: Manifest](base: Exp[_], index: Int) extends Def[T]

  override def argElt[T: Manifest](base: Exp[_], index: Int): Exp[T] = Argument[T](base, index)
}

trait ScalaGenArguments extends ScalaGenBase {
  val IR: ArgumentsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Argument(x, i) =>  emitValDef(sym, "" + quote(x) + "._" + i)
    case _ => super.emitNode(sym, rhs)
  }
}