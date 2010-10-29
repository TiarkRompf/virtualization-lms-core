package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaGenBase

trait ArrayOps extends Variables {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToRepArrayOps[A](x: Var[Array[A]]) = new RepArrayOpsCls(readVar(x))
  implicit def repArrayToRepArrayOps[T](a: Rep[Array[T]]) = new RepArrayOpsCls(a)
  implicit def arrayToRepArrayOps[T](a: Array[T]) = new RepArrayOpsCls(a)

  class RepArrayOpsCls[T](a: Rep[Array[T]]){
    def apply(n: Rep[Int]) = array_apply(a, n)
    def length = array_length(a)
  }
    
  def array_apply[T](x: Rep[Array[T]], n: Rep[Int]): Rep[T]
  def array_length[T](x: Rep[Array[T]]) : Rep[Int]
}

trait ArrayOpsExp extends ArrayOps with VariablesExp {
  case class ArrayLength[T](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayApply[T](x: Exp[Array[T]], n: Exp[Int]) extends Def[T]

  def array_apply[T](x: Exp[Array[T]], n: Exp[Int]): Rep[T] = ArrayApply(x, n)
  def array_length[T](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)
}

trait ScalaGenArrayOps extends ScalaGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
