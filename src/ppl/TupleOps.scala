package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import java.io.PrintWriter

trait TupleOps extends Base {

  implicit def toTupleOps[A,B](t: Rep[(A,B)]) = new TupleOps(t)

  class TupleOps[A,B](t: Rep[(A,B)]) {
    def _1 = tuple_1(t)
    def _2 = tuple_2(t)
  }

  def tuple_1[A,B](t: Rep[(A,B)]) : Rep[A]
  def tuple_2[A,B](t: Rep[(A,B)]) : Rep[B]
}

trait TupleOpsExp extends TupleOps with BaseExp {
  implicit def toETuple2[A,B](t: (Rep[A],Rep[B])) = toAtom(ETuple2(t._1, t._2))

  case class ETuple2[A,B](a: Exp[A], b: Exp[B]) extends Def[(A,B)]
  case class TupleAccess1[A,B](t: Exp[(A,B)]) extends Def[A]
  case class TupleAccess2[A,B](t: Exp[(A,B)]) extends Def[B]

  def tuple_1[A,B](t: Exp[(A,B)]) : Exp[A] = t match {
    case Def(ETuple2(a,b)) => a
    case _ => TupleAccess1(t)
  }
  def tuple_2[A,B](t: Exp[(A,B)]) : Exp[B] = t match {
    case Def(ETuple2(a,b)) => b
    case _ => TupleAccess2(t)
  }
}

trait ScalaGenTuple extends ScalaGenBase with TupleOpsExp {

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ETuple2(a,b)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + ")")
    case TupleAccess1(t) =>
      emitValDef(sym, quote(t) + "._1")
    case TupleAccess2(t) =>
      emitValDef(sym, quote(t) + "._2")
    case _ => super.emitNode(sym, rhs)
  }

}
