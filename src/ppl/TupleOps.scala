package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import java.io.PrintWriter

trait TupleOps extends Base {
  // TODO: this is ugly. there must be a way to condense this and share some implementations.
  // The trick is making everything (including the result values) well-typed.

  implicit def make_tuple2[A,B](t: (Rep[A], Rep[B])) : Rep[(A,B)]
  implicit def make_tuple3[A,B,C](t: (Rep[A], Rep[B], Rep[C])) : Rep[(A,B,C)]
  implicit def make_tuple4[A,B,C,D](t: (Rep[A], Rep[B], Rep[C], Rep[D])) : Rep[(A,B,C,D)]
  implicit def make_tuple5[A,B,C,D,E](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E])) : Rep[(A,B,C,D,E)]

  implicit def tuple2ToTupleOps[A,B](t: Rep[(A,B)]) = new tuple2Ops(t)
  implicit def tuple3ToTupleOps[A,B,C](t: Rep[(A,B,C)]) = new tuple3Ops(t)
  implicit def tuple4ToTupleOps[A,B,C,D](t: Rep[(A,B,C,D)]) = new tuple4Ops(t)
  implicit def tuple5ToTupleOps[A,B,C,D,E](t: Rep[(A,B,C,D,E)]) = new tuple5Ops(t)

  class tuple2Ops[A,B](t: Rep[(A,B)]) {
    def _1 = tuple2_get1(t)
    def _2 = tuple2_get2(t)
  }

  class tuple3Ops[A,B,C](t: Rep[(A,B,C)]) {
    def _1 = tuple3_get1(t)
    def _2 = tuple3_get2(t)
    def _3 = tuple3_get3(t)
  }

  class tuple4Ops[A,B,C,D](t: Rep[(A,B,C,D)]) {
    def _1 = tuple4_get1(t)
    def _2 = tuple4_get2(t)
    def _3 = tuple4_get3(t)
    def _4 = tuple4_get4(t)
  }

  class tuple5Ops[A,B,C,D,E](t: Rep[(A,B,C,D,E)]) {
    def _1 = tuple5_get1(t)
    def _2 = tuple5_get2(t)
    def _3 = tuple5_get3(t)
    def _4 = tuple5_get4(t)
    def _5 = tuple5_get5(t)
  }

  def tuple2_get1[A](t: Rep[(A,_)]) : Rep[A]
  def tuple2_get2[B](t: Rep[(_,B)]) : Rep[B]

  def tuple3_get1[A](t: Rep[(A,_,_)]) : Rep[A]
  def tuple3_get2[B](t: Rep[(_,B,_)]) : Rep[B]
  def tuple3_get3[C](t: Rep[(_,_,C)]) : Rep[C]

  def tuple4_get1[A](t: Rep[(A,_,_,_)]) : Rep[A]
  def tuple4_get2[B](t: Rep[(_,B,_,_)]) : Rep[B]
  def tuple4_get3[C](t: Rep[(_,_,C,_)]) : Rep[C]
  def tuple4_get4[D](t: Rep[(_,_,_,D)]) : Rep[D]

  def tuple5_get1[A](t: Rep[(A,_,_,_,_)]) : Rep[A]
  def tuple5_get2[B](t: Rep[(_,B,_,_,_)]) : Rep[B]
  def tuple5_get3[C](t: Rep[(_,_,C,_,_)]) : Rep[C]
  def tuple5_get4[D](t: Rep[(_,_,_,D,_)]) : Rep[D]
  def tuple5_get5[E](t: Rep[(_,_,_,_,E)]) : Rep[E]

}

trait TupleOpsExp extends TupleOps with BaseExp {
  implicit def make_tuple2[A,B](t: (Exp[A],Exp[B])) = toAtom(ETuple2(t._1, t._2))
  implicit def make_tuple3[A,B,C](t: (Exp[A],Exp[B],Exp[C])) = toAtom(ETuple3(t._1, t._2, t._3))
  implicit def make_tuple4[A,B,C,D](t: (Exp[A],Exp[B],Exp[C],Exp[D])) = toAtom(ETuple4(t._1, t._2, t._3, t._4))
  implicit def make_tuple5[A,B,C,D,E](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E])) = toAtom(ETuple5(t._1, t._2, t._3, t._4, t._5))

  case class ETuple2[A,B](_1: Exp[A], _2: Exp[B]) extends Def[(A,B)]
  case class ETuple3[A,B,C](_1: Exp[A], _2: Exp[B], _3: Exp[C]) extends Def[(A,B,C)]
  case class ETuple4[A,B,C,D](_1: Exp[A], _2: Exp[B], _3: Exp[C], _4: Exp[D]) extends Def[(A,B,C,D)]
  case class ETuple5[A,B,C,D,E](_1: Exp[A], _2: Exp[B], _3: Exp[C], _4: Exp[D], _5: Exp[E]) extends Def[(A,B,C,D,E)]

  // these are for generating actual tuple access operations in the generated code, i.e. if we have an Exp[Tuple]
  // that is not an ETuple (e.g. a Const). I am not sure that this will ever come up though.
  case class Tuple2Access1[A](t: Exp[(A,_)]) extends Def[A]
  case class Tuple2Access2[B](t: Exp[(_,B)]) extends Def[B]
  case class Tuple3Access1[A](t: Exp[(A,_,_)]) extends Def[A]
  case class Tuple3Access2[B](t: Exp[(_,B,_)]) extends Def[B]
  case class Tuple3Access3[C](t: Exp[(_,_,C)]) extends Def[C]
  case class Tuple4Access1[A](t: Exp[(A,_,_,_)]) extends Def[A]
  case class Tuple4Access2[B](t: Exp[(_,B,_,_)]) extends Def[B]
  case class Tuple4Access3[C](t: Exp[(_,_,C,_)]) extends Def[C]
  case class Tuple4Access4[D](t: Exp[(_,_,_,D)]) extends Def[D]
  case class Tuple5Access1[A](t: Exp[(A,_,_,_,_)]) extends Def[A]
  case class Tuple5Access2[B](t: Exp[(_,B,_,_,_)]) extends Def[B]
  case class Tuple5Access3[C](t: Exp[(_,_,C,_,_)]) extends Def[C]
  case class Tuple5Access4[D](t: Exp[(_,_,_,D,_)]) extends Def[D]
  case class Tuple5Access5[E](t: Exp[(_,_,_,_,E)]) extends Def[E]

  def tuple2_get1[A](t: Exp[(A,_)]) = t match {
    case Def(ETuple2(a,b)) => a
    case _ => Tuple2Access1(t)
  }
  def tuple2_get2[B](t: Exp[(_,B)]) = t match {
    case Def(ETuple2(a,b)) => b
    case _ => Tuple2Access2(t)
  }

  def tuple3_get1[A](t: Exp[(A,_,_)]) = t match {
    case Def(ETuple3(a,b,c)) => a
    case _ => Tuple3Access1(t)
  }
  def tuple3_get2[B](t: Exp[(_,B,_)]) = t match {
    case Def(ETuple3(a,b,c)) => b
    case _ => Tuple3Access2(t)
  }
  def tuple3_get3[C](t: Exp[(_,_,C)]) = t match {
    case Def(ETuple3(a,b,c)) => c
    case _ => Tuple3Access3(t)
  }

  def tuple4_get1[A](t: Exp[(A,_,_,_)]) = t match {
    case Def(ETuple4(a,b,c,d)) => a
    case _ => Tuple4Access1(t)
  }
  def tuple4_get2[B](t: Exp[(_,B,_,_)]) = t match {
    case Def(ETuple4(a,b,c,d)) => b
    case _ => Tuple4Access2(t)
  }
  def tuple4_get3[C](t: Exp[(_,_,C,_)]) = t match {
    case Def(ETuple4(a,b,c,d)) => c
    case _ => Tuple4Access3(t)
  }
  def tuple4_get4[D](t: Exp[(_,_,_,D)]) = t match {
    case Def(ETuple4(a,b,c,d)) => d
    case _ => Tuple4Access4(t)
  }

  def tuple5_get1[A](t: Exp[(A,_,_,_,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => a
    case _ => Tuple5Access1(t)
  }
  def tuple5_get2[B](t: Exp[(_,B,_,_,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => b
    case _ => Tuple5Access2(t)
  }
  def tuple5_get3[C](t: Exp[(_,_,C,_,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => c
    case _ => Tuple5Access3(t)
  }
  def tuple5_get4[D](t: Exp[(_,_,_,D,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => d
    case _ => Tuple5Access4(t)
  }
  def tuple5_get5[E](t: Exp[(_,_,_,_,E)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => e
    case _ => Tuple5Access5(t)
  }
}

trait ScalaGenTuple extends ScalaGenBase with TupleOpsExp {

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ETuple2(a,b)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + ")")
    case ETuple3(a,b,c)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + "," + quote(c) + ")")
    case ETuple4(a,b,c,d)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + "," + quote(c) + "," + quote(d) + ")")
    case ETuple5(a,b,c,d,e)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + "," + quote(c) + "," + quote(d) + "," + quote(e) + ")")
    case _ => super.emitNode(sym, rhs)
  }

}
