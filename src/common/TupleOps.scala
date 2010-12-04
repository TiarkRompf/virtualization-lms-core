package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaGenBase

trait TupleOps extends Base {
  implicit def make_tuple2[A:Manifest,B:Manifest](t: (Rep[A], Rep[B])) : Rep[(A,B)]
  implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Rep[A], Rep[B], Rep[C])) : Rep[(A,B,C)]
  implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D])) : Rep[(A,B,C,D)]
  implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E])) : Rep[(A,B,C,D,E)]

  implicit def t2[A:Manifest,B:Manifest](t: Rep[(A,B)]) =
    ((tuple2_get1(t),tuple2_get2(t)))
  implicit def t3[A:Manifest,B:Manifest,C:Manifest](t: Rep[(A,B,C)]) =
    ((tuple3_get1(t),tuple3_get2(t),tuple3_get3(t)))
  implicit def t4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: Rep[(A,B,C,D)]) =
    ((tuple4_get1(t),tuple4_get2(t),tuple4_get3(t),tuple4_get4(t)))
  implicit def t5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: Rep[(A,B,C,D,E)]) =
    ((tuple5_get1(t),tuple5_get2(t),tuple5_get3(t),tuple5_get4(t),tuple5_get5(t)))

  def tuple2_get1[A:Manifest](t: Rep[(A,_)]) : Rep[A]
  def tuple2_get2[B:Manifest](t: Rep[(_,B)]) : Rep[B]

  def tuple3_get1[A:Manifest](t: Rep[(A,_,_)]) : Rep[A]
  def tuple3_get2[B:Manifest](t: Rep[(_,B,_)]) : Rep[B]
  def tuple3_get3[C:Manifest](t: Rep[(_,_,C)]) : Rep[C]

  def tuple4_get1[A:Manifest](t: Rep[(A,_,_,_)]) : Rep[A]
  def tuple4_get2[B:Manifest](t: Rep[(_,B,_,_)]) : Rep[B]
  def tuple4_get3[C:Manifest](t: Rep[(_,_,C,_)]) : Rep[C]
  def tuple4_get4[D:Manifest](t: Rep[(_,_,_,D)]) : Rep[D]

  def tuple5_get1[A:Manifest](t: Rep[(A,_,_,_,_)]) : Rep[A]
  def tuple5_get2[B:Manifest](t: Rep[(_,B,_,_,_)]) : Rep[B]
  def tuple5_get3[C:Manifest](t: Rep[(_,_,C,_,_)]) : Rep[C]
  def tuple5_get4[D:Manifest](t: Rep[(_,_,_,D,_)]) : Rep[D]
  def tuple5_get5[E:Manifest](t: Rep[(_,_,_,_,E)]) : Rep[E]
}

trait TupleOpsExp extends TupleOps with BaseExp {
  implicit def make_tuple2[A:Manifest,B:Manifest](t: (Exp[A],Exp[B])) : Exp[(A,B)] = ETuple2(t._1, t._2)
  implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Exp[A],Exp[B],Exp[C])) : Exp[(A,B,C)] = ETuple3(t._1, t._2, t._3)
  implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D])) : Exp[(A,B,C,D)] = ETuple4(t._1, t._2, t._3, t._4)
  implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E])) : Exp[(A,B,C,D,E)] = ETuple5(t._1, t._2, t._3, t._4, t._5)

  case class ETuple2[A:Manifest,B:Manifest](_1: Exp[A],_2: Exp[B]) extends Def[(A,B)]
  case class ETuple3[A:Manifest,B:Manifest,C:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C]) extends Def[(A,B,C)]
  case class ETuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C],_4: Exp[D]) extends Def[(A,B,C,D)]
  case class ETuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C],_4: Exp[D],_5: Exp[E]) extends Def[(A,B,C,D,E)]

  case class Tuple2Access1[A:Manifest](t: Exp[(A,_)]) extends Def[A]
  case class Tuple2Access2[B:Manifest](t: Exp[(_,B)]) extends Def[B]
  case class Tuple3Access1[A:Manifest](t: Exp[(A,_,_)]) extends Def[A]
  case class Tuple3Access2[B:Manifest](t: Exp[(_,B,_)]) extends Def[B]
  case class Tuple3Access3[C:Manifest](t: Exp[(_,_,C)]) extends Def[C]
  case class Tuple4Access1[A:Manifest](t: Exp[(A,_,_,_)]) extends Def[A]
  case class Tuple4Access2[B:Manifest](t: Exp[(_,B,_,_)]) extends Def[B]
  case class Tuple4Access3[C:Manifest](t: Exp[(_,_,C,_)]) extends Def[C]
  case class Tuple4Access4[D:Manifest](t: Exp[(_,_,_,D)]) extends Def[D]
  case class Tuple5Access1[A:Manifest](t: Exp[(A,_,_,_,_)]) extends Def[A]
  case class Tuple5Access2[B:Manifest](t: Exp[(_,B,_,_,_)]) extends Def[B]
  case class Tuple5Access3[C:Manifest](t: Exp[(_,_,C,_,_)]) extends Def[C]
  case class Tuple5Access4[D:Manifest](t: Exp[(_,_,_,D,_)]) extends Def[D]
  case class Tuple5Access5[E:Manifest](t: Exp[(_,_,_,_,E)]) extends Def[E]

  def tuple2_get1[A:Manifest](t: Exp[(A,_)]) = t match {
    case Def(ETuple2(a,b)) => a
    case _ => Tuple2Access1(t)
  }
  def tuple2_get2[B:Manifest](t: Exp[(_,B)]) = t match {
    case Def(ETuple2(a,b)) => b
    case _ => Tuple2Access2(t)
  }

  def tuple3_get1[A:Manifest](t: Exp[(A,_,_)]) = t match {
    case Def(ETuple3(a,b,c)) => a
    case _ => Tuple3Access1(t)
  }
  def tuple3_get2[B:Manifest](t: Exp[(_,B,_)]) = t match {
    case Def(ETuple3(a,b,c)) => b
    case _ => Tuple3Access2(t)
  }
  def tuple3_get3[C:Manifest](t: Exp[(_,_,C)]) = t match {
    case Def(ETuple3(a,b,c)) => c
    case _ => Tuple3Access3(t)
  }

  def tuple4_get1[A:Manifest](t: Exp[(A,_,_,_)]) = t match {
    case Def(ETuple4(a,b,c,d)) => a
    case _ => Tuple4Access1(t)
  }
  def tuple4_get2[B:Manifest](t: Exp[(_,B,_,_)]) = t match {
    case Def(ETuple4(a,b,c,d)) => b
    case _ => Tuple4Access2(t)
  }
  def tuple4_get3[C:Manifest](t: Exp[(_,_,C,_)]) = t match {
    case Def(ETuple4(a,b,c,d)) => c
    case _ => Tuple4Access3(t)
  }
  def tuple4_get4[D:Manifest](t: Exp[(_,_,_,D)]) = t match {
    case Def(ETuple4(a,b,c,d)) => d
    case _ => Tuple4Access4(t)
  }

  def tuple5_get1[A:Manifest](t: Exp[(A,_,_,_,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => a
    case _ => Tuple5Access1(t)
  }
  def tuple5_get2[B:Manifest](t: Exp[(_,B,_,_,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => b
    case _ => Tuple5Access2(t)
  }
  def tuple5_get3[C:Manifest](t: Exp[(_,_,C,_,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => c
    case _ => Tuple5Access3(t)
  }
  def tuple5_get4[D:Manifest](t: Exp[(_,_,_,D,_)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => d
    case _ => Tuple5Access4(t)
  }
  def tuple5_get5[E:Manifest](t: Exp[(_,_,_,_,E)]) = t match {
    case Def(ETuple5(a,b,c,d,e)) => e
    case _ => Tuple5Access5(t)
  }
}

trait ScalaGenTupleOps extends ScalaGenBase {
  val IR: TupleOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ETuple2(a,b)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + ")")
    case Tuple2Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple2Access2(t) => emitValDef(sym, quote(t) + "._2")

    case ETuple3(a,b,c)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + "," + quote(c) + ")")
    case Tuple3Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple3Access2(t) => emitValDef(sym, quote(t) + "._2")
    case Tuple3Access3(t) => emitValDef(sym, quote(t) + "._3")

    case ETuple4(a,b,c,d)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + "," + quote(c) + "," + quote(d) + ")")
    case Tuple4Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple4Access2(t) => emitValDef(sym, quote(t) + "._2")
    case Tuple4Access3(t) => emitValDef(sym, quote(t) + "._3")
    case Tuple4Access4(t) => emitValDef(sym, quote(t) + "._4")

    case ETuple5(a,b,c,d,e)  =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + "," + quote(c) + "," + quote(d) + "," + quote(e) + ")")
    case Tuple5Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple5Access2(t) => emitValDef(sym, quote(t) + "._2")
    case Tuple5Access3(t) => emitValDef(sym, quote(t) + "._3")
    case Tuple5Access4(t) => emitValDef(sym, quote(t) + "._4")
    case Tuple5Access5(t) => emitValDef(sym, quote(t) + "._5")

    case _ => super.emitNode(sym, rhs)
  }

}