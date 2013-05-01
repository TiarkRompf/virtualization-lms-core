package scala.lms
package ops

import internal.GenericCodegen
import java.io.PrintWriter
import scala.reflect.SourceContext

trait LiftTupleTypes { this: TypeRepBase =>
  implicit def liftTuple2[T1, T2](implicit t1: TypeRep[T1], t2: TypeRep[T2]): TypeRep[Tuple2[T1, T2]] = {
    implicit val (mf1, mf2) = (t1.mf, t2.mf)
    typeRep[Tuple2[T1, T2]]
  }

  implicit def liftTuple3[T1, T2, T3](implicit t1: TypeRep[T1], t2: TypeRep[T2], t3: TypeRep[T3]): TypeRep[Tuple3[T1, T2, T3]] = {
    implicit val (mf1, mf2, mf3) = (t1.mf, t2.mf, t3.mf)
    typeRep[Tuple3[T1, T2, T3]]
  }

  implicit def liftTuple4[T1, T2, T3, T4](implicit t1: TypeRep[T1], t2: TypeRep[T2], t3: TypeRep[T3], t4: TypeRep[T4]): TypeRep[Tuple4[T1, T2, T3, T4]] = {
    implicit val (mf1, mf2, mf3, mf4) = (t1.mf, t2.mf, t3.mf, t4.mf)
    typeRep[Tuple4[T1, T2, T3, T4]]
  }

  implicit def liftTuple5[T1, T2, T3, T4, T5](implicit t1: TypeRep[T1], t2: TypeRep[T2], t3: TypeRep[T3], t4: TypeRep[T4], t5: TypeRep[T5]): TypeRep[Tuple5[T1, T2, T3, T4, T5]] = {
    implicit val (mf1, mf2, mf3, mf4, mf5) = (t1.mf, t2.mf, t3.mf, t4.mf, t5.mf)

    typeRep[Tuple5[T1, T2, T3, T4, T5]]
  }
}

trait TupleOps extends Base with LiftTupleTypes {
  implicit def make_tuple2[A:TypeRep,B:TypeRep](t: (Rep[A], Rep[B]))(implicit pos: SourceContext) : Rep[(A,B)]
  implicit def make_tuple3[A:TypeRep,B:TypeRep,C:TypeRep](t: (Rep[A], Rep[B], Rep[C]))(implicit pos: SourceContext) : Rep[(A,B,C)]
  implicit def make_tuple4[A:TypeRep,B:TypeRep,C:TypeRep,D:TypeRep](t: (Rep[A], Rep[B], Rep[C], Rep[D]))(implicit pos: SourceContext) : Rep[(A,B,C,D)]
  implicit def make_tuple5[A:TypeRep,B:TypeRep,C:TypeRep,D:TypeRep,E:TypeRep](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]))(implicit pos: SourceContext) : Rep[(A,B,C,D,E)]

  implicit def t2[A:TypeRep,B:TypeRep](t: Rep[(A,B)])(implicit pos: SourceContext) =
    ((tuple2_get1(t),tuple2_get2(t)))
  implicit def t3[A:TypeRep,B:TypeRep,C:TypeRep](t: Rep[(A,B,C)])(implicit pos: SourceContext) =
    ((tuple3_get1(t),tuple3_get2(t),tuple3_get3(t)))
  implicit def t4[A:TypeRep,B:TypeRep,C:TypeRep,D:TypeRep](t: Rep[(A,B,C,D)])(implicit pos: SourceContext) =
    ((tuple4_get1(t),tuple4_get2(t),tuple4_get3(t),tuple4_get4(t)))
  implicit def t5[A:TypeRep,B:TypeRep,C:TypeRep,D:TypeRep,E:TypeRep](t: Rep[(A,B,C,D,E)])(implicit pos: SourceContext) =
    ((tuple5_get1(t),tuple5_get2(t),tuple5_get3(t),tuple5_get4(t),tuple5_get5(t)))

  def tuple2_get1[A:TypeRep](t: Rep[(A,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple2_get2[B:TypeRep](t: Rep[(_,B)])(implicit pos: SourceContext) : Rep[B]

  def tuple3_get1[A:TypeRep](t: Rep[(A,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple3_get2[B:TypeRep](t: Rep[(_,B,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple3_get3[C:TypeRep](t: Rep[(_,_,C)])(implicit pos: SourceContext) : Rep[C]

  def tuple4_get1[A:TypeRep](t: Rep[(A,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple4_get2[B:TypeRep](t: Rep[(_,B,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple4_get3[C:TypeRep](t: Rep[(_,_,C,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple4_get4[D:TypeRep](t: Rep[(_,_,_,D)])(implicit pos: SourceContext) : Rep[D]

  def tuple5_get1[A:TypeRep](t: Rep[(A,_,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple5_get2[B:TypeRep](t: Rep[(_,B,_,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple5_get3[C:TypeRep](t: Rep[(_,_,C,_,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple5_get4[D:TypeRep](t: Rep[(_,_,_,D,_)])(implicit pos: SourceContext) : Rep[D]
  def tuple5_get5[E:TypeRep](t: Rep[(_,_,_,_,E)])(implicit pos: SourceContext) : Rep[E]
}

trait TupleOpsExp extends TupleOps with StructExpOpt {

  implicit def make_tuple2[A:TypeRep,B:TypeRep](t: (Exp[A],Exp[B]))(implicit pos: SourceContext) : Exp[(A,B)] = struct(classTag[(A,B)], "_1" -> t._1, "_2" -> t._2)
  implicit def make_tuple3[A:TypeRep,B:TypeRep,C:TypeRep](t: (Exp[A],Exp[B],Exp[C]))(implicit pos: SourceContext) : Exp[(A,B,C)] = struct(classTag[(A,B,C)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3)
  implicit def make_tuple4[A:TypeRep,B:TypeRep,C:TypeRep,D:TypeRep](t: (Exp[A],Exp[B],Exp[C],Exp[D]))(implicit pos: SourceContext) : Exp[(A,B,C,D)] = struct(classTag[(A,B,C,D)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3, "_4" -> t._4)
  implicit def make_tuple5[A:TypeRep,B:TypeRep,C:TypeRep,D:TypeRep,E:TypeRep](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]))(implicit pos: SourceContext) : Exp[(A,B,C,D,E)] = struct(classTag[(A,B,C,D,E)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3, "_4" -> t._4, "_5" -> t._5)

  def tuple2_get1[A:TypeRep](t: Exp[(A,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple2_get2[B:TypeRep](t: Exp[(_,B)])(implicit pos: SourceContext) = field[B](t, "_2")

  def tuple3_get1[A:TypeRep](t: Exp[(A,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple3_get2[B:TypeRep](t: Exp[(_,B,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple3_get3[C:TypeRep](t: Exp[(_,_,C)])(implicit pos: SourceContext) = field[C](t, "_3")

  def tuple4_get1[A:TypeRep](t: Exp[(A,_,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple4_get2[B:TypeRep](t: Exp[(_,B,_,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple4_get3[C:TypeRep](t: Exp[(_,_,C,_)])(implicit pos: SourceContext) = field[C](t, "_3")
  def tuple4_get4[D:TypeRep](t: Exp[(_,_,_,D)])(implicit pos: SourceContext) = field[D](t, "_4")

  def tuple5_get1[A:TypeRep](t: Exp[(A,_,_,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple5_get2[B:TypeRep](t: Exp[(_,B,_,_,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple5_get3[C:TypeRep](t: Exp[(_,_,C,_,_)])(implicit pos: SourceContext) = field[C](t, "_3")
  def tuple5_get4[D:TypeRep](t: Exp[(_,_,_,D,_)])(implicit pos: SourceContext) = field[D](t, "_4")
  def tuple5_get5[E:TypeRep](t: Exp[(_,_,_,_,E)])(implicit pos: SourceContext) = field[E](t, "_5")

  object Both { def unapply[T](x:T):Some[(T,T)] = Some((x,x)) }
}

trait TupleGenBase extends GenericCodegen with BaseGenStruct {
  val IR: TupleOpsExp
  import IR._

  override def remap[A](m:TypeRep[A]) = m.erasure.getSimpleName match {
    case "Tuple2" => IR.structName(m)
    case "Tuple3" => IR.structName(m)
    case "Tuple4" => IR.structName(m)
    case "Tuple5" => IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenTupleOps extends ScalaGenBase with TupleGenBase with ScalaGenStruct { val IR: TupleOpsExp }
