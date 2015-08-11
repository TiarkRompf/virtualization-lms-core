package scala.lms
package common

import scala.lms.internal.GenericCodegen
import java.io.PrintWriter
import scala.reflect.SourceContext

trait TupleOps extends Base {
  implicit def tuple2_typ[A:Typ,B:Typ]: Typ[(A,B)]
  implicit def tuple3_typ[A:Typ,B:Typ,C:Typ]: Typ[(A,B,C)]
  implicit def tuple4_typ[A:Typ,B:Typ,C:Typ,D:Typ]: Typ[(A,B,C,D)]
  implicit def tuple5_typ[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ]: Typ[(A,B,C,D,E)]

  implicit def make_tuple2[A:Typ,B:Typ](t: (Rep[A], Rep[B]))(implicit pos: SourceContext) : Rep[(A,B)]
  implicit def make_tuple3[A:Typ,B:Typ,C:Typ](t: (Rep[A], Rep[B], Rep[C]))(implicit pos: SourceContext) : Rep[(A,B,C)]
  implicit def make_tuple4[A:Typ,B:Typ,C:Typ,D:Typ](t: (Rep[A], Rep[B], Rep[C], Rep[D]))(implicit pos: SourceContext) : Rep[(A,B,C,D)]
  implicit def make_tuple5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]))(implicit pos: SourceContext) : Rep[(A,B,C,D,E)]

  implicit def t2[A:Typ,B:Typ](t: Rep[(A,B)])(implicit pos: SourceContext) =
    ((tuple2_get1(t),tuple2_get2(t)))
  implicit def t3[A:Typ,B:Typ,C:Typ](t: Rep[(A,B,C)])(implicit pos: SourceContext) =
    ((tuple3_get1(t),tuple3_get2(t),tuple3_get3(t)))
  implicit def t4[A:Typ,B:Typ,C:Typ,D:Typ](t: Rep[(A,B,C,D)])(implicit pos: SourceContext) =
    ((tuple4_get1(t),tuple4_get2(t),tuple4_get3(t),tuple4_get4(t)))
  implicit def t5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: Rep[(A,B,C,D,E)])(implicit pos: SourceContext) =
    ((tuple5_get1(t),tuple5_get2(t),tuple5_get3(t),tuple5_get4(t),tuple5_get5(t)))

  def tuple2_get1[A:Typ](t: Rep[(A,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple2_get2[B:Typ](t: Rep[(_,B)])(implicit pos: SourceContext) : Rep[B]

  def tuple3_get1[A:Typ](t: Rep[(A,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple3_get2[B:Typ](t: Rep[(_,B,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple3_get3[C:Typ](t: Rep[(_,_,C)])(implicit pos: SourceContext) : Rep[C]

  def tuple4_get1[A:Typ](t: Rep[(A,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple4_get2[B:Typ](t: Rep[(_,B,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple4_get3[C:Typ](t: Rep[(_,_,C,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple4_get4[D:Typ](t: Rep[(_,_,_,D)])(implicit pos: SourceContext) : Rep[D]

  def tuple5_get1[A:Typ](t: Rep[(A,_,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple5_get2[B:Typ](t: Rep[(_,B,_,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple5_get3[C:Typ](t: Rep[(_,_,C,_,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple5_get4[D:Typ](t: Rep[(_,_,_,D,_)])(implicit pos: SourceContext) : Rep[D]
  def tuple5_get5[E:Typ](t: Rep[(_,_,_,_,E)])(implicit pos: SourceContext) : Rep[E]
}

trait TupleOpsExp extends TupleOps with StructExpOpt {

  implicit def make_tuple2[A:Typ,B:Typ](t: (Exp[A],Exp[B]))(implicit pos: SourceContext) : Exp[(A,B)] = struct(classTag[(A,B)], "_1" -> t._1, "_2" -> t._2)
  implicit def make_tuple3[A:Typ,B:Typ,C:Typ](t: (Exp[A],Exp[B],Exp[C]))(implicit pos: SourceContext) : Exp[(A,B,C)] = struct(classTag[(A,B,C)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3)
  implicit def make_tuple4[A:Typ,B:Typ,C:Typ,D:Typ](t: (Exp[A],Exp[B],Exp[C],Exp[D]))(implicit pos: SourceContext) : Exp[(A,B,C,D)] = struct(classTag[(A,B,C,D)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3, "_4" -> t._4)  
  implicit def make_tuple5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]))(implicit pos: SourceContext) : Exp[(A,B,C,D,E)] = struct(classTag[(A,B,C,D,E)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3, "_4" -> t._4, "_5" -> t._5) 

  def tuple2_get1[A:Typ](t: Exp[(A,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple2_get2[B:Typ](t: Exp[(_,B)])(implicit pos: SourceContext) = field[B](t, "_2")

  def tuple3_get1[A:Typ](t: Exp[(A,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple3_get2[B:Typ](t: Exp[(_,B,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple3_get3[C:Typ](t: Exp[(_,_,C)])(implicit pos: SourceContext) = field[C](t, "_3")

  def tuple4_get1[A:Typ](t: Exp[(A,_,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple4_get2[B:Typ](t: Exp[(_,B,_,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple4_get3[C:Typ](t: Exp[(_,_,C,_)])(implicit pos: SourceContext) = field[C](t, "_3")
  def tuple4_get4[D:Typ](t: Exp[(_,_,_,D)])(implicit pos: SourceContext) = field[D](t, "_4")

  def tuple5_get1[A:Typ](t: Exp[(A,_,_,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple5_get2[B:Typ](t: Exp[(_,B,_,_,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple5_get3[C:Typ](t: Exp[(_,_,C,_,_)])(implicit pos: SourceContext) = field[C](t, "_3")
  def tuple5_get4[D:Typ](t: Exp[(_,_,_,D,_)])(implicit pos: SourceContext) = field[D](t, "_4")
  def tuple5_get5[E:Typ](t: Exp[(_,_,_,_,E)])(implicit pos: SourceContext) = field[E](t, "_5")

  object Both { def unapply[T](x:T):Some[(T,T)] = Some((x,x)) }
}

trait TupleGenBase extends GenericCodegen with BaseGenStruct { 
  val IR: TupleOpsExp

  override def remap[A](m: Typ[A]) = m.erasure.getSimpleName match {
    case "Tuple2" => IR.structName(m)
    case "Tuple3" => IR.structName(m)
    case "Tuple4" => IR.structName(m)
    case "Tuple5" => IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenTupleOps extends ScalaGenBase with TupleGenBase with ScalaGenStruct { val IR: TupleOpsExp }
trait CGenTupleOps extends CGenBase with TupleGenBase with CGenStruct
trait CudaGenTupleOps extends CudaGenBase with TupleGenBase with CudaGenStruct
trait OpenCLGenTupleOps extends OpenCLGenBase with TupleGenBase with OpenCLGenStruct
