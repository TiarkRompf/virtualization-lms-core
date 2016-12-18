package scala.lms
package common

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * Taken char-for-char from the delite-develop branch of lms
 */

trait TupleOps extends Base {
  val tuple_elems: Seq[String]

  implicit def make_tuple2[A:Manifest,B:Manifest](t: (Rep[A], Rep[B]))(implicit pos: SourceContext) : Rep[(A,B)]
  implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Rep[A], Rep[B], Rep[C]))(implicit pos: SourceContext) : Rep[(A,B,C)]
  implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D]))(implicit pos: SourceContext) : Rep[(A,B,C,D)]
  implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]))(implicit pos: SourceContext) : Rep[(A,B,C,D,E)]

  implicit def t2[A:Manifest,B:Manifest](t: Rep[(A,B)])(implicit pos: SourceContext) =
    ((tuple2_get1(t),tuple2_get2(t)))
  implicit def t3[A:Manifest,B:Manifest,C:Manifest](t: Rep[(A,B,C)])(implicit pos: SourceContext) =
    ((tuple3_get1(t),tuple3_get2(t),tuple3_get3(t)))
  implicit def t4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: Rep[(A,B,C,D)])(implicit pos: SourceContext) =
    ((tuple4_get1(t),tuple4_get2(t),tuple4_get3(t),tuple4_get4(t)))
  implicit def t5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: Rep[(A,B,C,D,E)])(implicit pos: SourceContext) =
    ((tuple5_get1(t),tuple5_get2(t),tuple5_get3(t),tuple5_get4(t),tuple5_get5(t)))

  def tuple2_get1[A:Manifest](t: Rep[(A,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple2_get2[B:Manifest](t: Rep[(_,B)])(implicit pos: SourceContext) : Rep[B]

  def tuple3_get1[A:Manifest](t: Rep[(A,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple3_get2[B:Manifest](t: Rep[(_,B,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple3_get3[C:Manifest](t: Rep[(_,_,C)])(implicit pos: SourceContext) : Rep[C]

  def tuple4_get1[A:Manifest](t: Rep[(A,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple4_get2[B:Manifest](t: Rep[(_,B,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple4_get3[C:Manifest](t: Rep[(_,_,C,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple4_get4[D:Manifest](t: Rep[(_,_,_,D)])(implicit pos: SourceContext) : Rep[D]

  def tuple5_get1[A:Manifest](t: Rep[(A,_,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple5_get2[B:Manifest](t: Rep[(_,B,_,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple5_get3[C:Manifest](t: Rep[(_,_,C,_,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple5_get4[D:Manifest](t: Rep[(_,_,_,D,_)])(implicit pos: SourceContext) : Rep[D]
  def tuple5_get5[E:Manifest](t: Rep[(_,_,_,_,E)])(implicit pos: SourceContext) : Rep[E]
  def hashTuple2(t: Rep[(_,_)]): Rep[Int]
  def compareTuple2(t1: Rep[(_,_)], t2: Rep[(_,_)]): Rep[Boolean]
  def hashTuple3(t: Rep[(_,_,_)]): Rep[Int]
  def compareTuple3(t1: Rep[(_,_,_)], t2: Rep[(_,_,_)]): Rep[Boolean]
  def hashTuple5(t: Rep[(_,_,_,_,_)]): Rep[Int]
  def compareTuple5(t1: Rep[(_,_,_,_,_)], t2: Rep[(_,_,_,_,_)]): Rep[Boolean]
}

object TupleOpsGenType { var isCGen: scala.Boolean = false }

trait TupleOpsExp extends TupleOps with StructExpOpt {
  case class ETuple2[A:Manifest,B:Manifest](_1: Exp[A],_2: Exp[B]) extends Def[(A,B)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
  }
  case class ETuple3[A:Manifest,B:Manifest,C:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C]) extends Def[(A,B,C)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
    val m3 = manifest[C]
  }
  case class ETuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C],_4: Exp[D]) extends Def[(A,B,C,D)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
    val m3 = manifest[C]
    val m4 = manifest[D]
  }
  case class ETuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C],_4: Exp[D],_5: Exp[E]) extends Def[(A,B,C,D,E)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
    val m3 = manifest[C]
    val m4 = manifest[D]
    val m5 = manifest[E]
  }

  val tuple_elems: Seq[String] = List("_1", "_2", "_3", "_4", "_5")

  implicit def make_tuple2[A:Manifest,B:Manifest](t: (Exp[A],Exp[B]))(implicit pos: SourceContext) : Exp[(A,B)] = {
	  if (TupleOpsGenType.isCGen) struct(classTag[(A,B)], tuple_elems(0) -> t._1, tuple_elems(1) -> t._2)
	  else ETuple2(t._1, t._2)
  }
  implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Exp[A],Exp[B],Exp[C]))(implicit pos: SourceContext) : Exp[(A,B,C)] = {
      if (TupleOpsGenType.isCGen) struct(classTag[(A,B,C)], tuple_elems(0) -> t._1, tuple_elems(1) -> t._2, tuple_elems(2) -> t._3)
	  else ETuple3(t._1, t._2, t._3)
  }
  implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D]))(implicit pos: SourceContext) : Exp[(A,B,C,D)] = {
      if (TupleOpsGenType.isCGen) struct(classTag[(A,B,C,D)], tuple_elems(0) -> t._1, tuple_elems(1) -> t._2, tuple_elems(2) -> t._3, tuple_elems(3) -> t._4)
	  else ETuple4(t._1, t._2, t._3, t._4)
  }
  implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]))(implicit pos: SourceContext) : Exp[(A,B,C,D,E)] = {
      if (TupleOpsGenType.isCGen) struct(classTag[(A,B,C,D,E)], tuple_elems(0) -> t._1, tuple_elems(1) -> t._2, tuple_elems(2) -> t._3, tuple_elems(3) -> t._4, tuple_elems(4) -> t._5)
	  else ETuple5(t._1, t._2, t._3, t._4, t._5)
  }
  
  def hashTuple2(t: Rep[(_,_)]): Rep[Int] = reflectEffect(HashTuple2(t))
  def compareTuple2(t1: Rep[(_,_)], t2: Rep[(_,_)]): Rep[Boolean] = reflectEffect(CompareTuple2(t1,t2))
  def hashTuple3(t: Rep[(_,_,_)]): Rep[Int] = reflectEffect(HashTuple3(t))
  def compareTuple3(t1: Rep[(_,_,_)], t2: Rep[(_,_,_)]): Rep[Boolean] = reflectEffect(CompareTuple3(t1,t2))
  def hashTuple5(t: Rep[(_,_,_,_,_)]): Rep[Int] = reflectEffect(HashTuple5(t))
  def compareTuple5(t1: Rep[(_,_,_,_,_)], t2: Rep[(_,_,_,_,_)]): Rep[Boolean] = reflectEffect(CompareTuple5(t1,t2))

  case class Tuple2Access1[A:Manifest](t: Exp[(A,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple2Access2[B:Manifest](t: Exp[(_,B)]) extends Def[B] { val m = manifest[B] }
  case class Tuple3Access1[A:Manifest](t: Exp[(A,_,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple3Access2[B:Manifest](t: Exp[(_,B,_)]) extends Def[B] { val m = manifest[B] }
  case class Tuple3Access3[C:Manifest](t: Exp[(_,_,C)]) extends Def[C] { val m = manifest[C] }
  case class Tuple4Access1[A:Manifest](t: Exp[(A,_,_,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple4Access2[B:Manifest](t: Exp[(_,B,_,_)]) extends Def[B] { val m = manifest[B] }
  case class Tuple4Access3[C:Manifest](t: Exp[(_,_,C,_)]) extends Def[C] { val m = manifest[C] }
  case class Tuple4Access4[D:Manifest](t: Exp[(_,_,_,D)]) extends Def[D] { val m = manifest[D] }
  case class Tuple5Access1[A:Manifest](t: Exp[(A,_,_,_,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple5Access2[B:Manifest](t: Exp[(_,B,_,_,_)]) extends Def[B] { val m = manifest[B] }
  case class Tuple5Access3[C:Manifest](t: Exp[(_,_,C,_,_)]) extends Def[C] { val m = manifest[C] }
  case class Tuple5Access4[D:Manifest](t: Exp[(_,_,_,D,_)]) extends Def[D] { val m = manifest[D] }
  case class Tuple5Access5[E:Manifest](t: Exp[(_,_,_,_,E)]) extends Def[E] { val m = manifest[E] }
  case class HashTuple2(t: Exp[(_,_)]) extends Def[Int]
  case class CompareTuple2(t1: Exp[(_,_)], t2: Exp[(_,_)]) extends Def[Boolean]
  case class HashTuple3(t: Exp[(_,_,_)]) extends Def[Int]
  case class CompareTuple3(t1: Exp[(_,_,_)], t2: Exp[(_,_,_)]) extends Def[Boolean]
  case class HashTuple5(t: Exp[(_,_,_,_,_)]) extends Def[Int]
  case class CompareTuple5(t1: Exp[(_,_,_,_,_)], t2: Exp[(_,_,_,_,_)]) extends Def[Boolean]

  def tuple2_get1[A:Manifest](t: Exp[(A,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple2(a,b)) => a
    case _ =>
        field[A](t, tuple_elems(0))
  }
  def tuple2_get2[B:Manifest](t: Exp[(_,B)])(implicit pos: SourceContext) = t match {
    case Def(ETuple2(a,b)) => b
    case _ =>
        field[B](t, tuple_elems(1))
  }

  def tuple3_get1[A:Manifest](t: Exp[(A,_,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple3(a,b,c)) => a
    case _ =>
        field[A](t, tuple_elems(0))
  }
  def tuple3_get2[B:Manifest](t: Exp[(_,B,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple3(a,b,c)) => b
    case _ =>
        field[B](t, tuple_elems(1))
  }
  def tuple3_get3[C:Manifest](t: Exp[(_,_,C)])(implicit pos: SourceContext) = t match {
    case Def(ETuple3(a,b,c)) => c
    case _ =>
        field[C](t, tuple_elems(2))
  }

  def tuple4_get1[A:Manifest](t: Exp[(A,_,_,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple4(a,b,c,d)) => a
    case _ =>
        field[A](t, tuple_elems(0))
  }
  def tuple4_get2[B:Manifest](t: Exp[(_,B,_,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple4(a,b,c,d)) => b
    case _ =>
        field[B](t, tuple_elems(1))
  }
  def tuple4_get3[C:Manifest](t: Exp[(_,_,C,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple4(a,b,c,d)) => c
    case _ =>
        field[C](t, tuple_elems(2))
  }
  def tuple4_get4[D:Manifest](t: Exp[(_,_,_,D)])(implicit pos: SourceContext) = t match {
    case Def(ETuple4(a,b,c,d)) => d
    case _ =>
        field[D](t, tuple_elems(3))
  }

  def tuple5_get1[A:Manifest](t: Exp[(A,_,_,_,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple5(a,b,c,d,e)) => a
    case _ =>
        field[A](t, tuple_elems(0))
  }
  def tuple5_get2[B:Manifest](t: Exp[(_,B,_,_,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple5(a,b,c,d,e)) => b
    case _ =>
        field[B](t, tuple_elems(1))
  }
  def tuple5_get3[C:Manifest](t: Exp[(_,_,C,_,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple5(a,b,c,d,e)) => c
    case _ =>
        field[C](t, tuple_elems(2))
  }
  def tuple5_get4[D:Manifest](t: Exp[(_,_,_,D,_)])(implicit pos: SourceContext) = t match {
    case Def(ETuple5(a,b,c,d,e)) => d
    case _ =>
        field[D](t, tuple_elems(3))
  }
  def tuple5_get5[E:Manifest](t: Exp[(_,_,_,_,E)])(implicit pos: SourceContext) = t match {
    case Def(ETuple5(a,b,c,d,e)) => e
    case _ =>
        field[E](t, tuple_elems(4))
  }

  object Both { def unapply[T](x:T):Some[(T,T)] = Some((x,x)) }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ETuple2(a,b)     => make_tuple2(f(a),f(b))(e.m1,e.m2,pos)
    case e@Tuple2Access1(t) => tuple2_get1(f(t))(mtype(e.m),pos)
    case e@Tuple2Access2(t) => tuple2_get2(f(t))(mtype(e.m),pos)
    case Reflect(e@Tuple2Access1(t), u, es) => reflectMirrored(Reflect(Tuple2Access1(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Tuple2Access2(t), u, es) => reflectMirrored(Reflect(Tuple2Access2(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    
    case e@ETuple3(a,b,c)   => make_tuple3(f(a),f(b),f(c))(e.m1,e.m2,e.m3,pos)
    case e@Tuple3Access1(t) => tuple3_get1(f(t))(mtype(e.m),pos)
    case e@Tuple3Access2(t) => tuple3_get2(f(t))(mtype(e.m),pos)
    case e@Tuple3Access3(t) => tuple3_get3(f(t))(mtype(e.m),pos)
    case Reflect(e@Tuple3Access1(t), u, es) => reflectMirrored(Reflect(Tuple3Access1(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Tuple3Access2(t), u, es) => reflectMirrored(Reflect(Tuple3Access2(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Tuple3Access3(t), u, es) => reflectMirrored(Reflect(Tuple3Access3(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))

    case e@ETuple4(a,b,c,d) => make_tuple4(f(a),f(b),f(c),f(d))(e.m1,e.m2,e.m3,e.m4,pos)
    case e@Tuple4Access1(t) => tuple4_get1(f(t))(mtype(e.m),pos)
    case e@Tuple4Access2(t) => tuple4_get2(f(t))(mtype(e.m),pos)
    case e@Tuple4Access3(t) => tuple4_get3(f(t))(mtype(e.m),pos)
    case e@Tuple4Access4(t) => tuple4_get4(f(t))(mtype(e.m),pos)
    case Reflect(e@Tuple4Access1(t), u, es) => reflectMirrored(Reflect(Tuple4Access1(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Tuple4Access2(t), u, es) => reflectMirrored(Reflect(Tuple4Access2(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Tuple4Access3(t), u, es) => reflectMirrored(Reflect(Tuple4Access3(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@Tuple4Access4(t), u, es) => reflectMirrored(Reflect(Tuple4Access4(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))    

    case e@ETuple5(a,b,c,d,g) => make_tuple5(f(a),f(b),f(c),f(d),f(g))(e.m1,e.m2,e.m3,e.m4,e.m5,pos)
    case e@Tuple5Access1(t)   => tuple5_get1(f(t))(mtype(e.m),pos)
    case e@Tuple5Access2(t)   => tuple5_get2(f(t))(mtype(e.m),pos)
    case e@Tuple5Access3(t)   => tuple5_get3(f(t))(mtype(e.m),pos)
    case e@Tuple5Access4(t)   => tuple5_get4(f(t))(mtype(e.m),pos)
    case e@Tuple5Access5(t)   => tuple5_get5(f(t))(mtype(e.m),pos)
    case Reflect(e@Tuple5Access1(t), u, es) => reflectMirrored(Reflect(Tuple5Access1(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Tuple5Access2(t), u, es) => reflectMirrored(Reflect(Tuple5Access2(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Tuple5Access3(t), u, es) => reflectMirrored(Reflect(Tuple5Access3(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@Tuple5Access4(t), u, es) => reflectMirrored(Reflect(Tuple5Access4(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@Tuple5Access5(t), u, es) => reflectMirrored(Reflect(Tuple5Access5(f(t))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait TupleGenBase extends GenericCodegen with BaseGenStruct { val IR: TupleOpsExp }

trait ScalaGenTupleOps extends ScalaGenBase with TupleGenBase {
  val IR: TupleOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ETuple2(a,b)  =>
      emitValDef(sym, src"($a,$b)")
    case Tuple2Access1(t) => emitValDef(sym, src"$t._1")
    case Tuple2Access2(t) => emitValDef(sym, src"$t._2")

    case ETuple3(a,b,c)  =>
      emitValDef(sym, src"($a,$b,$c)")
    case Tuple3Access1(t) => emitValDef(sym, src"$t._1")
    case Tuple3Access2(t) => emitValDef(sym, src"$t._2")
    case Tuple3Access3(t) => emitValDef(sym, src"$t._3")

    case ETuple4(a,b,c,d)  =>
      emitValDef(sym, src"($a,$b,$c,$d)")
    case Tuple4Access1(t) => emitValDef(sym, src"$t._1")
    case Tuple4Access2(t) => emitValDef(sym, src"$t._2")
    case Tuple4Access3(t) => emitValDef(sym, src"$t._3")
    case Tuple4Access4(t) => emitValDef(sym, src"$t._4")

    case ETuple5(a,b,c,d,e)  =>
      emitValDef(sym, src"($a,$b,$c,$d,$e)")
    case Tuple5Access1(t) => emitValDef(sym, src"$t._1")
    case Tuple5Access2(t) => emitValDef(sym, src"$t._2")
    case Tuple5Access3(t) => emitValDef(sym, src"$t._3")
    case Tuple5Access4(t) => emitValDef(sym, src"$t._4")
    case Tuple5Access5(t) => emitValDef(sym, src"$t._5")

    case _ => super.emitNode(sym, rhs)
  }

  
/*  override def remap[A](m: Manifest[A]) = m.runtimeClass.getSimpleName match {
    case "Tuple2" => "Tuple2" + m.typeArguments.foldLeft("")((x,y) => x + remap(y).toString).replace("Array[","ArrayOf").replace("]","")
    case "Tuple3" => "Tuple3" + m.typeArguments.foldLeft("")((x,y) => x + remap(y).toString).replace("Array[","ArrayOf").replace("]","")
    case "Tuple4" => "Tuple4" + m.typeArguments.foldLeft("")((x,y) => x + remap(y).toString).replace("Array[","ArrayOf").replace("]","")
    case "Tuple5" => "Tuple5" + m.typeArguments.foldLeft("")((x,y) => x + remap(y).toString).replace("Array[","ArrayOf").replace("]","")
    case _ => super.remap(m)
  }*/
}

trait CGenTupleOps extends CGenBase with TupleGenBase with CGenStruct {
  val IR: TupleOpsExp
  import IR._ 

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
	case HashTuple2(t) => emitValDef(sym, "(int)" + quote(t) + "->_1 + (int)" + quote(t) + "->_2;")
	case CompareTuple2(t1,t2) => emitValDef(sym, quote(t1) + "->_1 == " + quote(t2) + "->_1 && " + quote(t1) + "->_2 == " + quote(t2) + "->_2;")
	case HashTuple3(t) => emitValDef(sym, "(int)" + quote(t) + "->_1 + (int)" + quote(t) + "->_2 + (int)" + quote(t) + "->_3;")
	case CompareTuple3(t1,t2) => emitValDef(sym, quote(t1) + "->_1 == " + quote(t2) + "->_1 && " + quote(t1) + "->_2 == " + quote(t2) + "->_2 && " + quote(t1) + "->_3 == " + quote(t2) + "->_3;")
	case HashTuple5(t) => emitValDef(sym, "(int)" + quote(t) + "->_1 + (int)" + quote(t) + "->_2 + (int)" + quote(t) + "->_3 + (int)" + quote(t) + "->_4 + (int)" + quote(t) + "->_5;")
	case CompareTuple5(t1,t2) => emitValDef(sym, quote(t1) + "->_1 == " + quote(t2) + "->_1 && " + quote(t1) + "->_2 == " + quote(t2) + "->_2 && " + quote(t1) + "->_3 == " + quote(t2) + "->_3 && " + quote(t1) + "->_4 == " + quote(t2) + "->_4 && " + quote(t1) + "->_5 == " + quote(t2) + "->_5;")
	case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = m.runtimeClass.getSimpleName match {
    case "Tuple2" =>
      val elems = IR.tuple_elems.take(2) zip m.typeArguments
      IR.registerStruct(IR.structName(m), elems)
      IR.structName(m) + "*"
    case "Tuple3" =>
      val elems = IR.tuple_elems.take(3) zip m.typeArguments
      IR.registerStruct(IR.structName(m), elems)
      IR.structName(m) + "*"
    case "Tuple4" =>
      val elems = IR.tuple_elems.take(4) zip m.typeArguments
      IR.registerStruct(IR.structName(m), elems)
      IR.structName(m) + "*"
    case "Tuple5" =>
      val elems = IR.tuple_elems.take(5) zip m.typeArguments
      IR.registerStruct(IR.structName(m), elems)
      IR.structName(m) + "*"
    case _ => super.remap(m)
  }
}
