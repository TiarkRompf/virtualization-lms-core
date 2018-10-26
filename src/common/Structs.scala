package scala.lms
package common

import scala.lms.common._

import scala.lms.util.OverloadHack
import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen}
import scala.reflect.{SourceContext, RefinedManifest}
import java.io.{StringWriter,PrintWriter}
import scala.language.dynamics

/**
 * Taken char-for-char from the delite-develop branch of lms
 */

trait Structs extends Base with Variables {

  /**
   * Allows to write things like “val z = new Record { val re = 1.0; val im = -1.0 }; print(z.re)”
   */
  abstract class Record extends Struct
  abstract class CompositeRecord[T1:Manifest,T2:Manifest] extends Record
  def __new[T : Manifest](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = record_new(args)

  class RecordOps[T1<:Record:Manifest](record: Rep[T1]) extends Dynamic {
	def apply[TF: Manifest](field: String): Rep[TF] = record_select[T1,TF](record, field)
    def selectDynamic[TF : Manifest](field: String): Rep[TF] = record_select[T1,TF](record, field)
	def concatenate[T2: Manifest](record2: Rep[T2], leftAlias: String = "", rightAlias: String = ""): Rep[CompositeRecord[T1,T2]] = record_concatenate[T1,T2](record, record2, leftAlias, rightAlias)
	def print = record_print[T1](record)
  }
  implicit def recordToRecordOps[T<:Record:Manifest](record: Rep[T]) = new RecordOps[T](record)
  implicit def varrecordToRecordOps[T<:Record:Manifest](record: Var[T]) = new RecordOps[T](readVar(record))
  def infix_f[T<:Record:Manifest](record: Rep[T]) = new RecordOps(record)(manifest[T])

  def registerStruct[T<:Record:Manifest](name: String, elems: Seq[(String, Manifest[_])])
  def structName[T](m: Manifest[T]): String

  object DefaultRecord {
	def apply[T:Manifest]() = default_record[T]()
  }
  def default_record[T: Manifest](): Rep[T]

  def record_new[T : Manifest](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]): Rep[T]
  def record_new[T:Manifest](structName: String, fieldSyms: Seq[(String, Rep[Any])]): Rep[T]
  def record_select[T1:Manifest, TF:Manifest](record: Rep[T1], field: String): Rep[TF]
  def record_concatenate[T1:Manifest, T2:Manifest](record: Rep[T1], record2: Rep[T2], leftAlias: String = "", rightAlias: String = ""): Rep[CompositeRecord[T1,T2]]
  def record_print[T<:Record:Manifest](record: Rep[T]): Rep[Unit]
  def field[T:Manifest](struct: Rep[Any], index: String)(implicit pos: SourceContext): Rep[T]
  def record_hash[T:Manifest](record: Rep[T]): Rep[Int]
  def record_equals[T:Manifest](record1: Rep[T], record2: Rep[T]): Rep[Boolean]
}

trait StructTags {
  abstract class StructTag[+T]
  case class ClassTag[T](name: String) extends StructTag[T]
  case class NestClassTag[C[_],T](elem: StructTag[T]) extends StructTag[C[T]]
  case class AnonTag[T](fields: RefinedManifest[T]) extends StructTag[T]
  case class MapTag[T]() extends StructTag[T]
}

trait StructExp extends Structs with StructTags with EffectExp with WhileExp with VariablesExp with ObjectOpsExp with StringOpsExp with FunctionsExp with MiscOpsExp with RangeOpsExp with ArrayOps with BooleanOps with Equal with PrimitiveOps with NumericOps with OrderingOps {

  // TODO: structs should take Def parameters that define how to generate constructor and accessor calls
  val structHeaderFile = "default"

  abstract class AbstractStruct[T] extends Def[T] {
    val tag: StructTag[T]
    val elems: Seq[(String, Rep[Any])]
  }

  abstract class AbstractField[T] extends Def[T] {
    val struct: Rep[Any]
    val index: String
  }

  /* override def fresh[T:Manifest] = manifest[T] match {
case s if s <:< manifest[Record] =>
val m = spawnRefinedManifest
super.fresh(m)
case _ => super.fresh
} */ //TODO: best way to ensure full structural type is always available?

  object Struct {
    def unapply[T](d: Def[T]) = unapplyStruct(d)
  }

  def unapplyStruct[T](d: Def[T]): Option[(StructTag[T], Seq[(String, Rep[Any])])] = d match {
    case s: AbstractStruct[T] => Some((s.tag, s.elems))
    case _ => None
  }

  object Field {
    def unapply[T](d: Def[T]) = unapplyField(d)
  }

  def unapplyField[T](d: Def[T]): Option[(Rep[Any], String)] = d match {
    case f: AbstractField[T] => Some((f.struct, f.index))
    case _ => None
  }

  case class DefaultRecordDef[T:Manifest]() extends Def[T] {
	val m = manifest[T]
  }
  def recordFieldTypes2[T:Manifest]: List[(String,Manifest[_])] = manifest[T] match {
    case m: RefinedManifest[T] => m.fields
    case m if m.toString.contains("CompositeRecord") =>
      val (mA:Manifest[Record])::(mB:Manifest[Record])::Nil = m.typeArguments
      recordFieldTypes2(mA) ++ recordFieldTypes2(mB)
    case m => throw new RuntimeException(">> " + m.toString)
  }

  case class SimpleStruct[T:Manifest](tag: StructTag[T], elems: Seq[(String, Rep[Any])]) extends AbstractStruct[T] {
	if (tag.isInstanceOf[ClassTag[_]]) registerStruct(tag.asInstanceOf[ClassTag[_]].name, recordFieldTypes2[T])
  }
  case class ConcatenateRecords[T1:Manifest, T2:Manifest](x: Rep[T1], y: Rep[T2], leftAlias: String, rightAlias: String) extends Def[CompositeRecord[T1,T2]] {
	val m1 = manifest[T1]
	val m2 = manifest[T2]
  }
  case class RecordPrint[T<:Record:Manifest](rec: Rep[T]) extends Def[Unit]
  case class RecordHash[T:Manifest](rec: Rep[T]) extends Def[Int]
  case class RecordEquals[T:Manifest](rec1: Rep[T], rec2: Rep[T]) extends Def[Boolean]
  case class FieldApply[T:Manifest](struct: Rep[Any], index: String) extends AbstractField[T] {
	val m = manifest[T]
  }
  case class FieldUpdate[T:Manifest](struct: Exp[Any], index: String, rhs: Exp[T]) extends Def[Unit]

  def struct[T:Manifest](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit o: Overloaded1, pos: SourceContext): Rep[T] = struct[T](tag, elems)
  def struct[T:Manifest](tag: StructTag[T], elems: Seq[(String, Rep[Any])])(implicit pos: SourceContext): Rep[T] = SimpleStruct(tag, elems)

  def field[T:Manifest](struct: Rep[Any], index: String)(implicit pos: SourceContext): Rep[T] = FieldApply[T](struct, index)
  def var_field[T:Manifest](struct: Rep[Any], index: String)(implicit pos: SourceContext): Var[T] = Variable(FieldApply[Var[T]](struct, index))
  def field_update[T:Manifest](struct: Exp[Any], index: String, rhs: Exp[T]): Exp[Unit] = reflectWrite(struct)(FieldUpdate(struct, index, rhs))

  def default_record[T: Manifest]() = DefaultRecordDef[T]()
  def record_new[T : Manifest](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]) = {
    val x: Sym[T] = Sym[T](-99) // self symbol -- not defined anywhere, so make it obvious!! (TODO)
    val fieldSyms = fields map {
      case (index, false, rhs) => (index, rhs(x))
      case (index, true, rhs) => (index, var_new(rhs(x)).e)
    }
    struct(ClassTag(structName(manifest[T])),/*AnonTag(manifest[T].asInstanceOf[RefinedManifest[T]]),*/ fieldSyms)
  }

  def record_new[T:Manifest](structName: String, fieldSyms: Seq[(String, Rep[Any])]) = {
     struct(ClassTag(structName), fieldSyms)
  }

  def record_select[T1 : Manifest, TF: Manifest](record: Rep[T1], fieldName: String) = {
    field[TF](record, fieldName)
  }
  def record_concatenate[T1:Manifest,T2:Manifest](record: Rep[T1], record2: Rep[T2], leftAlias: String = "", rightAlias: String = "") = {
	val name1 = structName(manifest[T1]).replace("CompositeRecord","")
	val s1 = {
		if (encounteredStructs.contains(name1)) encounteredStructs(name1)
		else name1.split("Anon").filter(x => x.length != 0).map(x => encounteredStructs("Anon" + x)).flatten.toList
	}
	val name2 = structName(manifest[T2]).replace("CompositeRecord","")
	val s2 = {
		if (encounteredStructs.contains(name2)) encounteredStructs(name2)
		else name2.split("Anon").filter(x => x.length != 0).map(x => encounteredStructs("Anon" + x)).flatten.toList
	}
    val elems: Seq[(String, Manifest[_])] = {
		for (s <- s1) yield leftAlias  + s._1 -> s._2
	} ++ {
		for (s <- s2) yield rightAlias + s._1 -> s._2
	}
	registerStruct(name1 + name2, elems)
	/*reflectEffect*/(ConcatenateRecords(record, record2, leftAlias, rightAlias)(record.tp,record2.tp))
  }
  def record_print[T<:Record:Manifest](rec: Rep[T]) = reflectEffect(RecordPrint[T](rec))
  def record_hash[T:Manifest](rec: Rep[T]) = reflectEffect(RecordHash(rec))
  def record_equals[T:Manifest](rec1: Rep[T], rec2: Rep[T]) = reflectEffect(RecordEquals(rec1,rec2))

  override def syms(e: Any): List[Sym[Any]] = e match {
    case s:AbstractStruct[_] => s.elems.flatMap(e => syms(e._2)).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s:AbstractStruct[_] => s.elems.flatMap(e => symsFreq(e._2)).toList
    case _ => super.symsFreq(e)
  }

  override def effectSyms(e: Any): List[Sym[Any]] = e match {
    case s:AbstractStruct[_] => s.elems.flatMap(e => effectSyms(e._2)).toList
    case _ => super.effectSyms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case s:AbstractStruct[_] => Nil //struct creation doesn't de-reference any of its inputs
    case _ => super.readSyms(e)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => Nil
    case FieldUpdate(s,x,b) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleStruct(tag,elems) => elems.collect { case (k,v:Sym[Any]) => v }.toList
    case FieldApply(s,x) => Nil
    case FieldUpdate(s,x,b) => syms(b)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => syms(s)
    case FieldUpdate(s,x,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => Nil
    case FieldUpdate(s,x,b) => Nil
    case _ => super.copySyms(e)
  }



  // TODO: read/write/copy summary

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleStruct(tag, elems) => struct(tag, elems map { case (k,v) => (k, f(v)) })(mtype(manifest[A]),pos)
    case fa@FieldApply(struct, key) => record_select(f(struct), key)(struct.tp,fa.m)
    case cr@ConcatenateRecords(rec1,rec2,leftAlias,rightAlias) => record_concatenate(f(rec1),f(rec2),leftAlias,rightAlias)(rec1.tp,rec2.tp)
    case Reflect(fa@FieldApply(struct, key), u, es) => record_select(f(struct),key)(struct.tp,fa.m)
    case Reflect(FieldUpdate(struct, key, rhs), u, es) => reflectMirrored(Reflect(FieldUpdate(f(struct), key, f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(SimpleStruct(tag, elems), u, es) => reflectMirrored(Reflect(SimpleStruct(tag, elems map { case (k,v) => (k, f(v)) }), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(RecordPrint(rec), u, es) => reflectMirrored(Reflect(RecordPrint(f(rec)), mapOver(f,u), f(es)))
    case Reflect(RecordHash(rec), u, es) => reflectMirrored(Reflect(RecordHash(f(rec)), mapOver(f,u), f(es)))
    case Reflect(RecordEquals(rec1,rec2), u, es) => reflectMirrored(Reflect(RecordEquals(f(rec1),f(rec2)), mapOver(f,u), f(es)))
    case Reflect(cr@ConcatenateRecords(rec1,rec2,leftAlias,rightAlias), u, es) => record_concatenate(f(rec1),f(rec2),leftAlias,rightAlias)(rec1.tp,rec2.tp)
    case dr@DefaultRecordDef() => default_record()(dr.m)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  def structName[T](m: Manifest[T]): String = m match {
    // FIXME: move to codegen? we should be able to have different policies/naming schemes
    case s if s <:< manifest[CompositeRecord[Any,Any]] => m.typeArguments.map(structName(_)).mkString
    case rm: RefinedManifest[_] => "Anon" + math.abs(rm.fields.zipWithIndex.map { case (f, i) => (f._1.## + f._2.toString.##) * (i + 1) /* don't want to multiply by 0 */ }.sum)
	case s if m.erasure.isArray => "ArrayOf" + m.typeArguments.map(a => structName(a)).mkString("")
    case s if (m <:< manifest[AnyVal]) => m.toString
    case s if m.erasure.getSimpleName == "Tuple2" => "Tuple2" + m.typeArguments.foldLeft("")((x,y) => x + structName(y))
    case _ => m.erasure.getSimpleName + m.typeArguments.map(a => structName(a)).mkString("")
  }

  def classTag[T:Manifest] = ClassTag[T](structName(manifest[T]))

  override def object_tostring(x: Exp[Any])(implicit pos: SourceContext): Exp[String] = x match {
    case Def(s@Struct(tag, elems)) => //tag(elem1, elem2, ...)
      val e = elems.map(e=>string_plus(unit(e._1 + " = "), object_tostring(e._2))).reduceLeft((l,r)=>string_plus(string_plus(l,unit(", ")),r))
      string_plus(unit(structName(s.tp)+"("),string_plus(e,unit(")")))
    case _ => super.object_tostring(x)
  }

  def registerStruct[T<:Record:Manifest](name: String, elems: Seq[(String, Manifest[_])]) = {
    if (elems.exists(_._2 == manifest[Any])) {
      System.out.println(s"Any found in Record $name, $elems")
    }
  	encounteredStructs += name -> elems
  }
  val encounteredStructs = new scala.collection.mutable.HashMap[String, Seq[(String, Manifest[_])]]
}

trait StructExpOpt extends StructExp {

  //this is factored out so it can be called by domain-specific field access methods
  def fieldLookup[T](struct: Exp[Any], index: String): Option[Exp[T]] = {
    def lookup(elems: Seq[(String, Exp[Any])]) = elems.find(_._1 == index).getOrElse(
      throw new RuntimeException("ERROR: " + index + " is not a field of type " + struct.tp)
    )._2.asInstanceOf[Exp[T]]

    struct match {
      case Def(Struct(tag, elems)) => Some(lookup(elems))
      case Def(Reflect(Struct(tag, elems),u,es)) => Some(lookup(elems))
      case _ => None
    }
  }

  override def field[T:Manifest](struct: Exp[Any], index: String)(implicit pos: SourceContext): Exp[T] = { if (manifest[T] == manifest[Any]) ???; fieldLookup[T](struct, index) } match {
    // the two variable pattern matches each seem to miss certain cases, so both are needed. why?
    case Some(Def(Reflect(NewVar(x),u,es))) => super.field(struct, index)
    case Some(x: Exp[Var[T]]) if x.tp == manifest[Var[T]] => super.field(struct, index) //readVar(Variable(x))
    case Some(x) => x
    case _ => super.field[T](struct, index)
  }

  //TODO: need to be careful unwrapping Structs of vars since partial unwrapping can result in reads & writes to two different memory locations in the generated code
  //(the original var and the struct)
  /* override def var_field[T:Manifest](struct: Exp[Any], index: String)(implicit pos: SourceContext): Var[T] = fieldLookup(struct, index) match {
case Some(x: Exp[Var[T]]) if x.tp == manifest[Var[T]] => Variable(x)
case Some(x) => throw new RuntimeException("ERROR: " + index + " is not a variable field of type " + struct.tp)
case None => super.var_field(struct, index)
} */

}

trait StructExpOptCommon extends StructExpOpt with VariablesExp with IfThenElseExp {

  override def structName[T](m: Manifest[T]): String = m.erasure.getSimpleName match {
    case "Variable" => structName(m.typeArguments(0))
    case _ => super.structName(m)
  }

  override def var_new[T:Manifest](init: Exp[T])(implicit pos: SourceContext): Var[T] = init match {
    case Def(Struct(tag, elems)) =>
      //val r = Variable(struct(tag, elems.mapValues(e=>var_new(e).e))) // DON'T use mapValues!! <--lazy
      Variable(struct[Variable[T]](NestClassTag[Variable,T](tag), elems.map(p=>(p._1,var_new(p._2)(p._2.tp,pos).e))))
    case _ =>
      super.var_new(init)
  }

  override def var_assign[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = (lhs,rhs) match {
    case (Variable(Def(Struct(NestClassTag(tagL),elemsL: Seq[(String,Exp[Variable[Any]])]))), Def(Struct(tagR, elemsR))) =>
      assert(tagL == tagR)
      for (((lk,lv), (rk,rv)) <- elemsL zip elemsR) {
        assert(lk == rk)
        var_assign(Variable(lv), rv)(rv.tp, pos)
      }
      Const(())
    case (Variable(Def(Struct(NestClassTag(tag), elems: Seq[(String,Exp[Variable[Any]])]))), Def(r)) => //TODO: keep this?
      for ((k,v) <- elems) {
        var_assign(Variable(v), field(r,k)(mtype(v.tp),pos))(unwrap(v.tp),pos)
      }
      Const(())
    case (Variable(Def(Reflect(Field(struct,idx),_,_))), rhs) =>
      field_update(struct, idx, rhs)
    case _ => super.var_assign(lhs, rhs)
  }

  private def unwrap[A](m:Manifest[Variable[A]]): Manifest[A] = m.typeArguments match {
    case a::_ => mtype(a)
    case _ => printerr("warning: expect type Variable[A] but got "+m); mtype(manifest[Any])
  }

  override def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext): Exp[T] = v match {
    case Variable(Def(Struct(NestClassTag(tag), elems: Seq[(String,Exp[Variable[Any]])]))) =>
      struct[T](tag, elems.map(p=>(p._1,readVar(Variable(p._2))(unwrap(p._2.tp), pos))))
    case Variable(Def(Field(struct,idx))) =>
      field[T](struct, idx)
    case _ => super.readVar(v)
  }

  override def ifThenElse[T:Manifest](cond: Rep[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = (a,b) match {
    case (Block(Def(Struct(tagA,elemsA))), Block(Def(Struct(tagB, elemsB)))) =>
      assert(tagA == tagB)
      val elemsNew = for (((lk,lv), (rk,rv)) <- elemsA zip elemsB) yield {
        assert(lk == rk)
        lk -> ifThenElse(cond, Block(lv), Block(rv))(rv.tp, pos)
      }
      struct[T](tagA, elemsNew)
    case _ => super.ifThenElse(cond,a,b)
  }

}


// the if/phi stuff is more general than structs -- could be used for variable assignments as well

trait StructFatExp extends StructExp with BaseFatExp

trait StructFatExpOptCommon extends StructFatExp with StructExpOptCommon with IfThenElseFatExp {

  // Phi nodes:
  // created by splitting an IfThenElse node
  // a1 and b1 will be the effects of the original IfThenElse, packaged into blocks with a unit result

  case class Phi[T](cond: Exp[Boolean], a1: Block[Unit], val thenp: Block[T], b1: Block[Unit], val elsep: Block[T])(val parent: Exp[Unit]) extends AbstractIfThenElse[T] // parent points to conditional
  def phi[T:Manifest](c: Exp[Boolean], a1: Block[Unit], a2: Exp[T], b1: Block[Unit], b2: Exp[T])(parent: Exp[Unit]): Exp[T] = if (a2 == b2) a2 else Phi(c,a1,Block(a2),b1,Block(b2))(parent)
  def phiB[T:Manifest](c: Exp[Boolean], a1: Block[Unit], a2: Block[T], b1: Block[Unit], b2: Block[T])(parent: Exp[Unit]): Exp[T] = if (a2 == b2) a2.res else Phi(c,a1,a2,b1,b2)(parent) // FIXME: duplicate

  override def syms(x: Any): List[Sym[Any]] = x match {
    // case Phi(c,a,u,b,v) => syms(List(c,a,b))
    case _ => super.syms(x)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    // case Phi(c,a,u,b,v) => freqNormal(c) ++ freqCold(a) ++ freqCold(b)
    case _ => super.symsFreq(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Phi(c,a,u,b,v) => effectSyms(a):::effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case p@Phi(c,a,u,b,v) => phiB(f(c),f(a),f(u),f(b),f(v))(f(p.parent))
    case _ => super.mirror(e,f)
  }

  def deReify[T:Manifest](a: Block[T]): (Block[Unit], Rep[T]) = a match { // take Reify(stms, e) and return Reify(stms, ()), e
    case Block(Def(Reify(x,es,u))) => (Block(Reify(Const(()), es, u)), x)
    case Block(x) => (Block(Const(())), x)
  }


  override def ifThenElse[T:Manifest](cond: Rep[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = (deReify(a),deReify(b)) match {
    case ((u, Def(Struct(tagA,elemsA))), (v, Def(Struct(tagB, elemsB)))) =>
      //assert(tagA == tagB, tagA+" !== "+tagB)
      if (tagA != tagB) System.out.println("ERROR: "+tagA+" !== "+tagB)
      // create stm that computes all values at once
      // return struct of syms
      val combinedResult = super.ifThenElse(cond,u,v)

      val elemsNew = for (((lk,lv), (rk,rv)) <- elemsA zip elemsB) yield {
        assert(lk == rk)
        lk -> phi(cond,u,lv,v,rv)(combinedResult)(mtype(lv.tp))
      }
      struct[T](tagA, elemsNew)

    case _ => super.ifThenElse(cond,a,b)
  }
}

trait BaseGenFatStruct extends GenericFatCodegen {
  val IR: StructFatExpOptCommon // TODO: restructure traits, maybe move this to if then else codegen?
  import IR._

  // TODO: implement regular fatten ?

  override def fattenAll(e: List[Stm]): List[Stm] = {
    val m = e collect {
      case t@TP(sym, p @ Phi(c,a,u,b,v)) => t
    } groupBy {
      case TP(sym, p @ Phi(c,a,u,b,v)) => p.parent
    }

    //println("grouped: ")
    //println(m.mkString("\n"))
    def fatphi(s:Sym[Unit]) = m.get(s).map { phis =>
      val ss = phis collect { case TP(s, _) => s }
      val us = phis collect { case TP(_, Phi(c,a,u,b,v)) => u } // assert c,a,b match
      val vs = phis collect { case TP(_, Phi(c,a,u,b,v)) => v }
      val c = phis collect { case TP(_, Phi(c,a,u,b,v)) => c } reduceLeft { (c1,c2) => assert(c1 == c2); c1 }
      TTP(ss, phis map (_.rhs), SimpleFatIfThenElse(c,us,vs))
    }
    def fatif(s:Sym[Unit],o:Def[Unit],c:Exp[Boolean],a:Block[Unit],b:Block[Unit]) = fatphi(s) match {
      case Some(TTP(ss, oo, SimpleFatIfThenElse(c2,us,vs))) =>
        assert(c == c2)
        TTP(s::ss, o::oo, SimpleFatIfThenElse(c,a::us,b::vs))
      case _ =>
        TTP(s::Nil, o::Nil, SimpleFatIfThenElse(c,a::Nil,b::Nil))
    }

    val orphans = m.keys.toList.filterNot(k => e exists (_.lhs contains k)) // parent if/else might have been removed!

    val r = e.flatMap {
      case TP(sym, p@Phi(c,a,u,b,v)) => Nil
      case TP(sym:Sym[Unit], o@IfThenElse(c,a:Block[Unit],b:Block[Unit])) => List(fatif(sym,o.asInstanceOf[Def[Unit]],c,a,b))
      case TP(sym:Sym[Unit], o@Reflect(IfThenElse(c,a:Block[Unit],b:Block[Unit]),_,_)) => List(fatif(sym,o.asInstanceOf[Def[Unit]],c,a,b))
      case t => List(fatten(t))
    } ++ orphans.map { case s: Sym[Unit] => fatphi(s).get } // be fail-safe here?

    //r.foreach(println)
    r
  }
}

trait ScalaGenFatStruct extends ScalaGenStruct with BaseGenFatStruct {
  val IR: StructFatExpOptCommon // TODO: restructure traits, maybe move this to if then else codegen?
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case p@Phi(c,a,u,b,v) =>
      emitValDef(sym, "XXX " + rhs + " // parent " + quote(p.parent))
    case _ => super.emitNode(sym, rhs)
  }
}

trait BaseGenStruct extends GenericNestedCodegen {
  val IR: StructExp
  import IR._

  //Moved encounteredStructs to IR
}

trait ScalaGenStruct extends ScalaGenBase with BaseGenStruct {
  val IR: StructExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
	case r@DefaultRecordDef() =>
	  def defaultValue[T: ClassManifest]: T = classManifest[T].erasure.toString match {
		case "boolean" => false.asInstanceOf[T]
		case "byte" => (0: Byte).asInstanceOf[T]
		case "short" => (0: Short).asInstanceOf[T]
		case "char" => '\0'.asInstanceOf[T]
		case "int" => 0.asInstanceOf[T]
		case "long" => 0L.asInstanceOf[T]
		case "float" => 0.0F.asInstanceOf[T]
		case "double" => 0.0.asInstanceOf[T]
	    case _ => null.asInstanceOf[T]
	  }
	  val name = structName(r.m)
	  val fieldTypes = encounteredStructs(name).map(_._2)
	  val caseClassStr = if (fieldTypes.length < 22) "" else "new "
	  stream.println("val " + quote(sym) + " = " + caseClassStr + name + "(" + (for (f <- fieldTypes) yield defaultValue(f)).mkString(",") + ")")
    case Struct(tag, elems) =>
      val header = if (elems.length < 22) "" else "new "
      emitValDef(sym, header + structName(sym.tp) + "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
    case FieldApply(struct, index) =>
      emitValDef(sym, quote(struct) + "." + index)
    case FieldUpdate(struct, index, rhs) =>
      emitValDef(sym, quote(struct) + "." + index + " = " + quote(rhs))
    case RecordPrint(t) =>
      stream.println(src"println($t.toString)")
    case RecordHash(t) =>
		emitValDef(sym, quote(t) + ".hashCode")
    case RecordEquals(t1,t2) =>
		emitValDef(sym, quote(t1) + " equals " + quote(t2))
	case c@ConcatenateRecords(record1, record2, leftAlias, rightAlias) =>
		val name1 = structName(c.m1).replace("CompositeRecord", "")
		val s1 = encounteredStructs(name1)
		val name2 = structName(c.m2).replace("CompositeRecord", "")
		val s2 = encounteredStructs(name2)
		val header = if (s1.length + s2.length < 22) "" else "new "
		emitValDef(sym, header + name1 + name2 + "(" + s1.map(x => quote(record1) + "." + x._1).mkString(",") + "," + s2.map(x => quote(record2) + "." + x._1).mkString(",") + ")")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = m match {
    case s if s <:< manifest[Record] => structName(m).replace("CompositeRecord","")
    case _ => super.remap(m)
  }

  override def emitDataStructures(stream: PrintWriter) {
    for ((name, elems) <- encounteredStructs) {
      stream.println()
	  if (elems.length < 22) stream.print("case ")
      stream.print("class " + name + "(")
      stream.println(elems.map(e => "val " + e._1 + ": " + remap(e._2)).mkString(", ") + ") {")
	  stream.println("override def toString() = {")
      stream.println(elems.map(e => {
		if (e._2 == manifest[Array[Byte]]) "(if(" + e._1 + " != null) new String(" + e._1 + ") else \"\")"
		else if (e._2.erasure.isArray) e._1 + ".mkString(\" \")"
		else e._1
	  }).mkString(" + \"|\" + ") + "+\"\"")
	  stream.println("}")
	  stream.println("override def hashCode() = {")
      stream.println(elems.map(e => {
		if (e._2.erasure.isArray) e._1 + ".foldLeft(0) { (cnt,x) => cnt + x.## }"
		else e._1 + ".hashCode"
	  }).mkString(" + "))
	  stream.println("}")
	  stream.println("override def equals(y: Any) = {")
	  stream.println("val e = y.asInstanceOf[" + name + "]")
      stream.println(elems.map(e => {
		if (e._2.erasure.isArray) e._1 + ".corresponds(e." + e._1 + "){_==_}"
		else e._1 + " == e." + e._1
	  }).mkString(" && "))
	  stream.println("}")
	  stream.println("}")
    }
    stream.flush()
    super.emitDataStructures(stream)
  }

}

trait CGenStruct extends CGenBase with BaseGenStruct {
  val IR: StructExp
  import IR._
  def remapToPrintFDescr[A:Manifest](m: Manifest[A]): String = m match {
	case s if m == manifest[Int] => "%d|"
	case s if m == manifest[Double] => "%lf|"
	case s if m == manifest[java.lang.Character] => "%c|"
	case s if m == manifest[Long] => "%lu|"
	case s if m == manifest[Array[Byte]] => "%s|"
    case s if m == manifest[Byte] => "%c|"
	case s if m == manifest[java.lang.String] => "%s|"
	case dflt@_ => throw new Exception("Unsupported printf descr " + dflt + " when emitting struct. Stringifying...")
  }

  override def lowerNode[A:Manifest](sym: Sym[A], rhs: Def[A]) = rhs match {
	case fa@FieldApply(rec,fldname) =>
        if (encounteredStructs.get(structName(rec.tp)).isEmpty) {
          System.out.println("XX struct not encountered: "+rec.tp)
          System.out.println("XX at: "+(sym:AnyRef)+"/"+rhs)
          //registerStruct(rec.tp)
        }

        val fldtype = encounteredStructs(structName(rec.tp)).find(e => e._1 == fldname).get._2
        val lfldtype = {
            val s = fresh(fldtype)
            s.tp = fldtype
            remapManifest(s)(fldtype.asInstanceOf[Manifest[Any]])
        }
		sym.atPhase(LIRLowering) {
            val e = (if (sym.tp.erasure.isArray)
                reflectEffect(FieldApply(LIRLowering(rec),fldname)(remapManifest(fresh(sym.tp))(sym.tp)))
            else record_select(LIRLowering(rec),fldname)(rec.tp,lfldtype))
			e.tp = lfldtype
			e.asInstanceOf[Exp[A]]
		}
    case RecordHash(t) =>
        sym.atPhase(LIRLowering) {
            val rec = LIRLowering(t)
	        def hash[T:Manifest](lrec: Exp[T]): Exp[Size] = {
			    val fields = encounteredStructs(structName(manifest[T]))
                var h = var_new[Size](unit(0))
                fields.foreach { fld =>
                    if (fld._2.erasure.isPrimitive || fld._2 == manifest[java.lang.Character])
                      h += field(lrec, fld._1)(fld._2,implicitly[SourceContext])
					else if (fld._2 == manifest[Array[Byte]]) {
        				val fldtype = {
            				val s = fresh(fld._2)
            				s.tp = fld._2
            				remapManifest(s)(fld._2.asInstanceOf[Manifest[Any]])
        				}
						val a = field(lrec, fld._1)(fld._2, implicitly[SourceContext])
						a.tp = fldtype
						val arr = field[Array[Byte]](a,"array")
						val arrlen = field[Size](a,"length").asInstanceOf[Rep[Size]]
						var i = var_new[Size](unit(0))
                		var harr = var_new[Size](unit(0))
						__whileDo(i < arrlen && arr(i) != unit('\0'), {
                    		harr += arr(i)
							i+=1
						})
						h += readVar(h)
					}
					else throw new Exception("Unsupported hash type " + fld._2 + " in record hash!")
                }
                h
            }
            val newrec = fresh(rec.tp) // so that there are no effects
            val func = uninlinedFunc1(newrec.asInstanceOf[Sym[Any]], reifyEffects({hash(newrec)(rec.tp);}))
            doApply(func, rec.asInstanceOf[Sym[Any]]).asInstanceOf[Exp[A]]
        }
	case RecordEquals(r1,r2) =>
        sym.atPhase(LIRLowering) {
            val rec1 = LIRLowering(r1)
            val rec2 = LIRLowering(r2)
            def equals[T:Manifest](lrec1: Exp[T], lrec2: Exp[T]): Exp[Boolean] = {
			    val fields = encounteredStructs(structName(manifest[T]))
                var e = var_new[Boolean](unit(true))
                fields.foreach { fld =>
                    if (fld._2.erasure.isPrimitive || fld._2 == manifest[java.lang.Character])
                        e = e && __equal(field(lrec1, fld._1)(fld._2,implicitly[SourceContext]).asInstanceOf[Exp[A]], field(lrec2, fld._1)(fld._2,implicitly[SourceContext]).asInstanceOf[Exp[A]])
                	else if (fld._2 == manifest[Array[Byte]]) {
        				val fldtype = {
            				val s = fresh(fld._2)
            				s.tp = fld._2
            				remapManifest(s)(fld._2.asInstanceOf[Manifest[Any]])
        				}
						val a1 = field(lrec1, fld._1)(fld._2, implicitly[SourceContext])
						a1.tp = fldtype
						val a2 = field(lrec2, fld._1)(fld._2, implicitly[SourceContext])
						a2.tp = fldtype
						val arr2 = field[Array[Byte]](LIRLowering(a2),"array")
                        e = e && __equal(a1.asInstanceOf[Exp[A]], arr2.asInstanceOf[Exp[A]])
					}
					else throw new Exception("Unsupported equals type " + fld._2 + " in record equals!")
				}
                e
            }
            val (newrec1,newrec2) = (fresh(rec1.tp), fresh(rec2.tp)) // so that there are no effects
            val func = uninlinedFunc2(newrec1.asInstanceOf[Sym[Any]], newrec2.asInstanceOf[Sym[Any]], reifyEffects({equals(newrec1,newrec2)(rec1.tp);})).asInstanceOf[Exp[Function1[Tuple2[_,_],Boolean]]]
            doApply(func, Const(scala.Tuple2(rec1.asInstanceOf[Sym[Any]], rec2.asInstanceOf[Sym[Any]]))).asInstanceOf[Exp[A]]
        }
    case RecordPrint(r) =>
        sym.atPhase(LIRLowering) {
            val rec = LIRLowering(r)
            def recordprint[T:Manifest](lrec: Exp[T]): Unit = {
				val fields = encounteredStructs(structName(manifest[T]))
                fields.foreach { fld =>
                    val (fldname, fldtype) = (fld._1, fld._2)
                    val repfld = field(lrec, fldname)(fldtype,implicitly[SourceContext])
					if (fldtype <:< manifest[Record])
						recordprint(repfld)(fldtype.asInstanceOf[Manifest[Any]])
					else if (fldtype.erasure.getSimpleName == "LoweredArray") {
						val arrayType = fldtype.typeArguments.head
						val arrayElemType = arrayType.typeArguments.head
    				    val array = field(repfld, "array")(arrayType, implicitly[SourceContext])
                        if (arrayType == manifest[Array[Byte]])
                            printf(remapToPrintFDescr(arrayType), array)
                        else {
	    					val length = field[Long](repfld, "length")
                            for (i <- unit(0L) until length: Rep[LongRange])
                                printf(remapToPrintFDescr(arrayElemType), array_apply(array.asInstanceOf[Exp[Array[Any]]],i)(arrayElemType.asInstanceOf[Manifest[Any]], implicitly[SourceContext]))
                        }
					}
                    else printf(remapToPrintFDescr(fld._2), repfld)
                }
            }
            val newrec = fresh(rec.tp) // so that there are no effects
            val func = uninlinedFunc1(newrec.asInstanceOf[Sym[Any]], reifyEffects({recordprint(newrec)(rec.tp); printf("\n");}))
            doApply(func, rec.asInstanceOf[Sym[Any]]).asInstanceOf[Exp[A]]
        }
    case _ => super.lowerNode(sym,rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  	case r@DefaultRecordDef() =>
  	  def defaultValue[T: ClassManifest]: T = classManifest[T].erasure.toString match {
  		  case "boolean" => false.asInstanceOf[T]
  		  case "byte" => (0: Byte).asInstanceOf[T]
  		  case "short" => (0: Short).asInstanceOf[T]
  		  case "char" => '\0'.asInstanceOf[T]
  		  case "int" => 0.asInstanceOf[T]
  		  case "long" => 0L.asInstanceOf[T]
  		  case "float" => 0.0F.asInstanceOf[T]
  		  case "double" => 0.0.asInstanceOf[T]
  	    case _ => "NULL".asInstanceOf[T]
  	  }
  	  val name = structName(r.m)
  	  val fields = encounteredStructs(name)
  	  allocStruct(sym, "struct " + structName(r.m), stream)
  	  //fields.foreach( f => stream.println(quote(sym) + "->" + f._1 + " = " + defaultValue(f._2) + ";"))
      fields.foreach( f => stream.println(quote(sym) + "." + f._1 + " = " + defaultValue(f._2) + ";"))
    case Struct(tag, elems) =>
	    val fields = encounteredStructs(tag.asInstanceOf[ClassTag[_]].name).map(x => x._1) zip elems
	    allocStruct(sym, "struct " + tag.asInstanceOf[ClassTag[_]].name, stream)
	    //fields.foreach( f => stream.println(quote(sym) + "->" + f._1 + " = " + quote(f._2._2) + ";"))
      fields.foreach( f => stream.println(quote(sym) + "." + f._1 + " = " + quote(f._2._2) + ";"))
    case fa@FieldApply(struct, index) =>
      //emitValDef(sym, quote(struct) + "->" + index + ";")
      emitValDef(sym, quote(struct) + "." + index + ";")
    case FieldUpdate(struct, index, rhs) =>
      emitValDef(sym, quote(struct) + "->" + index + " = " + quote(rhs))
    case RecordPrint(t) =>
      stream.println("print_" + structName(t.tp) + "(" + quote(t) + ");")
	  case c@ConcatenateRecords(record1, record2, leftAlias, rightAlias) =>
		  val name1 = structName(record1.tp).replace("CompositeRecord", "")
	    val s1 = encounteredStructs(name1)
		  val name2 = structName(record2.tp).replace("CompositeRecord", "")
		  val s2 = encounteredStructs(name2)
	  	allocStruct(sym, remap(sym.tp).replace("*",""), stream)
		  //stream.println(s1.map(x => quote(sym) + "->" + leftAlias + x._1 + " = " + quote(record1) + "->" + x._1).mkString(";\n") + ";")
		  //stream.println(s2.map(x => quote(sym) + "->" + rightAlias + x._1 + " = " + quote(record2) + "->" + x._1).mkString(";\n") + ";")
      stream.println(s1.map(x => quote(sym) + "." + leftAlias + x._1 + " = " + quote(record1) + "." + x._1).mkString(";\n") + ";")
      stream.println(s2.map(x => quote(sym) + "." + rightAlias + x._1 + " = " + quote(record2) + "." + x._1).mkString(";\n") + ";")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = m match {
    case s if s <:< manifest[CompositeRecord[Any,Any]] => "struct " + structName(m) // + "*"
    case s if s <:< manifest[Record] => "struct " + structName(m) // + "*"
    case s if s.toString.contains("Pointer") => // TODO find a better place
      remap(m.typeArguments.head) + "*"
    case _ =>  super.remap(m)
  }

  override def headerSet = super.headerSet + s"""\"${structHeaderFile}_datastructure.h\""""
  override def emitDataStructures(stream: PrintWriter) {


    val data = new PrintWriter(s"cqueries/${structHeaderFile}_datastructure.h")
	// Forward references to resolve dependencies
    val hs = new scala.collection.mutable.LinkedHashMap[String,Seq[(String, Manifest[_])]]
    def hit(name: String, xs: Seq[(String,Manifest[_])]): Unit = {
      xs foreach { x =>
        val name = structName(x._2)
        encounteredStructs.get(name).map(x => hit(name, x))
      }
      hs(name) = xs
    }
    encounteredStructs.foreach((hit _).tupled)

    data.println(
        s"""|#ifndef ${structHeaderFile.toUpperCase}_DATASTRUCT
        |#define ${structHeaderFile.toUpperCase}_DATASTRUCT
        |#include <stdbool.h>
        |""".stripMargin)

    for ((name, elems) <- hs) {
      data.println()
      data.println("struct " + name + " {")
      for(e <- elems) data.println(remap(e._2) + " " + e._1 + ";")
      data.println("};")
    }

    data.println("#endif")
    data.flush
    data.close
    super.emitDataStructures(stream)
  }
}
/*trait CudaGenStruct extends CudaGenBase with BaseGenStruct
trait OpenCLGenStruct extends OpenCLGenBase with BaseGenStruct

trait CudaGenFatStruct extends CudaGenStruct with BaseGenFatStruct
trait OpenCLGenFatStruct extends OpenCLGenStruct with BaseGenFatStruct
trait CGenFatStruct extends CGenStruct with BaseGenFatStruct
*/
