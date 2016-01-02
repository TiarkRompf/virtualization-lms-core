package scala.lms
package common

import reflect.{SourceContext, RefinedManifest}
import util.OverloadHack
import java.io.PrintWriter
import internal.{GenericNestedCodegen, GenericFatCodegen}

abstract class Record extends Struct

trait StructOps extends Base {

  implicit def recordTyp[T<:Record:Manifest]: Typ[T]

  /**
   * Allows to write things like “val z = new Record { val re = 1.0; val im = -1.0 }; print(z.re)”
   */

  def __new[T:Typ](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = record_new(args)

  class RecordOps(record: Rep[Record]) {
    def selectDynamic[T : Typ](field: String): Rep[T] = record_select[T](record, field)
  }
  implicit def recordToRecordOps(record: Rep[Record]) = new RecordOps(record)

  def record_new[T : Typ](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]): Rep[T]
  def record_select[T : Typ](record: Rep[Record], field: String): Rep[T]
  def field[T:Typ](struct: Rep[Any], index: String)(implicit pos: SourceContext): Rep[T]
}

trait StructTags {
  abstract class StructTag[+T]
  case class ClassTag[T](name: String) extends StructTag[T]
  case class NestClassTag[C[_],T](elem: StructTag[T]) extends StructTag[C[T]]
  case class AnonTag[T](fields: RefinedManifest[T]) extends StructTag[T]
  case class MapTag[T]() extends StructTag[T]
}

trait StructExp extends StructOps with StructTags with BaseExp with EffectExp with VariablesExp with ObjectOpsExp with StringOpsExp with OverloadHack {

  implicit def recordTyp[T<:Record:Manifest]: Typ[T] = manifestTyp

  // TODO: structs should take Def parameters that define how to generate constructor and accessor calls

  abstract class AbstractStruct[T] extends Def[T] {
    val tag: StructTag[T]
    val elems: Seq[(String, Rep[Any])]
  }

  abstract class AbstractField[T] extends Def[T] {
    val struct: Rep[Any]
    val index: String
  }

  /* override def fresh[T:Typ] = manifest[T] match {
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

  case class SimpleStruct[T](tag: StructTag[T], elems: Seq[(String, Rep[Any])]) extends AbstractStruct[T]
  case class FieldApply[T](struct: Rep[Any], index: String) extends AbstractField[T]
  case class FieldUpdate[T:Typ](struct: Exp[Any], index: String, rhs: Exp[T]) extends Def[Unit] {
    def mA = manifest[T]
  }

  def struct[T:Typ](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit o: Overloaded1, pos: SourceContext): Rep[T] = struct[T](tag, elems)
  def struct[T:Typ](tag: StructTag[T], elems: Seq[(String, Rep[Any])])(implicit pos: SourceContext): Rep[T] = SimpleStruct(tag, elems)

  def field[T:Typ](struct: Rep[Any], index: String)(implicit pos: SourceContext): Rep[T] = FieldApply[T](struct, index)
  def var_field[T:Typ](struct: Rep[Any], index: String)(implicit pos: SourceContext): Var[T] = Variable(FieldApply[Var[T]](struct, index))
  def field_update[T:Typ](struct: Exp[Any], index: String, rhs: Exp[T]): Exp[Unit] = reflectWrite(struct)(FieldUpdate(struct, index, rhs))

  def record_new[T:Typ](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]) = {
    val x: Sym[T] = Sym[T](-99) // self symbol -- not defined anywhere, so make it obvious!! (TODO)
    val fieldSyms = fields map {
      case (index, false, rhs) => (index, rhs(x))
      case (index, true, rhs) => val y = rhs(x); (index, var_new(y)(y.tp,implicitly[SourceContext]).e)
    }
    val ManifestTyp(manifest) = typ[T]
    struct(AnonTag(manifest.asInstanceOf[RefinedManifest[T]]), fieldSyms)
  }

  def record_select[T : Typ](record: Rep[Record], fieldName: String) = {
    field[T](record, fieldName)
  }

  def imm_field(struct: Exp[Any], name: String, f: Exp[Any])(implicit pos: SourceContext): Exp[Any] = {
    if (f.tp.erasure.getSimpleName == "Variable") {
      field(struct,name)(mtype(f.tp.typeArguments(0)),pos)
    } else {
      object_unsafe_immutable(f)(mtype(f.tp),pos)
    }
  }

  // don't let unsafeImmutable hide struct-ness
  override def object_unsafe_immutable[A:Typ](lhs: Exp[A])(implicit pos: SourceContext) = lhs match {
    case Def(Struct(tag,elems)) => struct[A](tag, elems.map(t => (t._1, imm_field(lhs, t._1, t._2))))
    case Def(d@Reflect(Struct(tag, elems), u, es)) => struct[A](tag, elems.map(t => (t._1, imm_field(lhs, t._1, t._2))))
    case _ => super.object_unsafe_immutable(lhs)
  }

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

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleStruct(tag, elems) => struct(tag, elems map { case (k,v) => (k, f(v)) })(mtype(manifest[A]),pos)
    case FieldApply(struct, key) => field(f(struct), key)(mtype(manifest[A]),pos)
    case Reflect(FieldApply(struct, key), u, es) => reflectMirrored(Reflect(FieldApply(f(struct), key), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@FieldUpdate(struct, key, rhs), u, es) => reflectMirrored(Reflect(FieldUpdate(f(struct), key, f(rhs))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(SimpleStruct(tag, elems), u, es) => reflectMirrored(Reflect(SimpleStruct(tag, elems map { case (k,v) => (k, f(v)) }), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  def structName[T](m: Typ[T]): String = m match {
    // FIXME: move to codegen? we should be able to have different policies/naming schemes
    case ManifestTyp(rm: RefinedManifest[_]) => "Anon" + math.abs(rm.fields.map(f => f._1.## + f._2.toString.##).sum)
    case ManifestTyp(m) if (m <:< implicitly[Manifest[AnyVal]]) => m.toString
    case _ if m.erasure.isArray => "ArrayOf" + structName(m.typeArguments.head)
    case _ => m.erasure.getSimpleName + m.typeArguments.map(a => structName(a)).mkString("")
  }

  def classTag[T:Typ] = ClassTag[T](structName(manifest[T]))

  override def object_tostring(x: Exp[Any])(implicit pos: SourceContext): Exp[String] = x match {
    case Def(s@Struct(tag, elems)) => //tag(elem1, elem2, ...)
      val e = elems.map(e=>string_plus(unit(e._1 + " = "), object_tostring(e._2))).reduceLeft((l,r)=>string_plus(string_plus(l,unit(", ")),r))
      string_plus(unit(structName(x.tp)+"("),string_plus(e,unit(")")))
    case _ => super.object_tostring(x)
  }

  def registerStruct[T](name: String, elems: Seq[(String, Rep[Any])]) {
    encounteredStructs += name -> elems.map(e => (e._1, e._2.tp))
  }
  val encounteredStructs = new scala.collection.mutable.HashMap[String, Seq[(String, Typ[_])]]
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

  override def field[T:Typ](struct: Exp[Any], index: String)(implicit pos: SourceContext): Exp[T] = fieldLookup[T](struct, index) match {
    // the two variable pattern matches each seem to miss certain cases, so both are needed. why?
    case Some(Def(Reflect(NewVar(x),u,es))) => super.field[T](struct, index)
    case Some(x: Exp[Var[T]]) if x.tp == manifest[Var[T]] => super.field[T](struct, index) //readVar(Variable(x))
    case Some(x) => x
    case _ => super.field[T](struct, index)
  }

  //TODO: need to be careful unwrapping Structs of vars since partial unwrapping can result in reads & writes to two different memory locations in the generated code
  //(the original var and the struct)
  /* override def var_field[T:Typ](struct: Exp[Any], index: String)(implicit pos: SourceContext): Var[T] = fieldLookup(struct, index) match {
    case Some(x: Exp[Var[T]]) if x.tp == manifest[Var[T]] => Variable(x)
    case Some(x) => throw new RuntimeException("ERROR: " + index + " is not a variable field of type " + struct.tp)
    case None => super.var_field(struct, index)
  } */

}

trait StructExpOptCommon extends StructExpOpt with VariablesExp with IfThenElseExp {

  override def structName[T](m: Typ[T]): String = m.erasure.getSimpleName match {
    case "Variable" => structName(m.typeArguments(0))
    case _ => super.structName(m)
  }

  override def var_new[T:Typ](init: Exp[T])(implicit pos: SourceContext): Var[T] = init match {
    case Def(Struct(tag, elems)) =>
      //val r = Variable(struct(tag, elems.mapValues(e=>var_new(e).e))) // DON'T use mapValues!! <--lazy
      Variable(struct[Variable[T]](NestClassTag[Variable,T](tag), elems.map(p=>(p._1,var_new(p._2)(p._2.tp,pos).e))))
    case _ =>
      super.var_new(init)
  }

  override def var_assign[T:Typ](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = (lhs,rhs) match {
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

  private def unwrap[A](m:Typ[Variable[A]]): Typ[A] = m.typeArguments match {
    case a::_ => mtype(a)
    case _ => printerr("warning: expect type Variable[A] but got "+m); mtype(manifest[Unit])
  }

  override implicit def readVar[T:Typ](v: Var[T])(implicit pos: SourceContext): Exp[T] = v match {
    case Variable(Def(Struct(NestClassTag(tag), elems: Seq[(String,Exp[Variable[Any]])]))) =>
      struct[T](tag, elems.map(p=>(p._1,readVar(Variable(p._2))(unwrap(p._2.tp), pos))))
    case Variable(Def(Field(struct,idx))) =>
      field[T](struct, idx)
    case _ => super.readVar(v)
  }

  override def ifThenElse[T:Typ](cond: Rep[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = (a,b) match {
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
  def phi[T:Typ](c: Exp[Boolean], a1: Block[Unit], a2: Exp[T], b1: Block[Unit], b2: Exp[T])(parent: Exp[Unit]): Exp[T] = if (a2 == b2) a2 else Phi(c,a1,Block(a2),b1,Block(b2))(parent)
  def phiB[T:Typ](c: Exp[Boolean], a1: Block[Unit], a2: Block[T], b1: Block[Unit], b2: Block[T])(parent: Exp[Unit]): Exp[T] = if (a2 == b2) a2.res else Phi(c,a1,a2,b1,b2)(parent) // FIXME: duplicate

  override def syms(x: Any): List[Sym[Any]] = x match {
    //    case Phi(c,a,u,b,v) => syms(List(c,a,b))
    case _ => super.syms(x)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    //    case Phi(c,a,u,b,v) => freqNormal(c) ++ freqCold(a) ++ freqCold(b)
    case _ => super.symsFreq(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Phi(c,a,u,b,v) => effectSyms(a):::effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case p@Phi(c,a,u,b,v) => phiB(f(c),f(a),f(u),f(b),f(v))(f(p.parent))
    case _ => super.mirror(e,f)
  }

  def deReify[T:Typ](a: Block[T]): (Block[Unit], Rep[T]) = a match { // take Reify(stms, e) and return Reify(stms, ()), e
    case Block(Def(Reify(x,es,u))) => (Block(Reify(Const(()), es, u)), x)
    case Block(x) => (Block(Const(())), x)
  }


  override def ifThenElse[T:Typ](cond: Rep[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = (deReify(a),deReify(b)) match {
    case ((u, Def(Struct(tagA,elemsA))), (v, Def(Struct(tagB, elemsB)))) =>
      //assert(tagA == tagB, tagA+" !== "+tagB)
      if (tagA != tagB) println("ERROR: "+tagA+" !== "+tagB)
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
      val c  = phis collect { case TP(_, Phi(c,a,u,b,v)) => c } reduceLeft { (c1,c2) => assert(c1 == c2); c1 }
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
    case Struct(tag, elems) =>
      registerStruct(structName(sym.tp), elems)
      emitValDef(sym, "new " + structName(sym.tp) + "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
    case FieldApply(struct, index) =>
      emitValDef(sym, quote(struct) + "." + index)
    case FieldUpdate(struct, index, rhs) =>
      emitValDef(sym, quote(struct) + "." + index + " = " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Typ[A]) = m match {
    case ManifestTyp(s) if s <:< manifest[Record] => structName(m)
    case _ => super.remap(m)
  }

  override def emitDataStructures(stream: PrintWriter) {
    for ((name, elems) <- encounteredStructs) {
      stream.println()
      stream.print("case class " + name + "(")
      stream.println(elems.map(e => e._1 + ": " + remap(e._2)).mkString(", ") + ")")
    }
    stream.flush()
    super.emitDataStructures(stream)
  }

}

trait CGenStruct extends CGenBase with BaseGenStruct
trait CudaGenStruct extends CudaGenBase with BaseGenStruct
trait OpenCLGenStruct extends OpenCLGenBase with BaseGenStruct

trait CudaGenFatStruct extends CudaGenStruct with BaseGenFatStruct
trait OpenCLGenFatStruct extends OpenCLGenStruct with BaseGenFatStruct
trait CGenFatStruct extends CGenStruct with BaseGenFatStruct
