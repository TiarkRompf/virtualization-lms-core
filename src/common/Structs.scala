package scala.virtualization.lms
package common

import reflect.{SourceContext, RefinedManifest}
import util.OverloadHack
import java.io.PrintWriter
import internal.{GenericNestedCodegen, GenericFatCodegen}
import collection.mutable

trait StructOps extends Base {

  abstract class Record extends Struct[Rep]

  implicit def repToStructOps(s: Rep[Record]) = new StructOpsCls(s)
  class StructOpsCls(s: Rep[Record]) {
    def selectDynamic[T:Manifest](index: String): Rep[T] = field(s, index)
  }

  def __new[T<:Struct[Rep]:Manifest](fields: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = anonStruct[T](fields)

  def anonStruct[T:Manifest](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]): Rep[T]

  def field[T:Manifest](struct: Rep[Any], index: String)(implicit pos: SourceContext): Rep[T]

}

trait StructExp extends StructOps with BaseExp with EffectExp with ObjectOpsExp with StringOpsExp with OverloadHack {

  def anonStruct[T:Manifest](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]): Rep[T] = {
    val x = fresh[T]
    if (fields exists { case (_, mutable, _) => mutable })  throw new RuntimeException("ERROR: \"var\" fields in Records not yet supported") //FIXME
    val fieldSyms = fields map { case (index, _, rhs) => (index, rhs(x)) }
    struct(AnonTag(manifest.asInstanceOf[RefinedManifest[T]]), fieldSyms:_*)
  }

  abstract class StructTag[T]
  case class ClassTag[T](name: String) extends StructTag[T]
  case class NestClassTag[C[_],T](elem: StructTag[T]) extends StructTag[C[T]]
  case class AnonTag[T](fields: RefinedManifest[T]) extends StructTag[T]
  case class MapTag[T] extends StructTag[T]

  abstract class AbstractStruct[T] extends Def[T] {
    val tag: StructTag[T]
    val elems: Map[String, Rep[Any]]
  }

  object Struct {
    def unapply[T](d: Def[T]) = unapplyStruct(d)
  }

  def unapplyStruct[T](d: Def[T]): Option[(StructTag[T], Map[String, Rep[Any]])] = d match {
    case s: AbstractStruct[T] => Some((s.tag, s.elems))
    case _ => None
  }

  case class SimpleStruct[T](tag: StructTag[T], elems: Map[String,Rep[Any]]) extends AbstractStruct[T]
  case class Field[T](struct: Rep[Any], index: String) extends Def[T]

  def struct[T:Manifest](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit pos: SourceContext): Rep[T] = struct(tag, Map(elems:_*))
  def struct[T:Manifest](tag: StructTag[T], elems: Map[String, Rep[Any]])(implicit pos: SourceContext): Rep[T] = SimpleStruct[T](tag, elems)

  def field[T:Manifest](struct: Rep[Any], index: String)(implicit pos: SourceContext): Rep[T] = Field[T](struct, index)

  // FIXME: need  syms override because Map is not a Product
  override def syms(x: Any): List[Sym[Any]] = x match {
    case z:Iterable[_] => z.toList.flatMap(syms)
    case _ => super.syms(x)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case z:Iterable[_] => z.toList.flatMap(symsFreq)
    case _ => super.symsFreq(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case SimpleStruct(tag, elems) => struct(tag, elems map { case (k,v) => (k, f(v)) })
    case Field(struct, key) => field(f(struct), key)
    case _ => super.mirror(e,f)
  }

  def structName(m: Manifest[_]): String = m match {
    case rm: RefinedManifest[_] => rm.erasure.getSimpleName + rm.fields.map(f => structName(f._2)).mkString("")
    case _ if (m <:< manifest[AnyVal]) => m.toString
    case _ => m.erasure.getSimpleName + m.typeArguments.map(a => structName(a)).mkString("")
  }

  def structName(tag: StructTag[_]): String = tag match {
    case AnonTag(m) => structName(m)
    case ClassTag(name) => name
    case NestClassTag(elem) => structName(elem)
    case _ => throw new RuntimeException("don't know how to generate struct for " + tag.toString)
  }

  override def object_toString(x: Exp[Any])(implicit pos: SourceContext): Exp[String] = x match {
    case Def(Struct(tag, elems)) => //tag(elem1, elem2, ...)
      val e = elems.map(e=>string_plus(unit(e._1 + " = "), object_toString(e._2))).reduceLeft((l,r)=>string_plus(string_plus(l,unit(", ")),r))
      string_plus(unit(structName(tag)+"("),string_plus(e,unit(")")))
    case _ => super.object_toString(x)
  }
}

trait StructExpOpt extends StructExp {

  override def field[T:Manifest](struct: Rep[Any], index: String)(implicit pos: SourceContext): Rep[T] = struct match {
    case Def(Struct(tag, elems)) => elems(index).asInstanceOf[Rep[T]]
    case _ => super.field[T](struct, index)
  }

}

trait StructExpOptCommon extends StructExpOpt with VariablesExp with IfThenElseExp {

  override def var_new[T:Manifest](init: Exp[T])(implicit pos: SourceContext): Var[T] = init match {
    case Def(Struct(tag, elems)) =>
      //val r = Variable(struct(tag, elems.mapValues(e=>var_new(e).e))) // DON'T use mapValues!! <--lazy
      Variable(struct[Variable[T]](NestClassTag[Variable,T](tag), elems.map(p=>(p._1,var_new(p._2)(p._2.tp,pos).e))))
    case _ =>
      super.var_new(init)
  }

  override def var_assign[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = (lhs,rhs) match {
    case (Variable(Def(Struct(NestClassTag(tagL),elemsL: Map[String,Exp[Variable[Any]]]))), Def(Struct(tagR, elemsR))) =>
      assert(tagL == tagR)
      assert(elemsL.keySet == elemsR.keySet)
      for (k <- elemsL.keySet)
        var_assign(Variable(elemsL(k)), elemsR(k))(elemsR(k).tp, pos)
      Const(())
    case _ => super.var_assign(lhs, rhs)
  }

  override def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext): Exp[T] = v match {
    case Variable(Def(Struct(NestClassTag(tag), elems: Map[String,Exp[Variable[Any]]]))) =>
      def unwrap[A](m:Manifest[Variable[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ => printerr("warning: expect type Variable[A] but got "+m); mtype(manifest[Any])
      }
      struct[T](tag, elems.map(p=>(p._1,readVar(Variable(p._2))(unwrap(p._2.tp), pos))))
    case _ => super.readVar(v)
  }

  override def ifThenElse[T:Manifest](cond: Rep[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = (a,b) match {
    case (Block(Def(Struct(tagA,elemsA))), Block(Def(Struct(tagB, elemsB)))) =>
      assert(tagA == tagB)
      assert(elemsA.keySet == elemsB.keySet)
      val elemsNew = for (k <- elemsA.keySet) yield (k -> ifThenElse(cond, Block(elemsA(k)), Block(elemsB(k)))(elemsB(k).tp, pos))
      struct[T](tagA, elemsNew.toMap)
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
      assert(tagA == tagB)
      assert(elemsA.keySet == elemsB.keySet)
      // create stm that computes all values at once
      // return struct of syms
      val combinedResult = super.ifThenElse(cond,u,v)

      val elemsNew = for (k <- elemsA.keySet) yield (k -> phi(cond,u,elemsA(k),v,elemsB(k))(combinedResult))
      struct[T](tagA, elemsNew.toMap)

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

  def registerStruct[T](tag: StructTag[T], elems: Map[String, Rep[Any]]) {
    encounteredStructs += tag -> elems.map(e => (e._1, e._2.tp))
  }
  val encounteredStructs = new mutable.HashMap[StructTag[_], Map[String, Manifest[_]]]
}

trait ScalaGenStruct extends ScalaGenBase with BaseGenStruct {
  val IR: StructExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      registerStruct(tag, elems)
      //emitValDef(sym, "Map(" + elems.map(e => "\"" + e._1 + "\"->" + quote(e._2)).mkString(",") + ") //" + tag)
      emitValDef(sym, "new " + structName(tag) + "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
      printlog("WARNING: emitting " + structName(tag) + " struct " + quote(sym))
    case f@Field(struct, index) =>
      //emitValDef(sym, quote(struct) + "(\"" + index + "\").asInstanceOf[" + remap(sym.Type) + "]")
      emitValDef(sym, quote(struct) + "." + index)
      printlog("WARNING: emitting field access: " + quote(struct) + "." + index)
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = m match {
    case s if s <:< manifest[Record] => structName(m)
    case _ => super.remap(m)
  }

  override def emitDataStructures(path: String) {
    val stream = new PrintWriter(path + "Structs.scala")
    stream.println("package generated.scala")
    for ((tag, elems) <- encounteredStructs) {
      stream.println()
      stream.print("case class " + structName(tag) + "(")
      stream.println(elems.map(e => e._1 + ": " + remap(e._2)).mkString(", ") + ")")
    }
    stream.close()
    super.emitDataStructures(path)
  }

}

trait CudaGenStruct extends CudaGenBase with BaseGenStruct
trait OpenCLGenStruct extends OpenCLGenBase with BaseGenStruct

trait CudaGenFatStruct extends CudaGenStruct with BaseGenFatStruct
trait OpenCLGenFatStruct extends OpenCLGenStruct with BaseGenFatStruct
