package scala.lms
package common

import java.io.PrintWriter
import scala.lms.internal._
import scala.collection.mutable.{HashMap,Set}
import scala.reflect.SourceContext

trait HashMultiMapOps extends Base with Variables {
  object HashMultiMap {
    def apply[K:Manifest,V:Manifest](size: Long = 0, specializedKey: String = "", specializedValue:String = "")(implicit pos: SourceContext) = hashmultimap_new[K,V](size, specializedKey, specializedValue)
  }

  implicit def HashMultiMapToRepHashMapOps[K:Manifest,V:Manifest](m: HashMap[K,Set[V]]) = new hashMultiMapOpsCls(unit(m))
  implicit def repHashMultiMapToHashMapOps[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]]) = new hashMultiMapOpsCls(m)
  implicit def varrepHashMultiMapToHashMapOps[K:Manifest,V:Manifest](m: Var[HashMap[K,Set[V]]]) = new hashMultiMapOpsCls(readVar(m))

  class hashMultiMapOpsCls[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]]) {
    def apply(k: Rep[K])(implicit pos: SourceContext) = hashmultimap_apply(m, k)
    def update(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = hashmultimap_update(m,k,v)
    def contains(k: Rep[K])(implicit pos: SourceContext) = hashmultimap_contains(m, k)
    def foreach(block: Rep[(K,Set[V])] => Rep[Unit])(implicit pos: SourceContext) = hashmultimap_foreach(m, block)
    def mkString(delimiter: Rep[String]) = hashmultimap_mkString(m, delimiter)
    def getOrElseEmpty(k: Rep[K])(implicit pos: SourceContext) = hashmultimap_getorelseempty(m,k)
    def remove(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = hashmultimap_remove(m,k,v)
  }

  def hashmultimap_new[K:Manifest,V:Manifest](size: Long = 0, specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) : Rep[HashMap[K,Set[V]]]
  def hashmultimap_apply[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]], k: Rep[K])(implicit pos: SourceContext): Rep[Set[V]]
  def hashmultimap_update[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmultimap_contains[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]], i: Rep[K])(implicit pos: SourceContext): Rep[Boolean]
  def hashmultimap_foreach[K:Manifest,V:Manifest](x: Rep[HashMap[K,Set[V]]], block: Rep[(K,Set[V])] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def hashmultimap_mkString[K: Manifest, V: Manifest](m: Rep[HashMap[K,Set[V]]], v: Rep[String])(implicit pos: SourceContext): Rep[String]
  def hashmultimap_getorelseempty[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]], k: Rep[K])(implicit pos: SourceContext): Rep[Set[V]]
  def hashmultimap_remove[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
}

trait HashMultiMapOpsExp extends HashMultiMapOps with EffectExp {
  abstract class HashMultiMapDef[K:Manifest,V:Manifest,R:Manifest] extends Def[R] {
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class HashMultiMapNew[K:Manifest,V:Manifest](size: Long = 0, specializedKey: String = "", specializedValue: String ="") extends HashMultiMapDef[K,Set[V],HashMap[K,Set[V]]] 
  case class HashMultiMapApply[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], k: Exp[K]) extends HashMultiMapDef[K,V,Set[V]]
  case class HashMultiMapUpdate[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], k: Exp[K], v: Exp[V]) extends HashMultiMapDef[K,V,Unit]
  case class HashMultiMapContains[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], i: Exp[K]) extends HashMultiMapDef[K,V,Boolean]
  case class HashMultiMapForeach[K:Manifest, V:Manifest](a: Exp[HashMap[K,Set[V]]], x: Sym[(K,Set[V])], block: Block[Unit]) extends Def[Unit]
  case class HashMultiMapMkString[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], v:Rep[String]) extends HashMultiMapDef[K,V,String]
  case class HashMultiMapGetOrElseEmpty[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], k: Exp[K]) extends HashMultiMapDef[K,V,Set[V]]
  case class HashMultiMapRemove[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], k: Exp[K], v: Exp[V]) extends HashMultiMapDef[K,V,Unit]

  def hashmultimap_new[K:Manifest,V:Manifest](size: Long = 0, specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) = reflectEffect(HashMultiMapNew[K,V](size, specializedKey, specializedValue))
  def hashmultimap_apply[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], k: Exp[K])(implicit pos: SourceContext) = reflectEffect(HashMultiMapApply[K,V](m,k))
  def hashmultimap_update[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectEffect(HashMultiMapUpdate[K,V](m,k,v))
  def hashmultimap_contains[K:Manifest,V:Manifest](m: Exp[HashMap[K,Set[V]]], i: Exp[K])(implicit pos: SourceContext) = HashMultiMapContains(m, i)
  def hashmultimap_foreach[K:Manifest,V:Manifest](x: Rep[HashMap[K,Set[V]]], block: Rep[(K,Set[V])] => Rep[Unit])(implicit pos: SourceContext) = {
    val k = fresh[(K,Set[V])]
    val b = reifyEffects(block(k))
	reflectEffect(HashMultiMapForeach(x, k, b), summarizeEffects(b).star)
  }
  def hashmultimap_mkString[K: Manifest, V: Manifest](m: Rep[HashMap[K,Set[V]]], v: Rep[String])(implicit pos: SourceContext) = reflectEffect(HashMultiMapMkString(m, v))
  def hashmultimap_getorelseempty[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]], k: Rep[K])(implicit pos: SourceContext) = reflectEffect(HashMultiMapGetOrElseEmpty(m,k))
  def hashmultimap_remove[K:Manifest,V:Manifest](m: Rep[HashMap[K,Set[V]]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = reflectEffect(HashMultiMapRemove(m,k,v))

  override def syms(e: Any): List[Sym[Any]] = e match {
    case HashMultiMapForeach(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case HashMultiMapForeach(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case HashMultiMapForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }


}

trait ScalaGenHashMultiMap extends GenericNestedCodegen with ScalaGenEffect {
  val IR: HashMultiMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMultiMapNew(size, spkey, spvalue) => {
        val key = if (spkey != "") spkey else remap(m.mK)
        val value = if (spvalue != "") spvalue else remap(m.mV)
        emitValDef(sym, "new collection.mutable.HashMap[" + key + ", " + value + "]() with scala.collection.mutable.MultiMap[" + key + "," + value.replace("scala.collection.mutable.Set[","").replace("]","") + "]")
    }
    case HashMultiMapApply(m,k) => emitValDef(sym, quote(m) + "(" + quote(k) + ")")
    case HashMultiMapGetOrElseEmpty(m,k) => emitValDef(sym, quote(m) + ".getOrElse(" + quote(k) + ", scala.collection.mutable.Set.empty)")
    case HashMultiMapUpdate(m,k,v)  => emitValDef(sym, quote(m) + ".addBinding(" + quote(k) + "," + quote(v) + ")")
    case HashMultiMapContains(m,i) => emitValDef(sym, quote(m) + ".contains(" + quote(i) + ")")
    case HashMultiMapForeach(m,k,v) => emitValDef(sym, quote(m) + ".foreach(" + quote(k) + "=>{")
		emitBlock(v)
		emitBlockResult(v)
		stream.println("})")
    case HashMultiMapMkString(m,k) => emitValDef(sym, quote(m) + ".mkString(" + quote(k) + ")")
    case HashMultiMapRemove(m,k,v) => emitValDef(sym, quote(m) + "(" + quote(k) + ").remove(" + quote(v) +")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenHashMultiMapOpt extends GenericNestedCodegen with ScalaGenEffect {
  val IR: HashMultiMapOpsExp
  import IR._

  def emitKeyModulo[K: Manifest, V: Manifest](m: Rep[HashMap[K, Set[V]]], k: Rep[K]) = {
    quote(k) + "%" + quote(m) + ".length"
  } 

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMultiMapNew(size, spkey, spvalue) => {
        // Sanity check
        //if (m.mV != manifest[Set[scala.lms.common.DynamicRecord]])
          //  throw new RuntimeException("ScalaGenHashMultiMapOpt can only be used with sets of DynamicRecords (you provided " + remap(m.mV))
        val value = if (spvalue != "") spvalue else remap(m.mV)
        emitValDef(sym, "new Array[" + value + "](" + size + ")")
    }
    case HashMultiMapApply(m,k) => {
        stream.println("val " + quote(sym) + " = " + quote(m) + "(" + emitKeyModulo(m,k) + ")")
    }
    case HashMultiMapUpdate(m,k,v) => {
        stream.println("\nval x" + sym.toString.replace("Sym(","").replace(")","") + " = {")
        stream.println("\tval __elem = " + quote(m) + "(" + emitKeyModulo(m,k) + ")")
        stream.println("\tif (__elem == null) " + quote(m) + "(" + emitKeyModulo(m,k) + ") = " + quote(v))
        stream.println("\telse {")
        stream.println("\t\t" + quote(v) + ".next = __elem")
        stream.println("\t\t" + quote(m) + "(" + emitKeyModulo(m,k) + ") = " + quote(v))
        stream.println("\t}")
        stream.println("}")
    }
    case HashMultiMapContains(m,k) => {
        stream.println("val " + quote(sym) + " = " + quote(m) + "(" + emitKeyModulo(m,k) + ") != null")
    }
    case HashMultiMapGetOrElseEmpty(m,k) => 
        // Note: here you get back a null if you have no elements ("the else") but we handle 
        // this case in the for-each function in the dynamic record (which should be used to 
        // iterate over the values in a way similar to the normal multimap)
        stream.println("val " + quote(sym) + " = " + quote(m) + "(" + emitKeyModulo(m,k) + ")")
    case HashMultiMapMkString(m,k) => emitValDef(sym, quote(m) + ".mkString(" + emitKeyModulo(m,k) + ")")
    case HashMultiMapRemove(m,k,v) => {
        stream.println("\tvar __elem = " + quote(m) + "(" + emitKeyModulo(m,k) + ")")
        stream.println("\tvar __prevelem = __elem; __prevelem = null;")
        stream.println("\twhile ((__elem != null) && (__elem.equals(" + quote(v) + ") == false)) {")
        stream.println("\t\t__prevelem = __elem")
        stream.println("\t\t__elem = __elem.next")
        stream.println("}")
        stream.println("\tif (__elem == null) throw new RuntimeException(\"Element to be removed not found\")")
        stream.println("\telse if (__prevelem != null) __prevelem.next = __elem.next")
        stream.println("\telse " + quote(m) + "(" + emitKeyModulo(m,k) + ") = __elem.next") 
    }
    case _ => super.emitNode(sym, rhs)
  }
}
