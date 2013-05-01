package scala.lms
package ops

import internal._

import java.io.PrintWriter
import scala.collection.mutable.Set
import java.util.HashMap
import scala.reflect.SourceContext

trait LiftHashMapType { this: TypeRepBase =>
  implicit def liftHashMap[K, V](implicit t: TypeRep[K],v: TypeRep[V]): TypeRep[HashMap[K, V]] = {
    implicit val (mf, mf1) = (t.mf, v.mf)
    typeRep[HashMap[K,V]]
  }
}

trait HashMapOps extends Base with LiftSetType with LiftIterableType {
  object HashMap {
    def apply[K:TypeRep,V:TypeRep]()(implicit pos: SourceContext) = hashmap_new[K,V]()
  }

  implicit def repHashMapToHashMapOps[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]]) = new hashmapOpsCls(m)

  class hashmapOpsCls[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]]) {
    def apply(k: Rep[K])(implicit pos: SourceContext) = hashmap_apply(m, k)
    def update(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = hashmap_update(m,k,v)
    def contains(k: Rep[K])(implicit pos: SourceContext) = hashmap_contains(m, k)
    def size(implicit pos: SourceContext) = hashmap_size(m)
    def values(implicit pos: SourceContext) = hashmap_values(m)
    def clear()(implicit pos: SourceContext) = hashmap_clear(m)
    def keySet(implicit pos: SourceContext) = hashmap_keyset(m)
    def keys(implicit pos: SourceContext) = hashmap_keys(m)
  }

  def hashmap_new[K:TypeRep,V:TypeRep]()(implicit pos: SourceContext) : Rep[HashMap[K,V]]
  def hashmap_apply[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[V]
  def hashmap_update[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_unsafe_update[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_contains[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]], i: Rep[K])(implicit pos: SourceContext): Rep[Boolean]
  def hashmap_size[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int]
  def hashmap_values[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Iterable[V]]
  def hashmap_clear[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_keyset[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Set[K]]
  def hashmap_keys[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Iterable[K]]
}

trait HashMapOpsExp extends HashMapOps with EffectExp with LiftHashMapType {
  abstract class HashMapDef[K:TypeRep,V:TypeRep,R:TypeRep] extends Def[R] {
    val mK = typeRep[K]
    val mV = typeRep[V]
  }
  case class HashMapNew[K:TypeRep,V:TypeRep]() extends HashMapDef[K,V,HashMap[K,V]]
  case class HashMapApply[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]], k: Exp[K]) extends HashMapDef[K,V,V]
  case class HashMapUpdate[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V]) extends HashMapDef[K,V,Unit]
  case class HashMapContains[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]], i: Exp[K]) extends HashMapDef[K,V,Boolean]
  case class HashMapSize[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Int]
  case class HashMapValues[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Iterable[V]]
  case class HashMapClear[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Unit]
  case class HashMapKeySet[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Set[K]]
  case class HashMapKeys[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Iterable[K]]

  def hashmap_new[K:TypeRep,V:TypeRep]()(implicit pos: SourceContext) = reflectMutable(HashMapNew[K,V]())
  def hashmap_apply[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]], k: Exp[K])(implicit pos: SourceContext) = HashMapApply(m,k)
  def hashmap_update[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectWrite(m)(HashMapUpdate(m,k,v))
  def hashmap_unsafe_update[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectEffect(HashMapUpdate(m,k,v))
  def hashmap_contains[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]], i: Exp[K])(implicit pos: SourceContext) = HashMapContains(m, i)
  def hashmap_size[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = HashMapSize(m)
  def hashmap_values[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = HashMapValues(m)
  def hashmap_clear[K:TypeRep,V:TypeRep](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = reflectWrite(m)(HashMapClear(m))
  def hashmap_keyset[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = HashMapKeySet(m)
  def hashmap_keys[K:TypeRep,V:TypeRep](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = HashMapKeys(m)

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case e@HashMapApply(m,k) => hashmap_apply(f(m),f(k))(e.mK,e.mV,pos)
      case e@HashMapKeys(m) => hashmap_keys(f(m))(e.mK,e.mV,pos)
      case e@HashMapValues(m) => hashmap_values(f(m))(e.mK,e.mV,pos)
      case e@HashMapContains(m,k) => hashmap_contains(f(m),f(k))(e.mK,e.mV,pos)
      case e@HashMapSize(m) => hashmap_size(f(m))(e.mK,e.mV,pos)
      case Reflect(e@HashMapApply(m,k), u, es) => reflectMirrored(Reflect(HashMapApply(f(m),f(k))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case Reflect(e@HashMapKeys(m), u, es) => reflectMirrored(Reflect(HashMapKeys(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case Reflect(e@HashMapValues(m), u, es) => reflectMirrored(Reflect(HashMapValues(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case Reflect(e@HashMapContains(m,k), u, es) => reflectMirrored(Reflect(HashMapContains(f(m),f(k))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case Reflect(e@HashMapSize(m), u, es) => reflectMirrored(Reflect(HashMapSize(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case Reflect(e@HashMapNew(), u, es) => reflectMirrored(Reflect(HashMapNew()(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case Reflect(e@HashMapUpdate(m,k,v), u, es) => reflectMirrored(Reflect(HashMapUpdate(f(m),f(k),f(v))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(typeRep[A]))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }

}

trait BaseGenHashMapOps extends GenericNestedCodegen {
  val IR: HashMapOpsExp
  import IR._

}

trait ScalaGenHashMapOps extends BaseGenHashMapOps with ScalaGenEffect {
  val IR: HashMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMapNew() => emitValDef(sym, "new java.util.HashMap[" + remap(m.mK) + "," + remap(m.mV) + "]()")
    case HashMapApply(m,k) => emitValDef(sym, quote(m) + ".get(" + quote(k) + ")")
    case HashMapUpdate(m,k,v)  => emitValDef(sym, quote(m) + ".put(" + quote(k) + ", " + quote(v) + ")")
    case HashMapContains(m,i) => emitValDef(sym, quote(m) + ".containsKey(" + quote(i) + ")")
    case HashMapSize(m) => emitValDef(sym, quote(m) + ".size")
    case HashMapValues(m) => emitValDef(sym, "scala.collection.JavaConverters.collectionAsScalaIterableConverter("+quote(m)+".values).asScala")
    case HashMapClear(m) => emitValDef(sym, quote(m) + ".clear()")
    case HashMapKeySet(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala")
    case HashMapKeys(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala.toIterable")
    case _ => super.emitNode(sym, rhs)
  }
}
