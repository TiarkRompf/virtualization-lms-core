package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.collection.mutable.HashMap
import scala.reflect.SourceContext

trait HashMapOps extends Base {
  object HashMap {
    def apply[K:Manifest,V:Manifest]()(implicit pos: SourceContext) = hashmap_new[K,V]()
  }

  implicit def repHashMapToHashMapOps[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) = new hashmapOpsCls(m)

  class hashmapOpsCls[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) {
    def apply(k: Rep[K])(implicit pos: SourceContext) = hashmap_apply(m, k)
    def update(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = hashmap_update(m,k,v)
    def contains(k: Rep[K])(implicit pos: SourceContext) = hashmap_contains(m, k)
    def size(implicit pos: SourceContext) = hashmap_size(m)
    def values(implicit pos: SourceContext) = hashmap_values(m)
    def clear()(implicit pos: SourceContext) = hashmap_clear(m)
  }

  def hashmap_new[K:Manifest,V:Manifest]()(implicit pos: SourceContext) : Rep[HashMap[K,V]]
  def hashmap_apply[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[V]
  def hashmap_update[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_contains[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], i: Rep[K])(implicit pos: SourceContext): Rep[Boolean]
  def hashmap_size[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int]
  def hashmap_values[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Iterable[V]]
  def hashmap_clear[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Unit]
}

trait HashMapOpsExp extends HashMapOps with EffectExp {
  case class HashMapNew[K:Manifest,V:Manifest]() extends Def[HashMap[K,V]] {
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class HashMapApply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K]) extends Def[V]
  case class HashMapUpdate[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V]) extends Def[Unit]
  case class HashMapContains[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], i: Exp[K]) extends Def[Boolean]
  case class HashMapSize[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends Def[Int]
  case class HashMapValues[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends Def[Iterable[V]]
  case class HashMapClear[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends Def[Unit]

  def hashmap_new[K:Manifest,V:Manifest]()(implicit pos: SourceContext) = reflectMutable(HashMapNew[K,V]())
  def hashmap_apply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K])(implicit pos: SourceContext) = HashMapApply(m,k)
  def hashmap_update[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectMutable(HashMapUpdate(m,k,v))
  def hashmap_contains[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], i: Exp[K])(implicit pos: SourceContext) = HashMapContains(m, i)
  def hashmap_size[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = HashMapSize(m)
  def hashmap_values[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = HashMapValues(m)
  def hashmap_clear[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = reflectWrite(m)(HashMapClear(m))
}

trait BaseGenHashMapOps extends GenericNestedCodegen {
  val IR: HashMapOpsExp
  import IR._

}

trait ScalaGenHashMapOps extends BaseGenHashMapOps with ScalaGenEffect {
  val IR: HashMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMapNew() => emitValDef(sym, "collection.mutable.HashMap[" + remap(m.mK) + "," + remap(m.mV) + "]()")
    case HashMapApply(m,k) => emitValDef(sym, quote(m) + "(" + quote(k) + ")")
    case HashMapUpdate(m,k,v)  => emitValDef(sym, quote(m) + "(" + quote(k) + ") = " + quote(v))
    case HashMapContains(m,i) => emitValDef(sym, quote(m) + ".contains(" + quote(i) + ")")
    case HashMapSize(m) => emitValDef(sym, quote(m) + ".size")
    case HashMapValues(m) => emitValDef(sym, quote(m) + ".values")
    case HashMapClear(m) => emitValDef(sym, quote(m) + ".clear()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenHashMapOps extends BaseGenHashMapOps with CLikeCodegen {
  val IR: HashMapOpsExp
  import IR._

//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
}

trait CudaGenHashMapOps extends CudaGenEffect with CLikeGenHashMapOps
trait OpenCLGenHashMapOps extends OpenCLGenEffect with CLikeGenHashMapOps
trait CGenHashMapOps extends CGenEffect with CLikeGenHashMapOps
