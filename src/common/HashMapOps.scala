package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.collection.mutable.{HashMap,Set}
import scala.reflect.SourceContext

trait HashMapOps extends Base with Variables {
  object HashMap {
    def apply[K:Manifest,V:Manifest](specializedKey: String = null, specializedValue:String = null)(implicit pos: SourceContext) = hashmap_new[K,V](specializedKey, specializedValue)
  }

  implicit def HashMapToRepHashMapOps[K:Manifest,V:Manifest](m: HashMap[K,V]) = new hashmapOpsCls[K,V](unit(m))
  implicit def repHashMapToHashMapOps[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) = new hashmapOpsCls[K,V](m)
  implicit def varrepHashMapToHashMapOps[K:Manifest,V:Manifest](m: Var[HashMap[K,V]]) = new hashmapOpsCls[K,V](readVar(m))

  class hashmapOpsCls[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) {
    def apply(k: Rep[K])(implicit pos: SourceContext) = hashmap_apply(m, k)
    def update(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = hashmap_update(m,k,v)
    def contains(k: Rep[K])(implicit pos: SourceContext) = hashmap_contains(m, k)
    def size(implicit pos: SourceContext) = hashmap_size(m)
    def values(implicit pos: SourceContext) = hashmap_values(m)
    def clear()(implicit pos: SourceContext) = hashmap_clear(m)
    def keySet(implicit pos: SourceContext) = hashmap_keyset(m)
    def keys(implicit pos: SourceContext) = hashmap_keys(m)
    def removeHead(implicit pos:SourceContext) = hashmap_removehead(m)
    def getOrElseUpdate(k: Rep[K], v: => Rep[V])(implicit pos: SourceContext) = hashmap_getorelseupdate[K,V](m,k,v)
    def -=(v: Rep[K])(implicit pos:SourceContext) = hashmap_-=(m,v)
    def mkString(delimiter: Rep[String]) = hashmap_mkString(m, delimiter)
  }

  def hashmap_new[K:Manifest,V:Manifest](specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) : Rep[HashMap[K,V]]
  def hashmap_apply[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[V]
  def hashmap_update[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_unsafe_update[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_contains[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], i: Rep[K])(implicit pos: SourceContext): Rep[Boolean]
  def hashmap_size[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int]
  def hashmap_values[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Iterable[V]]
  def hashmap_clear[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_keyset[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Set[K]]
  def hashmap_keys[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Iterable[K]]
  def hashmap_removehead[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[(K,V)]
  def hashmap_-=[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]], v: Rep[K])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_getorelseupdate[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: => Rep[V])(implicit pos: SourceContext): Rep[V]
  def hashmap_mkString[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]], v: Rep[String])(implicit pos: SourceContext): Rep[String]
}

trait HashMapOpsExp extends HashMapOps with EffectExp {
  abstract class HashMapDef[K:Manifest,V:Manifest,R:Manifest] extends Def[R] {
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class HashMapNew[K:Manifest,V:Manifest](specializedKey: String = "", specializedValue: String ="") extends HashMapDef[K,V,HashMap[K,V]] 
  case class HashMapApply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K]) extends HashMapDef[K,V,V]
  case class HashMapUpdate[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V]) extends HashMapDef[K,V,Unit]
  case class HashMapContains[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], i: Exp[K]) extends HashMapDef[K,V,Boolean]
  case class HashMapSize[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Int]
  case class HashMapValues[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Iterable[V]]
  case class HashMapClear[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Unit]
  case class HashMapKeySet[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Set[K]]
  case class HashMapKeys[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Iterable[K]]
  case class HashMapRemoveHead[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], s: Sym[V]) extends HashMapDef[K,V,(K,V)]
  case class HashMapRemove[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], v:Rep[K]) extends HashMapDef[K,V,Unit]
  case class HashMapGetOrElseUpdate[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Block[V]) extends HashMapDef[K,V,V]
  case class HashMapMkString[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], v:Rep[String]) extends HashMapDef[K,V,String]

  def hashmap_new[K:Manifest,V:Manifest](specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) = reflectEffect(HashMapNew[K,V](specializedKey, specializedValue))
  def hashmap_apply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K])(implicit pos: SourceContext) = HashMapApply(m,k)
  def hashmap_update[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectEffect(HashMapUpdate(m,k,v))
  def hashmap_unsafe_update[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectEffect(HashMapUpdate(m,k,v))
  def hashmap_contains[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], i: Exp[K])(implicit pos: SourceContext) = HashMapContains(m, i)
  def hashmap_size[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = reflectEffect(HashMapSize(m))
  def hashmap_values[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = HashMapValues(m)
  def hashmap_clear[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = reflectWrite(m)(HashMapClear(m))
  def hashmap_keyset[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = HashMapKeySet(m)
  def hashmap_keys[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = HashMapKeys(m)
  def hashmap_removehead[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = {
    val s = fresh[V]
    reflectEffect(HashMapRemoveHead(m,s))
  }
  def hashmap_-=[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]], v: Rep[K])(implicit pos: SourceContext) = reflectEffect(HashMapRemove(m,v))
  def hashmap_getorelseupdate[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: => Exp[V])(implicit pos: SourceContext) = {
    val b = reifyEffects(v)
    reflectEffect(HashMapGetOrElseUpdate(m,k,b))//, summarizeEffects(b).star)
  }
  def hashmap_mkString[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]], v: Rep[String])(implicit pos: SourceContext) = reflectEffect(HashMapMkString(m, v))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case e@HashMapApply(m,k) => hashmap_apply(f(m),f(k))(e.mK,e.mV,pos)
      case e@HashMapKeys(m) => hashmap_keys(f(m))(e.mK,e.mV,pos)
      case e@HashMapValues(m) => hashmap_values(f(m))(e.mK,e.mV,pos)
      case e@HashMapContains(m,k) => hashmap_contains(f(m),f(k))(e.mK,e.mV,pos)
      case Reflect(e@HashMapApply(m,k), u, es) => reflectMirrored(Reflect(HashMapApply(f(m),f(k))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))            
      case Reflect(e@HashMapKeys(m), u, es) => reflectMirrored(Reflect(HashMapKeys(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapValues(m), u, es) => reflectMirrored(Reflect(HashMapValues(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapContains(m,k), u, es) => reflectMirrored(Reflect(HashMapContains(f(m),f(k))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@HashMapNew(spkey,spvalue), u, es) => reflectMirrored(Reflect(HashMapNew(spkey,spvalue)(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapUpdate(m,k,v), u, es) => reflectMirrored(Reflect(HashMapUpdate(f(m),f(k),f(v))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))   
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case HashMapGetOrElseUpdate(m, k, v) => syms(m):::syms(v)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case HashMapGetOrElseUpdate(m, k, v) => effectSyms(m) ::: effectSyms(v)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case HashMapGetOrElseUpdate(m, k, v) => freqNormal(m) ::: freqHot(v)
    case _ => super.symsFreq(e)
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
    case m@HashMapNew(spkey, spvalue) => {
        val key = if (spkey != "") spkey else remap(m.mK)
        val value = if (spvalue != "") spvalue else remap(m.mV)
        emitValDef(sym, "new scala.collection.mutable.HashMap[" + key + "," + value + "]() {")
        if (spkey.contains("Array[Byte]")) {
            stream.println("override def elemHashCode(key: Array[Byte]) = key(0).##")
            stream.println("override def elemEquals(key1: Array[Byte], key2: Array[Byte]) = key1.corresponds(key2){_ == _}")
        }
        stream.println("}")
    }
    case HashMapApply(m,k) => emitValDef(sym, quote(m) + "(" + quote(k) + ")")
    case HashMapUpdate(m,k,v)  => emitValDef(sym, quote(m) + "(" + quote(k) + ") = " + quote(v))
    case HashMapContains(m,i) => emitValDef(sym, quote(m) + ".contains(" + quote(i) + ")")
    case HashMapSize(m) => emitValDef(sym, quote(m) + ".size")
    case HashMapValues(m) => emitValDef(sym, quote(m) + ".values")
    case HashMapClear(m) => emitValDef(sym, quote(m) + ".clear()")
    case HashMapKeySet(m) => emitValDef(sym, quote(m) + ".keySet")
    case HashMapKeys(m) => emitValDef(sym, quote(m) + ".keys")
    case HashMapRemoveHead(m,s) => {
        emitValDef(s, quote(m) + ".head")
        stream.println(quote(m) + " -= " + quote(s) + "._1")
        emitValDef(sym, quote(s))
    }
    case HashMapRemove(m,v) => emitValDef(sym, quote(m) + "-=" + quote(v))
    case HashMapGetOrElseUpdate(m,k,v)  => {
         stream.print("val " + quote(sym) + " = ")
         stream.println(quote(m) + ".getOrElseUpdate(" + quote(k) + ", {")
         emitBlock(v)
         stream.println(quote(getBlockResult(v)))
         stream.println("})")
    }
    case HashMapMkString(m,k) => emitValDef(sym, quote(m) + ".mkString(" + quote(k) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenHashMapOps extends BaseGenHashMapOps with CLikeCodegen {
  val IR: HashMapOpsExp
  import IR._
}

trait CudaGenHashMapOps extends CudaGenEffect with CLikeGenHashMapOps
trait OpenCLGenHashMapOps extends OpenCLGenEffect with CLikeGenHashMapOps
trait CGenHashMapOps extends CGenEffect with CLikeGenHashMapOps
