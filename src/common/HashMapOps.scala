package scala.lms
package common

import java.io.PrintWriter
import java.io.StringWriter
import scala.lms.internal._
import scala.collection.mutable.{HashMap,Set}
import scala.reflect.SourceContext

trait HashMapOps extends Base with Variables with TupleOps {
  object HashMap {
    def apply[K:Manifest,V:Manifest]()(implicit pos: SourceContext) = hashmap_new[K,V]()
    def apply[K:Manifest,V:Manifest](hash: Rep[K] => Rep[Int], equals: (Rep[K],Rep[K])=>Rep[Boolean])(implicit pos: SourceContext) = 
		hashmap_new[K,V](hash, equals)
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
    def map[B:Manifest](f: Rep[(K,V)] => Rep[B]) = hashmap_map(m,f)
    def foreach(block: Rep[(K,V)] => Rep[Unit])(implicit pos: SourceContext) = hashmap_foreach(m, block)
    def clear()(implicit pos: SourceContext) = hashmap_clear(m)
    def keySet(implicit pos: SourceContext) = hashmap_keyset(m)
    def keys(implicit pos: SourceContext) = hashmap_keys(m)
	def head(implicit pos: SourceContext) = hashmap_head(m)
    def removeHead(implicit pos:SourceContext) = hashmap_removehead(m)
    def getOrElseUpdate(k: Rep[K], v: => Rep[V])(implicit pos: SourceContext) = hashmap_getorelseupdate[K,V](m,k,v)
    def remove(v: Rep[K])(implicit pos:SourceContext) = hashmap_remove[K,V](m,v)
    def mkString(delimiter: Rep[String]) = hashmap_mkString(m, delimiter)
  }

  def hashmap_new[K:Manifest,V:Manifest]() : Rep[HashMap[K,V]]
  def hashmap_new[K:Manifest,V:Manifest](hash: Rep[K] => Rep[Int], equals: (Rep[K],Rep[K])=>Rep[Boolean]) : Rep[HashMap[K,V]]
  def hashmap_apply[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[V]
  def hashmap_update[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_unsafe_update[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_contains[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], i: Rep[K])(implicit pos: SourceContext): Rep[Boolean]
  def hashmap_size[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int]
  def hashmap_values[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Iterable[V]]
  def hashmap_map[K:Manifest,V:Manifest,B:Manifest](m: Rep[HashMap[K,V]], f: Rep[(K,V)]=>Rep[B]): Rep[HashMap[K,B]]
  def hashmap_foreach[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], block: Rep[(K,V)] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_clear[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Unit]
  def hashmap_keyset[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Set[K]]
  def hashmap_keys[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Iterable[K]]
  def hashmap_head[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[(K,V)]
  def hashmap_removehead[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[(K,V)]
  def hashmap_remove[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], v: Rep[K])(implicit pos: SourceContext): Rep[V]
  def hashmap_getorelseupdate[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: => Rep[V])(implicit pos: SourceContext): Rep[V]
  def hashmap_mkString[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], v: Rep[String])(implicit pos: SourceContext): Rep[String]
}

trait HashMapOpsExp extends HashMapOps with EffectExp with TupleOpsExp with FunctionsExp with ArrayOps with IfThenElseExp with EqualExp {
  abstract class HashMapDef[K:Manifest,V:Manifest,R:Manifest] extends Def[R] {
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class HashMapNew[K:Manifest,V:Manifest]() extends HashMapDef[K,V,HashMap[K,V]] 
  case class HashMapExtendedNew[K:Manifest,V:Manifest](k: Sym[K], hf:Block[Int], v1: Sym[K], v2: Sym[K], ef: Block[Boolean]) extends HashMapDef[K,V,HashMap[K,V]] 
  case class HashMapApply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K]) extends HashMapDef[K,V,V]
  case class HashMapUpdate[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V]) extends HashMapDef[K,V,Unit]
  case class HashMapContains[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], i: Exp[K]) extends HashMapDef[K,V,Boolean]
  case class HashMapSize[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Int]
  case class HashMapValues[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Iterable[V]]
  case class HashMapMap[K:Manifest,V:Manifest,B:Manifest](m: Exp[HashMap[K,V]], s: Sym[(K,V)], v:Block[B]) extends HashMapDef[K,V,HashMap[K,B]]
  case class HashMapForeach[K:Manifest, V:Manifest](a: Exp[HashMap[K,V]], x: Sym[(K,V)], block: Block[Unit]) extends Def[Unit] 
  case class HashMapClear[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Unit]
  case class HashMapKeySet[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Set[K]]
  case class HashMapKeys[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Iterable[K]]
  case class HashMapHead[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,(K,V)] 
  case class HashMapRemoveHead[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], s: Sym[V]) extends HashMapDef[K,V,(K,V)] 
  // We assume that the element to be removed exists for sure for simplicitly
  case class HashMapRemove[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], v:Rep[K]) extends HashMapDef[K,V,V] {
        val ccc = manifest[V]
  }
  case class HashMapGetOrElseUpdate[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], vl: Block[V]) extends HashMapDef[K,V,V] with IfThenElseExp
  case class HashMapMkString[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], v:Rep[String]) extends HashMapDef[K,V,String]

  def hashmap_new[K:Manifest,V:Manifest]() = reflectMutable(HashMapNew[K,V]())
  def hashmap_new[K:Manifest,V:Manifest](hash: Exp[K] => Exp[Int], equals: (Exp[K],Exp[K])=>Exp[Boolean]) = {
    val k = fresh[K]
    val v1 = fresh[K]
    val v2 = fresh[K]
    val hf = reifyEffects(hash(k))
    val ef = reifyEffects(equals(v1,v2))
	reflectMutable(HashMapExtendedNew[K,V](k,hf,v1,v2,ef))
  }
  def hashmap_apply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K])(implicit pos: SourceContext) = HashMapApply(m,k)
  def hashmap_update[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectWrite(m)(HashMapUpdate(m,k,v))
  def hashmap_unsafe_update[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Exp[V])(implicit pos: SourceContext) = reflectEffect(HashMapUpdate(m,k,v))
  def hashmap_contains[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], i: Exp[K])(implicit pos: SourceContext) = HashMapContains(m, i)
  def hashmap_size[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = reflectEffect(HashMapSize(m))
  def hashmap_values[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = HashMapValues(m)
  def hashmap_map[K:Manifest,V:Manifest,B:Manifest](m: Rep[HashMap[K,V]], f: Rep[(K,V)]=>Rep[B]) = {
    val a = fresh[Tuple2[K,V]]
    val b = reifyEffects(f(a))
    reflectEffect(HashMapMap(m, a, b))//, summarizeEffects(b).star)
  }
  def hashmap_foreach[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], block: Rep[(K,V)] => Rep[Unit])(implicit pos: SourceContext) = {
    val k = fresh[(K,V)]
    val b = reifyEffects(block(k))
	reflectEffect(HashMapForeach(x, k, b), summarizeEffects(b).star)
  }

  def hashmap_clear[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = reflectWrite(m)(HashMapClear(m))
  def hashmap_keyset[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = reflectEffect(HashMapKeySet(m))
  def hashmap_keys[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = HashMapKeys(m)
  def hashmap_head[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = {
    reflectEffect(HashMapHead(m))
  }
  def hashmap_removehead[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = {
    val s = fresh[V]
    reflectEffect(HashMapRemoveHead(m,s))
  }
  def hashmap_remove[K:Manifest, V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext) = reflectEffect(HashMapRemove[K,V](m,k))
  def hashmap_getorelseupdate[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Exp[K], v: => Exp[V])(implicit pos: SourceContext) = {
    val b = reifyEffects(v)
    reflectEffect(HashMapGetOrElseUpdate[K,V](m,k,b), summarizeEffects(b).star)
  }
  def hashmap_mkString[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], v: Rep[String])(implicit pos: SourceContext) = reflectEffect(HashMapMkString(m, v))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case e@HashMapApply(m,k) => hashmap_apply(f(m),f(k))(e.mK,e.mV,pos)
      case e@HashMapRemove(m,k) => hashmap_remove(f(m),f(k))(e.mK,e.mV,pos)
      case e@HashMapKeys(m) => hashmap_keys(f(m))(e.mK,e.mV,pos)
      case e@HashMapKeySet(m) => hashmap_keyset(f(m))(e.mK,e.mV,pos)
      case e@HashMapValues(m) => hashmap_values(f(m))(e.mK,e.mV,pos)
      case e@HashMapContains(m,k) => hashmap_contains(f(m),f(k))(e.mK,e.mV,pos)
      case Reflect(e@HashMapApply(m,k), u, es) => reflectMirrored(Reflect(HashMapApply(f(m),f(k))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))            
      case Reflect(e@HashMapKeys(m), u, es) => reflectMirrored(Reflect(HashMapKeys(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapKeySet(m), u, es) => reflectMirrored(Reflect(HashMapKeySet(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapValues(m), u, es) => reflectMirrored(Reflect(HashMapValues(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapSize(m), u, es) => reflectMirrored(Reflect(HashMapSize(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapRemove(m,el), u, es) => reflectMirrored(Reflect(HashMapRemove(f(m),f(el))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapContains(m,k), u, es) => reflectMirrored(Reflect(HashMapContains(f(m),f(k))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@HashMapNew(), u, es) => reflectMirrored(Reflect(HashMapNew()(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(e@HashMapUpdate(m,k,v), u, es) => reflectMirrored(Reflect(HashMapUpdate(f(m),f(k),f(v))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))   
//      case Reflect(HashMapExtendedNew(k,hf,v1,v2,ef), u, es) => reflectMirrored(Reflect(HashMapExtendedNew(k,f(hf),v1,v2,f(ef)), mapOver(f,u), f(es)))
//      case Reflect(HashMapGetOrElseUpdate(m,k,v), u, es) => reflectMirrored(Reflect(HashMapGetOrElseUpdate(f(m).asInstanceOf[Exp[scala.collection.mutable.HashMap[Any,A]]],f(k),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case HashMapMap(m, k, v) => syms(m):::syms(v)
    case HashMapForeach(m, k, v) => syms(m):::syms(v)
    case HashMapExtendedNew(k,hf,v1,v2,ef) => syms(k):::syms(hf):::syms(v1):::syms(v2):::syms(ef)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case HashMapGetOrElseUpdate(m, k, v) => effectSyms(k) ::: effectSyms(m) ::: effectSyms(v)
    case HashMapMap(m, k, v) => k :: effectSyms(v)
    case HashMapForeach(m, k, v) => k :: effectSyms(v)
    case HashMapUpdate(m,k,v) => effectSyms(k) ::: effectSyms(m) ::: effectSyms(v)
    case HashMapExtendedNew(k,hf,v1,v2,ef) => k :: v1 :: v2 :: effectSyms(hf) ::: effectSyms(ef)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case HashMapMap(m, k, v) => freqNormal(m) ::: freqHot(v)
    case HashMapForeach(m, k, v) => freqNormal(m) ::: freqHot(v)
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
    case m@HashMapNew() => emitValDef(sym, src"new scala.collection.mutable.HashMap[${m.mK},${m.mV}]()")
    case m@HashMapExtendedNew(k,hf,v1,v2,ef) => 
		emitValDef(sym, src"new scala.collection.mutable.HashMap[${m.mK},${m.mV}]() {")
		stream.println("override def elemHashCode(" + quote(k) + ": " + remap(m.mK) + ") = {")
		emitBlock(hf)
		emitBlockResult(hf)
		stream.println("}")
        stream.println("override def elemEquals(" + quote(v1) + ": " + remap(m.mK) + ", " + quote(v2) + ": " + remap(m.mK) + ") = {")
		emitBlock(ef)
		emitBlockResult(ef)
		stream.println("}")
		stream.println("}")
    case HashMapApply(m,k) => emitValDef(sym, src"$m($k)")
    case HashMapUpdate(m,k,v)  => emitValDef(sym, src"$m($k) = $v")
    case HashMapContains(m,i) => emitValDef(sym, src"$m.contains($i)")
    case HashMapSize(m) => emitValDef(sym, src"$m.size")
    case HashMapValues(m) => emitValDef(sym, src"$m.values")
    case HashMapClear(m) => emitValDef(sym, src"$m.clear()")
    case HashMapKeySet(m) => emitValDef(sym, src"collection.mutable.Set($m.keySet.toSeq:_*)")
    case HashMapKeys(m) => emitValDef(sym, src"$m.keys")
    case HashMapHead(m) => emitValDef(sym, src"$m.head")
    case HashMapRemoveHead(m,s) => {
        emitValDef(s, src"$m.head")
        gen"$m -= $s._1"
        emitValDef(sym, quote(s))
    }
    // We assume that the element to be removed exists for sure for simplicitly
    case HashMapRemove(m,k) => emitValDef(sym, src"$m.remove($k).get")
    case HashMapMap(m,k,v)  => {
		 emitValDef(sym, quote(m) + ".map(" + quote(k) + "=> {")
         emitBlock(v)
	     emitBlockResult(v)
         stream.println("})")
    }
    case HashMapForeach(m,k,v)  => {
		 emitValDef(sym, quote(m) + ".foreach(" + quote(k) + "=> {")
         emitBlock(v)
	     emitBlockResult(v)
         stream.println("})")
    }
    case HashMapGetOrElseUpdate(m,k,v)  => {
	 	emitValDef(sym, quote(m) + ".getOrElseUpdate(" + quote(k) + ",{")
        emitBlock(v)
	    emitBlockResult(v)
        stream.println("})")
    }
    case HashMapMkString(m,k) => emitValDef(sym, src"$m.mkString($k)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenHashMapOps extends BaseGenHashMapOps with CLikeCodegen {
  val IR: HashMapOpsExp
  import IR._

  override def lowerNode[A:Manifest](sym: Sym[A], rhs: Def[A]) = rhs match {
    case m@HashMapExtendedNew(k,hf,v1,v2,ef) => 
        LIRTraversal(hf)
        LIRTraversal(ef)
		sym.atPhase(LIRLowering) {	
            val lhf = LIRLowering(hf)
            val lef = LIRLowering(ef)
            val nvt = remapManifest(fresh(m.mV))
			val lkM = remapManifest(fresh(m.mK))(m.mK)
			k.tp = lkM
			v1.tp = lkM
			v2.tp = lkM
            reflectMutable(HashMapExtendedNew(k,lhf,v1,v2,lef)(m.mK,nvt)).asInstanceOf[Exp[A]]
		}
	case ma@HashMapApply(m,k) =>
		sym.atPhase(LIRLowering) {
			reflectEffect(HashMapApply(LIRLowering(m).asInstanceOf[Exp[scala.collection.mutable.HashMap[Any,A]]],LIRLowering(k))).asInstanceOf[Exp[A]]
		}
    case gu@HashMapGetOrElseUpdate(m,k,v) => 
        LIRTraversal(v)
        sym.atPhase(LIRLowering) {
            val vb = LIRLowering(v)
            val hm = LIRLowering(m)
            val nvt = remapManifest(fresh(vb.tp))
            val c = hashmap_apply(hm.asInstanceOf[Exp[scala.collection.mutable.HashMap[Any,A]]],LIRLowering(k))(gu.mK,nvt.asInstanceOf[Manifest[A]],implicitly[SourceContext])
			val cc = reflectEffect(IfThenElse ((c == unit(null)), vb, Block(c)))
            __ifThenElse ((cc != c), hashmap_update(hm.asInstanceOf[Exp[scala.collection.mutable.HashMap[Any,A]]],LIRLowering(k),cc), ())
            cc.asInstanceOf[Exp[A]]
        }
    case ks@HashMapKeySet(m) => sym.atPhase(LIRLowering) { 
		hashmap_keyset(LIRLowering(m))(remapManifest(fresh(ks.mK))(ks.mK).asInstanceOf[Manifest[Any]],ks.mV,implicitly[SourceContext]).asInstanceOf[Exp[A]]
	}
    case r@HashMapRemove(m,k) => {
		sym.atPhase(LIRLowering) {
		  val hm = LIRLowering(m)
		  val nvt = remapManifest(fresh(r.ccc))(r.ccc)
	      val c = hashmap_apply(hm.asInstanceOf[Exp[scala.collection.mutable.HashMap[Any,A]]],LIRLowering(k))(r.mK,nvt.asInstanceOf[Manifest[A]],implicitly[SourceContext])
      	  hashmap_remove(hm.asInstanceOf[Exp[scala.collection.mutable.HashMap[Any,A]]], LIRLowering(k))
	   	  c.asInstanceOf[Exp[A]]
		}
    }
	case _ => super.lowerNode(sym,rhs)
  }
    
  override def remap[A](m: Manifest[A]) = m match {
      case s if m <:< manifest[scala.collection.mutable.HashMap[Any,Any]] => "GHashTable *"
      case _ => super.remap(m)
  }

  def remapKeyIfPrimitive(k: Exp[Any]) = k.tp match {
	case s if s == manifest[Int] || s == manifest[Double] || s == manifest[Long] => "GINT_TO_POINTER(" + quote(k) + ")"
	case _ => quote(k)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMapNew() => {
      m.mK match {
        case s if s == manifest[java.lang.String] =>
            emitValDef(sym, "g_hash_table_new(g_str_hash, g_str_equal)")
        case s if s == manifest[Int] || s == manifest[Double] || s == manifest[Long] =>
            emitValDef(sym, "g_hash_table_new(g_direct_hash, g_direct_equal)")
        case _ => throw new Exception("Unknown primitive type " + m.mK + " for CCodegen of HashMapNew.")
      }
    }
    case m@HashMapExtendedNew(k,hf,v1,v2,ef) => {
	    val uhf = uninlinedFunc1(k,hf)
		val uef = uninlinedFunc2(v1,v2,ef)
        emitValDef(sym, "g_hash_table_new((GHashFunc)" + quote(uhf) + ",(GEqualFunc)" + quote(uef) + ")")
    }
	case HashMapSize(m) => emitValDef(sym, "g_hash_table_size(" + quote(m) + ");")
	case HashMapApply(m,k) => emitValDef(sym, "g_hash_table_lookup(" + quote(m) + "," + remapKeyIfPrimitive(k) + ");");
    case HashMapUpdate(m,k,v) => stream.println("g_hash_table_insert(" + quote(m) + "," + remapKeyIfPrimitive(k) + "," + quote(v) + ");");
    case HashMapKeySet(m) => emitValDef(sym, "g_hash_table_get_keys(" + quote(m) + ")")
    case HashMapRemove(m,k) =>
      emitValDef(sym, "g_hash_table_lookup(" + quote(m) + "," + remapKeyIfPrimitive(k) + ");");
      stream.println("g_hash_table_remove(" + quote(m) + "," + remapKeyIfPrimitive(k) + ");");
    case HashMapClear(m) => stream.println("g_hash_table_remove_all(" + quote(m) + ");")
	case HashMapContains(m,k) => emitValDef(sym, "g_hash_table_lookup(" + quote(m) + "," + remapKeyIfPrimitive(k) + ") != NULL")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenHashMapOps extends CudaGenEffect with CLikeGenHashMapOps
trait OpenCLGenHashMapOps extends OpenCLGenEffect with CLikeGenHashMapOps
trait CGenHashMapOps extends CGenEffect with CLikeGenHashMapOps
