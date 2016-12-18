package scala.lms
package common

import java.io.PrintWriter
import scala.lms.internal._
import scala.collection.mutable.Set
import scala.reflect.SourceContext

trait SetOps extends Base with Variables {
  object Set {
    def apply[A:Manifest](xs: Rep[A]*)(implicit pos: SourceContext) = set_new[A](xs)
	def empty = set_empty()
  }

  implicit def repSetToSetOps[A:Manifest](v: Rep[Set[A]]) = new setOpsCls(v)
  implicit def varSetToSetOps[A:Manifest](v: Var[Set[A]]) = new setOpsCls(readVar(v))

  class setOpsCls[A:Manifest](s: Rep[Set[A]]) {
    def contains(i: Rep[A])(implicit pos: SourceContext) = set_contains(s, i)
    def add(i: Rep[A])(implicit pos: SourceContext) = set_add(s, i)
    def remove(i: Rep[A])(implicit pos: SourceContext) = set_remove(s, i)
    def size(implicit pos: SourceContext) = set_size(s)
    def clear()(implicit pos: SourceContext) = set_clear(s)
    def toSeq(implicit pos: SourceContext) = set_toseq(s)
    def toArray(implicit pos: SourceContext) = set_toarray(s)
    def map[B:Manifest](f: Rep[A] => Rep[B]) = set_map(s,f)
    def foreach(block: Rep[A] => Rep[Unit])(implicit pos: SourceContext) = set_foreach(s, block)
	def head(implicit pos: SourceContext) = set_head(s)
  }

  def set_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext) : Rep[Set[A]]
  def set_contains[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Boolean]
  def set_add[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Unit]
  def set_remove[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Boolean]
  def set_size[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Int]
  def set_map[A:Manifest,B:Manifest](a: Rep[Set[A]], f: Rep[A] => Rep[B]): Rep[Set[B]]
  def set_clear[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Unit]
  def set_toseq[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def set_toarray[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def set_foreach[T:Manifest](x: Rep[Set[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def set_empty[T:Manifest]() : Rep[Set[T]]
  def set_head[T:Manifest](m: Rep[Set[T]])(implicit pos: SourceContext): Rep[T]
}

trait SetOpsExp extends SetOps with ArrayOps with EffectExp {
  case class SetNew[A:Manifest](xs: Seq[Exp[A]], mA: Manifest[A]) extends Def[Set[A]]
  case class SetContains[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Boolean]
  case class SetAdd[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetRemove[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Boolean] {
	val m = manifest[A]
  }
  case class SetSize[A:Manifest](s: Exp[Set[A]]) extends Def[Int]
  case class SetClear[A:Manifest](s: Exp[Set[A]]) extends Def[Unit]
  case class SetToSeq[A:Manifest](s: Exp[Set[A]]) extends Def[Seq[A]]
  case class SetToArray[A:Manifest](s: Exp[Set[A]]) extends Def[Array[A]]
  case class SetMap[A:Manifest,B:Manifest](a: Exp[Set[A]], x: Sym[A], block: Block[B]) extends Def[Set[B]]
  case class SetForeach[T](a: Exp[Set[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]
  case class SetEmpty[T:Manifest]() extends Def[Set[T]] {
	val m = manifest[T]
  }
  case class SetHead[T:Manifest](s: Exp[Set[T]]) extends Def[T] {
	val m = manifest[T]
  }

  def set_new[A:Manifest](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(SetNew(xs, manifest[A]))
  def set_contains[A:Manifest](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = SetContains(s, i)
  def set_add[A:Manifest](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = reflectWrite(s)(SetAdd(s, i))
  def set_remove[A:Manifest](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = reflectEffect(SetRemove(s, i))
  def set_size[A:Manifest](s: Exp[Set[A]])(implicit pos: SourceContext) = SetSize(s)
  def set_clear[A:Manifest](s: Exp[Set[A]])(implicit pos: SourceContext) = reflectWrite(s)(SetClear(s))
  def set_toseq[A:Manifest](s: Exp[Set[A]])(implicit pos: SourceContext) = SetToSeq(s)
  def set_toarray[A:Manifest](s: Exp[Set[A]])(implicit pos: SourceContext) = SetToArray(s)
  def set_foreach[T:Manifest](a: Exp[Set[T]], block: Exp[T] => Exp[Unit])(implicit pos: SourceContext): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(SetForeach(a, x, b), summarizeEffects(b).star)
  }
  def set_map[A:Manifest,B:Manifest](a: Exp[Set[A]], f: Exp[A] => Exp[B]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(SetMap(a, x, b), summarizeEffects(b))
  }
  def set_empty[T:Manifest]() = reflectEffect(SetEmpty())
  def set_head[T:Manifest](s: Exp[Set[T]])(implicit pos: SourceContext) = reflectEffect(SetHead(s))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case Reflect(st@SetHead(s), u, es) => reflectMirrored(Reflect(SetHead(f(s))(st.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(st@SetRemove(s,el), u, es) => reflectMirrored(Reflect(SetRemove(f(s),f(el))(st.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case SetForeach(a, x, body) => syms(a):::syms(body)
    case SetMap(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SetForeach(a, x, body) => x :: effectSyms(body)
    case SetMap(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case SetForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case SetMap(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }

}

trait BaseGenSetOps extends GenericNestedCodegen {
  val IR: SetOpsExp
  import IR._

}

trait ScalaGenSetOps extends BaseGenSetOps with ScalaGenEffect {
  val IR: SetOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SetNew(xs, mA) => emitValDef(sym, src"collection.mutable.HashSet[$mA](" + (xs map {quote}).mkString(",") + ")")
    case SetContains(s,i) => emitValDef(sym, src"$s.contains($i)")
    case SetAdd(s,i) => emitValDef(sym, src"$s.add($i)")
    case SetRemove(s,i) => emitValDef(sym, src"$s.remove($i)")
    case SetSize(s) => emitValDef(sym, src"$s.size")
    case SetClear(s) => emitValDef(sym, src"$s.clear()")
    case SetToSeq(s) => emitValDef(sym, src"$s.toSeq")
    case n@SetToArray(s) => emitValDef(sym, quote(s) + ".toArray")
    case SetForeach(a,x,block) => 
      emitValDef(sym, src"$a.foreach{")    
      gen"""$x => 
           |${nestedBlock(block)}
           |$block
           |}"""
	case SetMap(a,x,block) => 
      emitValDef(sym, src"$a.map{")    
      gen"""$x => 
           |${nestedBlock(block)}
           |$block
           |}"""
	case SetEmpty() => emitValDef(sym, "scala.collection.mutable.HashSet.empty")
	case SetHead(s) => emitValDef(sym, src"$s.head")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSetOps extends BaseGenSetOps with CLikeCodegen {
  val IR: SetOpsExp
  import IR._    
}

trait CudaGenSetOps extends CudaGenEffect with CLikeGenSetOps
trait OpenCLGenSetOps extends OpenCLGenEffect with CLikeGenSetOps

trait CGenSetOps extends CGenEffect with CNestedCodegen {
  val IR: SetOpsExp
  import IR._    

  override def remap[A](m: Manifest[A]) = m match {
	  case s if m <:< manifest[Set[Any]] => "GList**"
      case _ => super.remap(m)
  }

  override def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
	if (sym.tp <:< manifest[Variable[Set[Any]]]) 
    	stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
	else super.emitVarDef(sym,rhs)
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
	if (sym.tp <:< manifest[Set[Any]]) {
		stream.println(remap(sym.tp) + " " + quote(sym) + " = " + getMemoryAllocString("1", "GList*") + ";");
		stream.println("*" + quote(sym) + " = " + rhs + ";")
	}
	else super.emitValDef(sym,rhs)
  }
  
  override def lowerNode[A:Manifest](sym: Sym[A], rhs: Def[A]) = rhs match {
	case sh@SetHead(s) => sym.atPhase(LIRLowering) {
        val ls = LIRLowering(s)
		set_head(ls.asInstanceOf[Exp[Set[Any]]])(remapManifest(fresh(sh.m))(sh.m).asInstanceOf[Manifest[Any]],implicitly[SourceContext]).asInstanceOf[Exp[A]]
	}
	case _ => super.lowerNode(sym,rhs)
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
	case SetHead(s) => {
		//NOTE: TODO: I do not like this -- find a better way
		if (sym.tp == manifest[Int] || sym.tp == manifest[Double])
			emitValDef(sym, src"GPOINTER_TO_INT(g_list_first(*$s)->data)")
		else emitValDef(sym, src"g_list_first(*$s)->data;")
	}
	case SetRemove(s,e) => 
		//NOTE: TODO: I do not like this -- find a better way
		if (e.tp == manifest[Int] || e.tp == manifest[Double])
 			stream.println(src"*$s = g_list_remove(*$s,GINT_TO_POINTER($e));")
		else stream.println(src"*$s = g_list_remove(*$s,$e);")
    case _ => super.emitNode(sym, rhs)
  }
}
