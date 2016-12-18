package scala.lms
package common

import java.io.PrintWriter
import internal._
import scala.reflect.{SourceContext, RefinedManifest}
import scala.collection.mutable.{HashMap,Set}

trait ArrayOps extends Variables {

  type Size = Long

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToArrayOps[T:Manifest](x: Var[Array[T]]) = new ArrayOpsCls(readVar(x))
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]) = new ArrayOpsCls(a)
  implicit def repArrayAnyToArrayOps(a: Rep[Array[Any]]) = new ArrayOpsCls(a)
  implicit def arrayToArrayOps[T:Manifest](a: Array[T]) = new ArrayOpsCls(unit(a))

  // substitution for "new Array[T](...)"
  // TODO: look into overriding __new for arrays
  object NewArray {
    def apply[T:Manifest](n: Rep[Size], specializedType: Rep[String] = unit("")) = array_obj_new(n, specializedType)    
  }

  object Array {
    def apply[T:Manifest](xs: T*) = array_obj_fromseq(xs)
  }

  class ArrayOpsCls[T:Manifest](a: Rep[Array[T]]){
    def apply(n: Rep[Size])(implicit pos: SourceContext) = array_apply(a, n)
    def update(n: Rep[Size], y: Rep[T])(implicit pos: SourceContext) = array_update(a,n,y)
    def length(implicit pos: SourceContext) = array_length(a)
    def foreach(block: Rep[T] => Rep[Unit])(implicit pos: SourceContext) = array_foreach(a, block)
    def filter(f: Rep[T] => Rep[Boolean]) = array_filter(a, f)
	def groupBy[B: Manifest](f: Rep[T] => Rep[B]) = array_group_by(a,f)
    def sort(implicit pos: SourceContext) = array_sort(a)
    def map[B:Manifest](f: Rep[T] => Rep[B]) = array_map(a,f)
    def toSeq = array_toseq(a)
	def sum = array_sum(a)
    def zip[B: Manifest](a2: Rep[Array[B]]) = array_zip(a,a2)
    def corresponds[B: Manifest](a2: Rep[Array[B]]) = array_corresponds(a,a2)
    def mkString(del: String = "") = array_mkString(a,del)
    def startsWith[B:Manifest](s2: Rep[Array[B]])(implicit pos: SourceContext) = array_startsWith[T,B](a,s2)
    def endsWith[B:Manifest](s2: Rep[Array[B]])(implicit pos: SourceContext) = array_endsWith[T,B](a,s2)
	def slice(from: Rep[Size], until: Rep[Size]) = array_slice[T](a, from, until)
	def hash = array_hash(a)
	def containsSlice(a2: Rep[Array[T]]) = array_containsSlice(a,a2)
	def indexOfSlice(a2: Rep[Array[T]], idx: Rep[Size]) = array_indexOfSlice(a,a2, idx)
	def compare(a2: Rep[Array[T]]) = array_compare(a,a2)
  }    

  def array_obj_new[T:Manifest](n: Rep[Size], specializedType: Rep[String] = unit("")): Rep[Array[T]]
  def array_obj_fromseq[T:Manifest](xs: Seq[T]): Rep[Array[T]]
  def array_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Size])(implicit pos: SourceContext): Rep[T]
  def array_update[T:Manifest](x: Rep[Array[T]], n: Rep[Size], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def array_unsafe_update[T:Manifest](x: Rep[Array[T]], n: Rep[Size], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def array_length[T:Manifest](x: Rep[Array[T]])(implicit pos: SourceContext) : Rep[Size]
  def array_foreach[T:Manifest](x: Rep[Array[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def array_filter[T : Manifest](l: Rep[Array[T]], f: Rep[T] => Rep[Boolean])(implicit pos: SourceContext): Rep[Array[T]]
  def array_group_by[T : Manifest, B: Manifest](l: Rep[Array[T]], f: Rep[T] => Rep[B])(implicit pos: SourceContext): Rep[HashMap[B, Array[T]]]
  def array_sort[T:Manifest](x: Rep[Array[T]])(implicit pos: SourceContext): Rep[Array[T]]
  def array_map[A:Manifest,B:Manifest](a: Rep[Array[A]], f: Rep[A] => Rep[B]): Rep[Array[B]]
  def array_toseq[A:Manifest](a: Rep[Array[A]]): Rep[Seq[A]]
  def array_sum[A:Manifest](a: Rep[Array[A]]): Rep[A]
  def array_zip[A:Manifest, B: Manifest](a: Rep[Array[A]], a2: Rep[Array[B]]): Rep[Array[(A,B)]]
  def array_corresponds[A: Manifest, B: Manifest](a: Rep[Array[A]], a2: Rep[Array[B]]): Rep[Boolean] // limited support for corresponds (tests equality)
  def array_mkString[A: Manifest](a: Rep[Array[A]], del: String = ""): Rep[String]
  def array_startsWith[A:Manifest, B:Manifest](s1: Rep[Array[A]], s2: Rep[Array[B]])(implicit pos: SourceContext): Rep[Boolean]
  def array_endsWith[A:Manifest, B:Manifest](s1: Rep[Array[A]], s2: Rep[Array[B]])(implicit pos: SourceContext): Rep[Boolean]
  def array_slice[A: Manifest](a: Rep[Array[A]], from: Rep[Size], until: Rep[Size]): Rep[Array[A]]
  def array_hash[A:Manifest](a: Rep[Array[A]]): Rep[Size]
  def array_containsSlice[A:Manifest](s1: Rep[Array[A]], s2: Rep[Array[A]])(implicit pos: SourceContext): Rep[Boolean]
  def array_indexOfSlice[A:Manifest](s1: Rep[Array[A]], s2: Rep[Array[A]], idx: Rep[Size])(implicit pos: SourceContext): Rep[Size]
  def array_compare[A:Manifest](s1: Rep[Array[A]], s2: Rep[Array[A]])(implicit pos: SourceContext): Rep[Int]
  def array_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Size], dest: Rep[Array[T]], destPos: Rep[Size], len: Rep[Size])(implicit pos: SourceContext): Rep[Unit]
  def array_unsafe_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Size], dest: Rep[Array[T]], destPos: Rep[Size], len: Rep[Size])(implicit pos: SourceContext): Rep[Unit]
}

trait ArrayOpsExp extends ArrayOps with EffectExp with VariablesExp with StructExp with WhileExp with OrderingOps with PrimitiveOps   with NumericOps {
  case class ArrayNew[T:Manifest](n: Exp[Size], specializedType: Rep[String] = unit("")) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayFromSeq[T:Manifest](xs: Seq[T]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Size]) extends Def[T]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], n: Exp[Size], y: Exp[T]) extends Def[Unit]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Size] {
    val m = manifest[T]
  }
  case class ArrayForeach[T](a: Exp[Array[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]
  case class ArrayFilter[T : Manifest](l: Exp[Array[T]], x: Sym[T], block: Block[Boolean]) extends Def[Array[T]]
  case class ArrayGroupBy[T: Manifest, B: Manifest](l: Exp[Array[T]], x: Sym[T], block: Block[B]) extends Def[HashMap[B, Array[T]]]
  case class ArraySort[T:Manifest](x: Exp[Array[T]]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayMap[A:Manifest,B:Manifest](a: Exp[Array[A]], x: Sym[A], block: Block[B]) extends Def[Array[B]] {
    val array = NewArray[B](a.length)
  }
  case class ArrayToSeq[A:Manifest](x: Exp[Array[A]]) extends Def[Seq[A]]
  case class ArraySum[A:Manifest](x: Exp[Array[A]]) extends Def[A]
  case class ArrayZip[A:Manifest, B: Manifest](x: Exp[Array[A]], x2: Exp[Array[B]]) extends Def[Array[(A,B)]]
  case class ArrayCorresponds[A:Manifest, B: Manifest](x: Exp[Array[A]], x2: Exp[Array[B]]) extends Def[Boolean]
  case class ArrayMkString[A:Manifest](a: Exp[Array[A]], b: String = "") extends Def[String]
  case class ArrayStartsWith[A:Manifest,B:Manifest](s1: Exp[Array[A]], s2: Exp[Array[B]]) extends Def[Boolean]
  case class ArrayEndsWith[A:Manifest,B:Manifest](s1: Exp[Array[A]], s2: Exp[Array[B]]) extends Def[Boolean]
  case class ArraySlice[A:Manifest](a: Exp[Array[A]], from: Exp[Size], until: Exp[Size]) extends Def[Array[A]] {
	val m = manifest[A]
  }
  case class ArrayContainsSlice[A:Manifest](s1: Exp[Array[A]], s2: Exp[Array[A]]) extends Def[Boolean]
  case class ArrayIndexOfSlice[A:Manifest](s1: Exp[Array[A]], s2: Exp[Array[A]], idx: Rep[Size]) extends Def[Size] {
    val m = manifest[A]
  }
  case class ArrayCompare[A:Manifest](s1: Exp[Array[A]], s2: Exp[Array[A]]) extends Def[Int] {
    val m = manifest[A]
  }
  case class ArrayHash[A:Manifest](a: Exp[Array[A]]) extends Def[Size]
  case class ArrayCopy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Size], dest: Exp[Array[T]], destPos: Exp[Size], len: Exp[Size]) extends Def[Unit] {
    val m = manifest[T]
  }
  
  def array_obj_new[T:Manifest](n: Exp[Size], specializedType: Rep[String] = unit("")) = reflectMutable(ArrayNew(n, specializedType))
  def array_obj_fromseq[T:Manifest](xs: Seq[T]) = /*reflectMutable(*/ ArrayFromSeq(xs) /*)*/
  def array_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Size])(implicit pos: SourceContext): Exp[T] = ArrayApply(x, n)
  def array_update[T:Manifest](x: Exp[Array[T]], n: Exp[Size], y: Exp[T])(implicit pos: SourceContext) = reflectWrite(x)(ArrayUpdate(x,n,y))
  def array_unsafe_update[T:Manifest](x: Rep[Array[T]], n: Rep[Size], y: Rep[T])(implicit pos: SourceContext) = ArrayUpdate(x,n,y)
  def array_length[T:Manifest](a: Exp[Array[T]])(implicit pos: SourceContext) : Rep[Size] = ArrayLength(a)
  def array_foreach[T:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Unit])(implicit pos: SourceContext): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayForeach(a, x, b), summarizeEffects(b).star)
  }
  def array_filter[T : Manifest](l: Exp[Array[T]], f: Exp[T] => Exp[Boolean])(implicit pos: SourceContext) = {
    val a = fresh[T]
    val b = reifyEffects(f(a))
    reflectEffect(ArrayFilter(l, a, b), summarizeEffects(b).star)
  }
  def array_group_by[T : Manifest, B: Manifest](l: Exp[Array[T]], f: Exp[T] => Exp[B])(implicit pos: SourceContext) = {
    val a = fresh[T]
    val b = reifyEffects(f(a))
    reflectEffect(ArrayGroupBy(l, a, b), summarizeEffects(b).star)
  }

  def array_sort[T:Manifest](x: Exp[Array[T]])(implicit pos: SourceContext) = ArraySort(x)
  def array_map[A:Manifest,B:Manifest](a: Exp[Array[A]], f: Exp[A] => Exp[B]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(ArrayMap(a, x, b), summarizeEffects(b))
  }
  def array_toseq[A:Manifest](a: Exp[Array[A]]) = ArrayToSeq(a)
  def array_sum[A:Manifest](a: Exp[Array[A]]) = reflectEffect(ArraySum(a))
  def array_zip[A:Manifest, B: Manifest](a: Exp[Array[A]], a2: Exp[Array[B]]) = reflectEffect(ArrayZip(a,a2))
  def array_corresponds[A: Manifest, B: Manifest](a: Rep[Array[A]], a2: Rep[Array[B]]) = reflectEffect(ArrayCorresponds(a,a2))
  def array_mkString[A: Manifest](a: Rep[Array[A]], del: String = "") = reflectEffect(ArrayMkString(a, del))
  def array_startsWith[A:Manifest,B:Manifest](s1: Exp[Array[A]], s2: Exp[Array[B]])(implicit pos: SourceContext) = ArrayStartsWith(s1,s2)
  def array_endsWith[A:Manifest,B:Manifest](s1: Exp[Array[A]], s2: Exp[Array[B]])(implicit pos: SourceContext) = ArrayEndsWith(s1,s2)
  def array_slice[A: Manifest](a: Exp[Array[A]], from: Exp[Size], until: Exp[Size]) = reflectEffect(ArraySlice[A](a,from,until))
  def array_hash[A:Manifest](a: Rep[Array[A]]) = reflectEffect(ArrayHash(a))
  def array_containsSlice[A:Manifest](s1: Exp[Array[A]], s2: Exp[Array[A]])(implicit pos: SourceContext) = ArrayContainsSlice(s1,s2)
  def array_indexOfSlice[A:Manifest](s1: Exp[Array[A]], s2: Exp[Array[A]], idx: Exp[Size])(implicit pos: SourceContext) = ArrayIndexOfSlice(s1,s2,idx)
  def array_compare[A:Manifest](s1: Exp[Array[A]], s2: Exp[Array[A]])(implicit pos: SourceContext)= ArrayCompare(s1,s2)
  def array_copy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Size], dest: Exp[Array[T]], destPos: Exp[Size], len: Exp[Size])(implicit pos: SourceContext) = reflectWrite(dest)(ArrayCopy(src,srcPos,dest,destPos,len))
  def array_unsafe_copy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Size], dest: Exp[Array[T]], destPos: Exp[Size], len: Exp[Size])(implicit pos: SourceContext) = ArrayCopy(src,srcPos,dest,destPos,len)
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayApply(a,x) => array_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case ArrayLength(x) => array_length(f(x))
    case e@ArraySort(x) => array_sort(f(x))(e.m,pos)
    case e@ArrayCopy(a,ap,d,dp,l) => toAtom(ArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.m))(mtype(manifest[A]),pos)
    case ArrayCompare(a1, a2) => array_compare(f(a1), f(a2))
    case ArrayStartsWith(a1, a2) => array_startsWith(f(a1), f(a2))
    case ArrayEndsWith(a1, a2) => array_startsWith(f(a1), f(a2))
    case ArrayContainsSlice(a1, a2) => array_startsWith(f(a1), f(a2))
    case Reflect(e@ArraySlice(arr,idx1,idx2), u, es) => reflectMirrored(Reflect(ArraySlice(f(arr), f(idx1), f(idx2))(e.m), mapOver(f,u),f(es)))(mtype(manifest[A]))
    case Reflect(e@ArrayIndexOfSlice(arr1,arr2,idx), u, es) => reflectMirrored(Reflect(ArrayIndexOfSlice(f(arr1), f(arr2), f(idx))(e.m), mapOver(f,u),f(es)))(mtype(manifest[A]))
    case Reflect(e@ArrayNew(n, sType), u, es) => reflectMirrored(Reflect(ArrayNew(f(n), sType)(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@ArrayLength(x), u, es) => reflectMirrored(Reflect(ArrayLength(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(ArrayApply(l,r), u, es) => reflectMirrored(Reflect(ArrayApply(f(l),f(r))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@ArraySort(x), u, es) => reflectMirrored(Reflect(ArraySort(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(ArrayUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@ArrayCopy(a,ap,d,dp,l), u, es) => reflectMirrored(Reflect(ArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => syms(a):::syms(body)
    case ArrayMap(a, x, body) => syms(a):::syms(body)
    case ArrayFilter(a, x, body) => syms(a):::syms(body)
    case ArrayGroupBy(a, x, body) => syms(a):::syms(body)
	case ArraySlice(a,idx1,idx2) => syms(a)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => x :: effectSyms(body)
    case ArrayMap(a, x, body) => x :: effectSyms(body)
    case ArrayFilter(a, x, body) => x :: effectSyms(body)
    case ArrayGroupBy(a, x, body) => x::effectSyms(body)
	case ArraySlice(a,idx1,idx2) => effectSyms(a)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayFilter(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayGroupBy(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }

}

trait ArrayOpsExpOpt extends ArrayOpsExp {


  override def array_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Size])(implicit pos: SourceContext): Exp[T] = {
    if (context ne null) {
      // find the last modification of array x
      // if it is an assigment at index n, just return the last value assigned
      val vs = x.asInstanceOf[Sym[Array[T]]]
      //TODO: could use calculateDependencies?

      val rhs = context.reverse.collectFirst {
        //case w @ Def(Reflect(ArrayNew(sz: Exp[T]), _, _)) if w == x => Some(Const(0)) // FIXME: bounds check!
        case Def(Reflect(ArrayUpdate(`x`, `n`, rhs: Exp[T]), _, _)) => Some(rhs)
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.array_apply(x,n))
    } else {
      super.array_apply(x,n)
    }
  }

  override def array_update[T:Manifest](x: Exp[Array[T]], n: Exp[Size], y: Exp[T])(implicit pos: SourceContext) = {
    if (context ne null) {
      // find the last modification of array x
      // if it is an assigment at index n with the same value, just do nothing
      val vs = x.asInstanceOf[Sym[Array[T]]]
      //TODO: could use calculateDependencies?

      val rhs = context.reverse.collectFirst {
        //case w @ Def(Reflect(ArrayNew(sz: Exp[T]), _, _)) if w == x => Some(Const(())) // FIXME: bounds check!
        case Def(Reflect(ArrayUpdate(`x`, `n`, `y`), _, _)) => Some(Const(()))
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.array_update(x,n,y))
    } else {
      super.array_update(x,n,y)
    }
  }



}




trait BaseGenArrayOps extends GenericNestedCodegen {
  val IR: ArrayOpsExp
  import IR._

}

trait ScalaGenArrayOps extends BaseGenArrayOps with ScalaGenBase {
  val IR: ArrayOpsExp
  import IR._

  val ARRAY_LITERAL_MAX_SIZE = 1000

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n, sType) => {
        val arrType = if (quote(sType) != "\"\"") quote(sType).replaceAll("\"","") else remap(a.m)
        emitValDef(sym, src"new Array[$arrType]($n)")
    }
    case e@ArrayFromSeq(xs) => {
      emitData(sym, xs)
      emitValDef(sym,
        if(xs.size > ARRAY_LITERAL_MAX_SIZE) {
          /* def append(i: Int) = {
            val start = i*ARRAY_LITERAL_MAX_SIZE
            val end = Math.min((i+1)*ARRAY_LITERAL_MAX_SIZE, xs.size)
            val size = end - start
            "def x" + sym.id + "_" + i + "=Array(" + (start until end).map{xs(_)} + ")\nArray.copy(x" + sym.id + "_" + i + ",0,buf," + start + "," + size + ")\n"
          }
          val numBlocks = Math.ceil(xs.size / ARRAY_LITERAL_MAX_SIZE).intValue
          "{val buf=new Array[" + remap(e.mt) + "](" + xs.size + ")\n" + ((0 until numBlocks).map(append)).mkString("\n") + "buf}" */
          "{import scala.io.Source;(Source.fromFile(\"" + symDataPath(sym) + "\").getLines.map{Integer.parseInt(_)}).toArray}"
        }
        else {
          src"Array($xs)"
        }
      )
    }
    case ArrayApply(x,n) => emitValDef(sym, src"$x($n)")
    case ArrayUpdate(x,n,y) => emitAssignment(sym, src"$x($n)", quote(y))
    case ArrayLength(x) => emitValDef(sym, src"$x.length")
    case ArrayForeach(a,x,block) => 
      stream.println(quote(a) + ".foreach{")
      gen"""$x => 
           |${nestedBlock(block)}
           |$block
           |}"""
    case a@ArraySort(x) => 
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        gen"""{
             |val d = new Array[${a.m}]($x.length)
             |System.arraycopy($x, 0, d, 0, $x.length)
             |scala.util.Sorting.quickSort(d)
             |d
             |}"""
      }
      emitValDef(sym, strWriter.toString)
    case n@ArrayMap(a,x,blk) => 
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
      //stream.println("/* workaround for refinedManifest problem */")
        gen"""{
             |val out = ${n.array}
             |val in = $a
             |var i = 0
             |while (i < in.length) {
             |val $x = in(i)
             |${nestedBlock(blk)}
             |out(i) = $blk
             |i += 1
             |}
             |out
             |}"""
      }
      emitValDef(sym, strWriter.toString)
	case ArrayFilter(a,x,blk) =>
		emitValDef(sym, quote(a) + ".filter(" + quote(x) + "=> {")
		emitBlock(blk)
		emitBlockResult(blk)
		stream.println("})") 
	case ArrayGroupBy(a,x,blk) =>
		emitValDef(sym, quote(a) + ".groupBy(" + quote(x) + "=> {")
		emitBlock(blk)
		emitBlockResult(blk)
		stream.println("})") 
	case ArraySum(a) => emitValDef(sym, quote(a) + ".sum")
    case ArrayToSeq(a) => emitValDef(sym, src"$a.toSeq")
    case ArrayZip(a,a2) => emitValDef(sym, src"$a zip $a2") 
    case ArrayCorresponds(a,a2) => emitValDef(sym, src"$a.corresponds($a2){_==_}") 
    case ArrayMkString(a, del) => 
      if (del != "")
        emitValDef(sym, src"$a.mkString($del)")
      else
        emitValDef(sym, src"$a.mkString")	
	case ArrayStartsWith(a,a2) => emitValDef(sym, src"$a.startsWith($a2)")
	case ArrayEndsWith(a,a2) => emitValDef(sym, src"$a.endsWith($a2)")
	case ArraySlice(a,from,until) => emitValDef(sym, src"$a.slice($from,$until)")
	case ArrayHash(a) => emitValDef(sym, src"$a.foldLeft(0) { (cnt,x) => cnt + x.## }")
	case ArrayContainsSlice(a,a2) => emitValDef(sym, src"$a.containsSlice($a2)")
	case ArrayIndexOfSlice(a,a2,idx) => emitValDef(sym, src"$a.indexOfSlice($a2,$idx)")
	case ArrayCompare(a,a2) => emitValDef(sym, "(" + quote(a) + ".zip(" + quote(a2) + ")).foldLeft(0){ (res, elem) => if (res == 0) elem._1 - elem._2 else res}")
    case ArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, src"System.arraycopy($src,$srcPos,$dest,$destPos,$len)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayOps extends BaseGenArrayOps with CLikeGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ArrayLength(x) => emitValDef(sym, src"$x.length")
        case ArrayApply(x,n) => emitValDef(sym, src"$x.apply($n)")
        case ArrayUpdate(x,n,y) => stream.println(src"$x.update($n,$y);")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayOps extends CudaGenBase with CLikeGenArrayOps
trait OpenCLGenArrayOps extends OpenCLGenBase with CLikeGenArrayOps
trait CGenArrayOps extends CGenEffect with CGenStruct {
	val IR: ArrayOpsExp
  	import IR._

/*
    
	override def remapManifest[T:Manifest](sym: Sym[T]): Manifest[_] = {
		if (sym.tp.erasure.isArray) manifest[LoweredArray[T]]
		else super.remapManifest(sym)
	}

    override def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]) = rhs match {
        case a@ArrayNew(n, sType) => {
            sym.atPhase(LIRLowering) {
               reflectEffect(SimpleStruct[LoweredArray[T]](ClassTag(structName(manifest[T])), Seq(
                    "array" -> reflectEffect(rhs),
                    "length" -> n
			   ))).asInstanceOf[Exp[T]]
            }
        }
        case ArrayApply(a, n) => {
            sym.atPhase(LIRLowering) {	
                val ar = field[Array[T]](LIRLowering(a), "array")
				reflectEffect(ArrayApply(ar, LIRLowering(n))).asInstanceOf[Exp[T]]
            }
		}
        case ArrayFilter(a,x,blk) => {
			/* Not yet implemented, not needed so far (we take advantage of the final
			  '\0' character for array of bytes for all operations of importance) */ 
            sym.atPhase(LIRLowering) {
               LIRLowering(a).asInstanceOf[Exp[T]]
            }
        }
        case ArrayUpdate(a, n, y) => { 
            sym.atPhase(LIRLowering) {
                val ar = field[Array[Any]](LIRLowering(a), "array")
				reflectEffect(ArrayUpdate(ar, LIRLowering(n), LIRLowering(y))).asInstanceOf[Exp[T]]
        	}
        }
        case ar@ArrayLength(a) => 
            sym.atPhase(LIRLowering) {
                field(LIRLowering(a),"length").asInstanceOf[Exp[T]]
        	}
		case ArrayHash(a) =>
			sym.atPhase(LIRLowering) {
                if (a.tp != manifest[Array[Byte]]) throw new Exception("Unknown manifest " + a.tp + " when lowering ArrayHash.")
				val arr = field[Array[Byte]](LIRLowering(a),"array")
				val arrlen = field(LIRLowering(a),"length").asInstanceOf[Rep[Int]]
				var i = var_new[Int](unit(0))
                var h = var_new[Int](unit(0))
				__whileDo(i < arrlen && arr(i) != unit('\0'), {
                    h += arr(i)
					i+=1
				})
				readVar(h).asInstanceOf[Exp[T]]
			}
        case ArrayCorresponds(a1,a2) => 
            sym.atPhase(LIRLowering) {
                if (a1.tp == manifest[Array[Byte]] && a2.tp == manifest[Array[Byte]]) {
				    val arr2 = field[Array[Byte]](LIRLowering(a2),"array")
                    __equal(LIRLowering(a1),arr2).asInstanceOf[Exp[T]]
                } else throw new Exception("Unknown manifest combination (" + a1.tp + "," + a2.tp + ") when lowering ArrayCorresponds.")
            }
        case ArrayCompare(a1,a2) =>
            sym.atPhase(LIRLowering) {
                if (a1.tp == manifest[Array[Byte]] && (a2.tp == manifest[Array[Byte]] || a2.tp.typeArguments(0) == manifest[Array[Byte]])) {
				    val arr1 = field[Array[Byte]](LIRLowering(a1),"array")
				    val arr2 = field[Array[Byte]](LIRLowering(a2),"array")
                    string_compareTo(arr1.asInstanceOf[Exp[String]],arr2.asInstanceOf[Exp[String]]).asInstanceOf[Exp[T]]
                } else throw new Exception("Unknown manifest combination (" + a1.tp + "," + a2.tp + ") when lowering ArrayCompare.")	
            }
		case ArrayStartsWith(a1,a2) =>
			sym.atPhase(LIRLowering) {
                if (a1.tp == manifest[Array[Byte]] && a2.tp == manifest[java.lang.String]) {
				    val arr1 = field[Array[Byte]](LIRLowering(a1),"array")
                    string_startswith(arr1.asInstanceOf[Exp[String]],a2.asInstanceOf[Exp[String]]).asInstanceOf[Exp[T]]
                } else throw new Exception("Unknown manifest combination (" + a1.tp + "," + a2.tp + ") when lowering ArrayStartsWith.")
			}
		case ArrayEndsWith(a1,a2) =>
			sym.atPhase(LIRLowering) {
                if (a1.tp == manifest[Array[Byte]] && a2.tp == manifest[java.lang.String]) {
				    val arr1 = field[Array[Byte]](LIRLowering(a1),"array")
                    string_endswith(arr1.asInstanceOf[Exp[String]],a2.asInstanceOf[Exp[String]]).asInstanceOf[Exp[T]]
                } else throw new Exception("Unknown manifest combination (" + a1.tp + "," + a2.tp + ") when lowering ArrayEndsWith.")
			}
		case ArrayContainsSlice(a1,a2) =>
			sym.atPhase(LIRLowering) {
                if (a1.tp == manifest[Array[Byte]] && a2.tp == manifest[java.lang.String]) {
				    val arr1 = field[Array[Byte]](LIRLowering(a1),"array")
                    string_containsSlice(arr1.asInstanceOf[Exp[String]],a2.asInstanceOf[Exp[String]]).asInstanceOf[Exp[T]]
                } else throw new Exception("Unknown manifest combination (" + a1.tp + "," + a2.tp + ") when lowering ArrayContainsSlice.")
			}
		case ArrayIndexOfSlice(a1,a2,idx) =>
			sym.atPhase(LIRLowering) {
                if (a1.tp == manifest[Array[Byte]] && a2.tp == manifest[java.lang.String]) {
				    val arr1 = field[Array[Byte]](LIRLowering(a1),"array")
                    string_indexOfSlice(arr1.asInstanceOf[Exp[String]],LIRLowering(a2).asInstanceOf[Exp[String]],LIRLowering(idx)).asInstanceOf[Exp[T]]
                } else throw new Exception("Unknown manifest combination (" + a1.tp + "," + a2.tp + ") when lowering ArrayIndexOfSlice.")
			}
		case as@ArraySlice(arr,idx1,idx2) => 
			sym.atPhase(LIRLowering) {
				val len = LIRLowering(idx2) - LIRLowering(idx1) + 1
				// Initialize new array
                val newarr = reflectEffect(SimpleStruct[LoweredArray[T]](ClassTag(structName(manifest[T])), Seq(
                    "array" -> array_obj_new(len)(as.m),
                    "length" -> len
			    )))
				// Perform the copy
                // TODO: FIX THE TYPE OF THIS ARRAY -- IT CAN BE ANY, BUT DOES IT AFFECT ANYTHING?
				val arr1 = field[Array[Byte]](newarr,"array")
				val arr2 = field[Array[Byte]](LIRLowering(arr),"array")
				array_copy(arr2.asInstanceOf[Exp[Array[Any]]],unit(0),arr1.asInstanceOf[Exp[Array[Any]]],unit(0),len-unit(1))
				newarr.asInstanceOf[Exp[T]]
			}	
        case _ => super.lowerNode(sym, rhs)
    }

*/


	abstract class LoweredArray[T:Manifest] 
    
    override def remap[A](m: Manifest[A]) = m match {
	   case s if m <:< manifest[LoweredArray[Any]] => "struct " + structName(m.typeArguments.head) + "*"
       case s if m.erasure.isArray => remap(m.typeArguments.head) + "*"
       case _ => super.remap(m)
	}
  
    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    	rhs match {
    		case a@ArrayNew(n, sType) => {
        		val arrType = if (quote(sType) != "\"\"") quote(sType) else remap(a.m)
		        stream.println(arrType + "* " + quote(sym) + " = " + getMemoryAllocString(quote(n), arrType))
			}
			case ArrayForeach(a,x,blk) => {
				stream.println("{")
				stream.println("int i = 0;")
				stream.println("for ( i = 0 ; i < " + quote(findInitSymbol(a)) + "Size; i += 1) {")
				emitBlock(blk)
				emitBlockResult(blk)
				stream.println("}")
				stream.println("};")
			}
			case ArrayApply(x,n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
        	case ArrayUpdate(x,n,y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
			case ArrayCopy(src,s1,dest,s2,len) =>
				stream.println("memcpy(" + quote(dest) + "," + quote(src) + "," + quote(len) + ");")
				stream.println(quote(dest) + "[" + quote(len) + "] = '\\0';")
        	case _ => super.emitNode(sym, rhs)
		}
	}
}
