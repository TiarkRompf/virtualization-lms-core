package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal._
import scala.reflect.SourceContext

trait ArrayOps extends Variables {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToArrayOps[T:Manifest](x: Var[Array[T]]) = new ArrayOpsCls(readVar(x))
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]) = new ArrayOpsCls(a)
  implicit def arrayToArrayOps[T:Manifest](a: Array[T]) = new ArrayOpsCls(unit(a))

  // substitution for "new Array[T](...)"
  // TODO: look into overriding __new for arrays
  object NewArray {
    def apply[T:Manifest](n: Rep[Int]) = array_obj_new(n)    
  }
  
  object Array {
    def apply[T:Manifest](xs: T*) = array_obj_fromseq(xs)
  }
  
  class ArrayOpsCls[T:Manifest](a: Rep[Array[T]]){
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = array_apply(a, n)
    def update(n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext) = array_update(a,n,y)
    def length(implicit ctx: SourceContext) = array_length(a)
    def foreach(block: Rep[T] => Rep[Unit])(implicit ctx: SourceContext) = array_foreach(a, block)
    def sort(implicit ctx: SourceContext) = array_sort(a)
    def map[B:Manifest](f: Rep[T] => Rep[B]) = array_map(a,f)
    def toSeq = array_toseq(a)
  }    

  def array_obj_new[T:Manifest](n: Rep[Int]): Rep[Array[T]]
  def array_obj_fromseq[T:Manifest](xs: Seq[T]): Rep[Array[T]]
  def array_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Int])(implicit ctx: SourceContext): Rep[T]
  def array_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def array_unsafe_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def array_length[T:Manifest](x: Rep[Array[T]])(implicit ctx: SourceContext) : Rep[Int]
  def array_foreach[T:Manifest](x: Rep[Array[T]], block: Rep[T] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def array_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def array_unsafe_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def array_sort[T:Manifest](x: Rep[Array[T]])(implicit ctx: SourceContext): Rep[Array[T]]
  def array_map[A:Manifest,B:Manifest](a: Rep[Array[A]], f: Rep[A] => Rep[B]): Rep[Array[B]]
  def array_toseq[A:Manifest](a: Rep[Array[A]]): Rep[Seq[A]]
}

trait ArrayOpsExp extends ArrayOps with EffectExp with VariablesExp {
  case class ArrayNew[T:Manifest](n: Exp[Int]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayFromSeq[T:Manifest](xs: Seq[T]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], n: Exp[Int], y: Exp[T]) extends Def[Unit]  
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayForeach[T](a: Exp[Array[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]
  case class ArrayCopy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int]) extends Def[Unit]
  case class ArraySort[T:Manifest](x: Exp[Array[T]]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayMap[A:Manifest,B:Manifest](a: Exp[Array[A]], x: Sym[A], block: Block[B]) extends Def[Array[B]] {
    val array = NewArray[B](a.length)
  }
  case class ArrayToSeq[A:Manifest](x: Exp[Array[A]]) extends Def[Seq[A]]
  
  def array_obj_new[T:Manifest](n: Exp[Int]) = reflectMutable(ArrayNew(n))
  def array_obj_fromseq[T:Manifest](xs: Seq[T]) = /*reflectMutable(*/ ArrayFromSeq(xs) /*)*/
  def array_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Int])(implicit ctx: SourceContext): Exp[T] = ArrayApply(x, n)
  def array_update[T:Manifest](x: Exp[Array[T]], n: Exp[Int], y: Exp[T])(implicit ctx: SourceContext) = reflectWrite(x)(ArrayUpdate(x,n,y))
  def array_unsafe_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext) = ArrayUpdate(x,n,y)
  def array_length[T:Manifest](a: Exp[Array[T]])(implicit ctx: SourceContext) : Rep[Int] = ArrayLength(a)
  def array_foreach[T:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Unit])(implicit ctx: SourceContext): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayForeach(a, x, b), summarizeEffects(b).star)
  }
  def array_copy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(dest)(ArrayCopy(src,srcPos,dest,destPos,len))
  def array_unsafe_copy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = ArrayCopy(src,srcPos,dest,destPos,len)
  def array_sort[T:Manifest](x: Exp[Array[T]])(implicit ctx: SourceContext) = ArraySort(x)
  def array_map[A:Manifest,B:Manifest](a: Exp[Array[A]], f: Exp[A] => Exp[B]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(ArrayMap(a, x, b), summarizeEffects(b))    
  }
  def array_toseq[A:Manifest](a: Exp[Array[A]]) = ArrayToSeq(a)
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    (e match {
      case ArrayLength(a) => array_length(f(a))
      case ArrayApply(a,x) => array_apply(f(a),f(x))
      case Reflect(e@ArrayNew(n), u, es) => reflectMirrored(Reflect(ArrayNew(f(n))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case Reflect(ArrayApply(l,r), u, es) => reflectMirrored(Reflect(ArrayApply(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(ArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(ArrayUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => syms(a):::syms(body)
    case ArrayMap(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => x :: effectSyms(body)
    case ArrayMap(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayMap(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case a@ArrayNew(n) => emitValDef(sym, "new Array[" + remap(a.m) + "](" + quote(n) + ")")
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
          "Array(" + xs.mkString(",") + ")"
        }
      )
    }
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case ArrayUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case ArrayForeach(a,x,block) => stream.println("val " + quote(sym) + " = " + quote(a) + ".foreach{")    
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("}")
    case ArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    case a@ArraySort(x) => 
      stream.println("val " + quote(sym) + " = {")
      stream.println("val d = new Array[" + remap(a.m) + "](" + quote(x) + ".length" + ")")
      stream.println("System.arraycopy(" + quote(x) + ", 0, d, 0, " + quote(x) + ".length)")
      stream.println("scala.util.Sorting.quickSort(d)")
      stream.println("d")
      stream.println("}")
    case n@ArrayMap(a,x,blk) => 
      stream.println("// workaround for refinedManifest problem")
      stream.println("val " + quote(sym) + " = {")
      stream.println("val out = " + quote(n.array))
      stream.println("val in = " + quote(a))
      stream.println("var i = 0")
      stream.println("while (i < in.length) {")
      stream.println("val " + quote(x) + " = in(i)")
      emitBlock(blk)
      stream.println("out(i) = " + quote(getBlockResult(blk)))
      stream.println("i += 1")      
      stream.println("}")
      stream.println("out")
      stream.println("}")
    
      // stream.println("val " + quote(sym) + " = " + quote(a) + ".map{")
      // stream.println(quote(x) + " => ")
      // emitBlock(blk)
      // stream.println(quote(getBlockResult(blk)))
      // stream.println("}")  
    case ArrayToSeq(a) => emitValDef(sym, quote(a) + ".toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayOps extends BaseGenArrayOps with CLikeGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case ArrayLength(x) => emitValDef(sym, quote(x) + ".length")
        case ArrayApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
        case ArrayUpdate(x,n,y) => stream.println(quote(x) + ".update(" + quote(n) + "," + quote(y) + ");")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayOps extends CudaGenBase with CLikeGenArrayOps
trait OpenCLGenArrayOps extends OpenCLGenBase with CLikeGenArrayOps
trait CGenArrayOps extends CGenBase with CLikeGenArrayOps

