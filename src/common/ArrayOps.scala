package scala.lms
package common

import scala.reflect.SourceContext
import scala.lms.codegen._
import scala.lms.internal._

import java.io.PrintWriter

trait LiftArrays { this: ArrayOps =>
  object Array {
    def apply[T:Typ](xs: Rep[T]*) = array_obj_fromseq(xs)
  }
}


trait ArrayOps extends Variables {

  //implicit def intTyp: Typ[Int] // import
  implicit def seqTyp[T:Typ]: Typ[Seq[T]] // import
  implicit def arrayTyp[T:Typ]: Typ[Array[T]]

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToArrayOps[T:Typ](x: Var[Array[T]])(implicit pos: SourceContext) = new ArrayOpsCls(readVar(x))
  implicit def repArrayToArrayOps[T:Typ](a: Rep[Array[T]]) = new ArrayOpsCls(a)
  implicit def arrayToArrayOps[T:Typ](a: Array[T]) = new ArrayOpsCls(unit(a))

  // substitution for "new Array[T](...)"
  // TODO: look into overriding __new for arrays
  object NewArray {
    def apply[T:Typ](n: Rep[Int]) = array_obj_new[T](n)
  }

  class ArrayOpsCls[T:Typ](a: Rep[Array[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = array_apply(a, n)
    def update(n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = array_update(a,n,y)
    def length(implicit pos: SourceContext) = array_length(a)
    def foreach(block: Rep[T] => Rep[Unit])(implicit pos: SourceContext) = array_foreach(a, block)
    def sort(implicit pos: SourceContext) = array_sort(a)
    def map[B:Typ](f: Rep[T] => Rep[B]) = array_map(a,f)
    def toSeq = array_toseq(a)
    def slice(start:Rep[Int], end:Rep[Int]) = array_slice(a,start,end)
  }

  def array_obj_new[T:Typ](n: Rep[Int]): Rep[Array[T]]
  def array_obj_fromseq[T:Typ](xs: Seq[Rep[T]]): Rep[Array[T]]

  def array_apply[T:Typ](x: Rep[Array[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def array_update[T:Typ](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def array_unsafe_update[T:Typ](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def array_length[T:Typ](x: Rep[Array[T]])(implicit pos: SourceContext) : Rep[Int]
  def array_foreach[T:Typ](x: Rep[Array[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def array_copy[T:Typ](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], len: Rep[Int])(implicit pos: SourceContext): Rep[Unit]
  def array_unsafe_copy[T:Typ](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], len: Rep[Int])(implicit pos: SourceContext): Rep[Unit]
  def array_sort[T:Typ](x: Rep[Array[T]])(implicit pos: SourceContext): Rep[Array[T]]
  def array_map[A:Typ,B:Typ](a: Rep[Array[A]], f: Rep[A] => Rep[B]): Rep[Array[B]]
  def array_toseq[A:Typ](a: Rep[Array[A]]): Rep[Seq[A]]
  def array_slice[A:Typ](a: Rep[Array[A]], start:Rep[Int], end:Rep[Int]): Rep[Array[A]]
}

trait ArrayOpsExp extends ArrayOps with BaseExp with VariablesExp {

  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = {
    implicit val ManifestTyp(m) = typ[T]
    manifestTyp
  }

  case class ArrayNew[T:Typ](n: Exp[Int]) extends Def2[T, Array[T]]
  case class ArrayFromSeq[T:Typ](xs: Seq[Exp[T]]) extends Def2[T, Array[T]]
  case class ArrayApply[T:Typ](a: Exp[Array[T]], n: Exp[Int]) extends Def1[T]
  case class ArrayUpdate[T:Typ](a: Exp[Array[T]], n: Exp[Int], y: Exp[T]) extends Def2[T, Unit]
  case class ArrayLength[T:Typ](a: Exp[Array[T]]) extends Def2[T, Int]
  case class ArrayCopy[T:Typ](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int]) extends Def2[T, Unit]
  case class ArraySort[T:Typ](x: Exp[Array[T]]) extends Def2[T, Array[T]]

  case class ArrayToSeq[A:Typ](x: Exp[Array[A]]) extends Def2[A, Seq[A]]
  case class ArrayChunk[A:Typ](a: Exp[Array[A]], s:Exp[Int], e:Exp[Int]) extends Def2[A, Array[A]]

  case class ArrayForeach[T](a: Exp[Array[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]
  case class ArrayMap[A:Typ,B:Typ](a: Exp[Array[A]], x: Sym[A], block: Block[B])(implicit ctx: SourceContext) extends Def3[A,B,Array[B]] {
    val array = NewArray[B](a.length)
  }

  def array_obj_new[T:Typ](n: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(ArrayNew[T](n))
  def array_obj_fromseq[T:Typ](xs: Seq[Exp[T]])(implicit ctx: SourceContext) = /*reflectMutable(*/ toAtom(ArrayFromSeq(xs)) /*)*/

  def array_apply[T:Typ](x: Exp[Array[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = toAtom(ArrayApply(x, n))
  def array_update[T:Typ](x: Exp[Array[T]], n: Exp[Int], y: Exp[T])(implicit pos: SourceContext) = reflectWrite(x)(ArrayUpdate(x,n,y))
  def array_unsafe_update[T:Typ](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = toAtom(ArrayUpdate(x,n,y))
  def array_length[T:Typ](a: Exp[Array[T]])(implicit pos: SourceContext) : Rep[Int] = toAtom(ArrayLength(a))

  def array_sort[T:Typ](x: Exp[Array[T]])(implicit pos: SourceContext) = toAtom(ArraySort(x))
  def array_copy[T:Typ](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int])(implicit pos: SourceContext) = reflectWrite(dest)(ArrayCopy(src,srcPos,dest,destPos,len))
  def array_unsafe_copy[T:Typ](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int])(implicit pos: SourceContext) = toAtom(ArrayCopy(src,srcPos,dest,destPos,len))

  def array_toseq[A:Typ](a: Exp[Array[A]])(implicit ctx: SourceContext) = toAtom(ArrayToSeq(a))
  def array_slice[A:Typ](a: Rep[Array[A]], start:Rep[Int], end:Rep[Int])(implicit ctx: SourceContext) = toAtom(ArrayChunk(a,start,end))

  def array_foreach[T:Typ](a: Exp[Array[T]], block: Exp[T] => Exp[Unit])(implicit pos: SourceContext): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayForeach(a, x, b), summarizeEffects(b).star)
  }
  def array_map[A:Typ,B:Typ](a: Exp[Array[A]], f: Exp[A] => Exp[B])(implicit ctx: SourceContext) = {
    val x = bound[A]
    val b = reifyEffects(f(x))
    reflectEffect(ArrayMap(a, x, b), summarizeEffects(b))
  }

  //////////////
  // mirroring

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ArrayNew(n) => array_obj_new(f(n))(e.mA,pos)
    case e@ArrayFromSeq(xs) => array_obj_fromseq(f(xs))(e.mA,pos)

    case ArrayApply(a,x) => array_apply(f(a),f(x))(mtype(typ[A]),pos)
    case e@ArrayUpdate(a,n,y) => array_unsafe_update(f(a),f(n),f(y))(e.mA,pos)
    case ArrayLength(x) => array_length(f(x))(mtype(typ[A]),pos)
    case e@ArraySort(x) => array_sort(f(x))(mtype(typ[A]),pos)
    case e@ArrayCopy(a,ap,d,dp,l) => array_copy(f(a),f(ap),f(d),f(dp),f(l))(mtype(typ[A]),pos)

    case e@ArrayToSeq(a) => array_toseq(f(a))(mtype(e.mA),pos)
    case e@ArrayChunk(a,s,t) => array_slice(f(a),f(s),f(t))(mtype(e.mA),pos)

    case e@ArrayForeach(a,x,b) => val b2 = f(b); reflectEffect(ArrayForeach(f(a),x,b2), summarizeEffects(b2).star)
    case e@ArrayMap(a,x,b) => val b2 = f(b); reflectEffect(ArrayMap(f(a),x,b2)(e.mA,e.mB,pos), summarizeEffects(b2))(mtype(e.mR),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

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

trait ArrayOpsExpOpt extends ArrayOpsExp {

  /**
   * @author  Alen Stojanov (astojanov@inf.ethz.ch)
   */
  override def array_length[T:Typ](a: Exp[Array[T]])(implicit pos: SourceContext) : Rep[Int] = a match {
    case Def(EatReflect(ArrayNew(n: Exp[Int]))) => n
    case Def(EatReflect(ArrayFromSeq(xs))) => Const(xs.size)
    case Def(EatReflect(ArraySort(x))) => array_length(x)(mtype(typ[T]),pos)
    case Def(EatReflect(ArrayMap(x, _, _))) => array_length(x.asInstanceOf[Exp[Array[T]]])
    case _ => super.array_length(a)
  }

  override def array_apply[T:Typ](x: Exp[Array[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = {
    if (context ne null) {
      // find the last modification of array x
      // if it is an assignment at index n, just return the last value assigned
      val vs = x.asInstanceOf[Sym[Array[T]]]
      //TODO: could use calculateDependencies?

      val rhs = context.reverse.collectFirst {
        //case w @ Def(Reflect(ArrayNew(sz: Exp[T]), _, _)) if w == x => Some(Const(0)) // FIXME: bounds check!
        case Def(Reflect(ArrayUpdate(`x`, `n`, rhs), _, _)) => Some(rhs.asInstanceOf[Exp[T]])
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.array_apply(x,n))
    } else {
      super.array_apply(x,n)
    }
  }

  override def array_update[T:Typ](x: Exp[Array[T]], n: Exp[Int], y: Exp[T])(implicit pos: SourceContext) = {
    if (context ne null) {
      // find the last modification of array x
      // if it is an assignment at index n with the same value, just do nothing
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

trait BaseGenArrayOps extends GenericCodegen {
  val IR: ArrayOpsExp
  import IR._
}

trait ScalaGenArrayOps extends BaseGenArrayOps with ScalaGenBase {
  val IR: ArrayOpsExp
  import IR._

  val ARRAY_LITERAL_MAX_SIZE = 1000

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n) => emitValDef(sym, src"new Array[${remap(a.mA)}]($n)")
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
          "Array(" + (xs map quote).mkString(",") + ")"
        }
      )
    }
    case ArrayApply(x,n) => emitValDef(sym, src"$x($n)")
    case ArrayUpdate(x,n,y) => emitValDef(sym, src"$x($n) = $y")
    case ArrayLength(x) => emitValDef(sym, src"$x.length")
    case ArrayForeach(a,x,block) =>
      gen"""val $sym = $a.foreach{
           |$x =>
           |${nestedBlock(block)}
           |$block
           |}"""
    case ArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, src"System.arraycopy($src,$srcPos,$dest,$destPos,$len)")
    case a@ArraySort(x) =>
      gen"""val $sym = {
           |val d = new Array[${remap(a.mA)}]($x.length)
           |System.arraycopy($x, 0, d, 0, $x.length)
           |scala.util.Sorting.quickSort(d)
           |d
           |}"""
    case n@ArrayMap(a,x,blk) =>
      gen"""// workaround for refinedManifest problem
           |val $sym = {
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

      // stream.println("val " + quote(sym) + " = " + quote(a) + ".map{")
      // stream.println(quote(x) + " => ")
      // emitBlock(blk)
      // stream.println(quote(getBlockResult(blk)))
      // stream.println("}")
    case ArrayToSeq(a) => emitValDef(sym, src"$a.toSeq")
    case ArrayChunk(a,s,e) => emitValDef(sym, src"$a.slice($s,$e)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayOps extends BaseGenArrayOps with CLikeGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ArrayLength(x) => emitValDef(sym, src"sizeof($x)/sizeof(*$x)") // WARN: statically allocated elements only
        case ArrayApply(x,n) => emitValDef(sym, src"$x[$n]")
        case ArrayUpdate(x,n,y) => stream.println(src"$x.update($n,$y);")
        case ArrayChunk(x,s,e) => val tp=remap(x.tp.typeArguments(0)); emitValDef(sym, src"({ size_t sz=sizeof("+tp+")*($e-$s); "+tp+"* r = ("+tp+"*)malloc(sz); memcpy(r,(("+tp+"*)$x)+$s,sz); r; })")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayOps extends CudaGenBase with CLikeGenArrayOps
trait OpenCLGenArrayOps extends OpenCLGenBase with CLikeGenArrayOps
trait CGenArrayOps extends CGenBase with BaseGenArrayOps {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ArrayLength(x) => emitValDef(sym, quote(x) + "->length")
        case ArrayApply(x,n) => emitValDef(sym, quote(x) + "->apply(" + quote(n) + ")")
        case ArrayUpdate(x,n,y) => stream.println(quote(x) + "->update(" + quote(n) + "," + quote(y) + ");")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

