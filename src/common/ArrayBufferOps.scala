package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.internal.GenericNestedCodegen
import collection.mutable.ArrayBuffer
import scala.reflect.SourceContext

trait ArrayBufferOps extends Base with Variables {

  object ArrayBuffer {
    def apply[A:Manifest](xs: Rep[A]*) = arraybuffer_new(xs)
  }

  implicit def repToArrayBufferOps[A:Manifest](l: Rep[ArrayBuffer[A]]) = new ArrayBufferOpsCls(l)
  implicit def varToArrayBufferOps[A:Manifest](l: Var[ArrayBuffer[A]]) = new ArrayBufferOpsCls(readVar(l))
  
  class ArrayBufferOpsCls[A:Manifest](l: Rep[ArrayBuffer[A]]) {
	def apply(e: Rep[Int])(implicit pos: SourceContext) = arraybuffer_apply(l,e)
    def append(e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l,e)
    def -=(e: Rep[Int])(implicit pos: SourceContext) = arraybuffer_remove(l,e)
    def mkString(sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l,sep)
    def foreach(block: Rep[A] => Rep[Unit])(implicit pos: SourceContext) = arraybuffer_foreach(l, block)
    def toArray(implicit pos: SourceContext) = arraybuffer_toarray(l)
    def toSeq(implicit pos: SourceContext) = arraybuffer_toseq(l)
    def size(implicit pos: SourceContext) = arraybuffer_size(l)
    def clear(implicit pos: SourceContext) = arraybuffer_clear(l)
    def indexWhere(f: Rep[A] => Rep[Boolean])(implicit pos: SourceContext) = arraybuffer_indexwhere(l,f)
	def minBy[B:Manifest](f: Rep[A] => Rep[B])(implicit pos: SourceContext) = arraybuffer_minBy(l,f)
	def foldLeft[B:Manifest](z: Rep[B])(f: (Rep[B], Rep[A]) => Rep[B]) = arraybuffer_foldLeft(l, z)(f)
  }
  
//  def infix_+=[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)

  /* when mixed in with OptiML, one of these infix operations causes an NPE in the scala-virtualized compiler */ //TR: still the case?
  /*
  def infix_mkString[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l, sep)
  def infix_+=[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
  def infix_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
  def infix_toArray[A:Manifest](l: Rep[ArrayBuffer[A]])(implicit pos: SourceContext) = arraybuffer_toarray(l)
  def infix_toSeq[A:Manifest](l: Rep[ArrayBuffer[A]])(implicit pos: SourceContext) = arraybuffer_toseq(l)
  */
  def arraybuffer_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[ArrayBuffer[A]]
  def arraybuffer_apply[A:Manifest](l: Rep[ArrayBuffer[A]], i: Rep[Int])(implicit pos: SourceContext): Rep[A] 
  def arraybuffer_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_remove[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[Int])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_mkstring[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit pos: SourceContext): Rep[String]
  def arraybuffer_foreach[A:Manifest](x: Rep[ArrayBuffer[A]], block: Rep[A] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_toarray[A:Manifest](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def arraybuffer_toseq[A:Manifest](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def arraybuffer_size[A:Manifest](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Int]
  def arraybuffer_clear[A:Manifest](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_indexwhere[A:Manifest](x: Rep[ArrayBuffer[A]], block: Rep[A] => Rep[Boolean])(implicit pos: SourceContext): Rep[Int]
  def arraybuffer_minBy[A:Manifest,B:Manifest](x: Rep[ArrayBuffer[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[A]
  def arraybuffer_foldLeft[A:Manifest,B:Manifest](x: Rep[ArrayBuffer[A]], z: Rep[B])(f: (Rep[B], Rep[A]) => Rep[B]): Rep[B]
}

trait ArrayBufferOpsExp extends ArrayBufferOps with EffectExp with VariablesExp with FunctionsExp {
  case class ArrayBufferNew[A:Manifest](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    val mA = manifest[A]
  }
  case class ArrayBufferApply[A:Manifest](x: Exp[ArrayBuffer[A]], e: Exp[Int]) extends Def[A] {
	val mA = manifest[A]
  }
  case class ArrayBufferAppend[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit] {
	val mA = manifest[A]
  }
  case class ArrayBufferRemove[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[Int]) extends Def[Unit] {
	val mA = manifest[A]
  }
  case class ArrayBufferMkString[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class ArrayBufferForeach[A:Manifest](a: Exp[ArrayBuffer[A]], x: Sym[A], block: Block[Unit]) extends Def[Unit] 
  case class ArrayBufferToArray[A:Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Array[A]]
  case class ArrayBufferToSeq[A:Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Seq[A]]
  case class ArrayBufferSize[A:Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Int] {
	val mA = manifest[A]
  }
  case class ArrayBufferClear[A:Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Unit]
  case class ArrayBufferIndexWhere[A:Manifest](a: Exp[ArrayBuffer[A]], k: Sym[A], block: Block[Boolean]) extends Def[Int] {
	val mA = manifest[A]
	val idx = fresh[Int]; val elem = fresh[A] // Used in the c code generation
  }
  case class ArrayBufferMinBy[A:Manifest,B:Manifest](a: Exp[ArrayBuffer[A]], x: Sym[A], block: Block[B]) extends Def[A] {
	val mA = manifest[A]
	val mB = manifest[B]
	val idx = fresh[Int]; val min = fresh[B] // Used in the c code generation
  }
  case class ArrayBufferFoldLeft[A:Manifest,B:Manifest](a: Exp[ArrayBuffer[A]], z: Exp[B], i: Sym[B], e: Sym[A], block: Block[B]) extends Def[B] {
	val mA = manifest[A]
	val mB = manifest[B]
	val idx = fresh[Int] // Used in the c code generation
  }

  def arraybuffer_new[A:Manifest](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_apply[A:Manifest](l: Rep[ArrayBuffer[A]], i: Rep[Int])(implicit pos: SourceContext) = ArrayBufferApply(l,i)
  def arraybuffer_append[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferAppend(l, e))
  def arraybuffer_remove[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[Int])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferRemove(l, e))
  def arraybuffer_mkstring[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit pos: SourceContext) = ArrayBufferMkString(l, sep)
  def arraybuffer_foreach[A:Manifest](x: Exp[ArrayBuffer[A]], block: Rep[A] => Rep[Unit])(implicit pos: SourceContext) = {
    val k = fresh[A]
    val b = reifyEffects(block(k))
	reflectEffect(ArrayBufferForeach(x, k, b), summarizeEffects(b).star)
  }
  def arraybuffer_toarray[A:Manifest](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToArray(x)
  def arraybuffer_toseq[A:Manifest](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToSeq(x)
  def arraybuffer_size[A:Manifest](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferSize(x)
  def arraybuffer_clear[A:Manifest](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferClear(x)
  def arraybuffer_indexwhere[A:Manifest](x: Exp[ArrayBuffer[A]], block: Rep[A] => Rep[Boolean])(implicit pos: SourceContext) = {
    val k = fresh[A]
    val b = reifyEffects(block(k))
	reflectEffect(ArrayBufferIndexWhere(x, k, b), summarizeEffects(b).star)
  }
  def arraybuffer_minBy[A:Manifest,B:Manifest](x: Exp[ArrayBuffer[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext) = {
    val k = fresh[A]
    val b = reifyEffects(f(k))
	reflectEffect(ArrayBufferMinBy(x, k, b)(manifest[A],manifest[B]), summarizeEffects(b).star)
  }
  def arraybuffer_foldLeft[A:Manifest,B:Manifest](x: Exp[ArrayBuffer[A]], z: Exp[B])(f: (Exp[B], Exp[A]) => Exp[B]) = {
	val e = fresh[A]
	val i = fresh[B]	
	val b = reifyEffects(f(i,e))
	reflectEffect(ArrayBufferFoldLeft(x,z,i,e,b), summarizeEffects(b).star)
  }

  //////////////
  // mirroring

  /*override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayBufferMkString(l,r) => ArrayBufferMkString(f(l),f(r))
    case ArrayBufferAppend(l,r) => ArrayBufferAppend(f(l),f(r))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??*/
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(a@ArrayBufferNew(b), u, es) => reflectMirrored(Reflect(ArrayBufferNew(f(b))(a.mA), mapOver(f,u), f(es)))
    case Reflect(a@ArrayBufferAppend(ar,v), u, es) => reflectMirrored(Reflect(ArrayBufferAppend(f(ar),f(v))(a.mA), mapOver(f,u), f(es)))
    case Reflect(a@ArrayBufferApply(ar,v), u, es) => reflectMirrored(Reflect(ArrayBufferApply(f(ar),f(v))(a.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(a@ArrayBufferToArray(ar), u, es) => reflectMirrored(Reflect(ArrayBufferToArray(f(ar)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(a@ArrayBufferRemove(ar,v), u, es) => reflectMirrored(Reflect(ArrayBufferRemove(f(ar),f(v))(a.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(a@ArrayBufferSize(ar), u, es) => reflectMirrored(Reflect(ArrayBufferSize(f(ar))(a.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(a@ArrayBufferIndexWhere(m,k,v), u, es) => reflectMirrored(Reflect(ArrayBufferIndexWhere(f(m),k,f(v))(a.mA), mapOver(f,u), f(es)))
    case Reflect(a@ArrayBufferMinBy(m,k,v), u, es) => 
		reflectMirrored(Reflect(ArrayBufferMinBy(f(m).asInstanceOf[Exp[scala.collection.mutable.ArrayBuffer[A]]],k,f(v))(a.mA.asInstanceOf[Manifest[A]],a.mB), mapOver(f,u), f(es)))
    case Reflect(afl@ArrayBufferFoldLeft(a,z,i,e,b), u, es) => 
		reflectMirrored(Reflect(ArrayBufferFoldLeft(f(a),f(z),i,e,f(b))(afl.mA,afl.mB.asInstanceOf[Manifest[A]]), mapOver(f,u), f(es)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
 
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayBufferForeach(m, k, v) => syms(m):::syms(v)
    case ArrayBufferMinBy(m, k, v) => syms(m):::syms(v)
    case ArrayBufferFoldLeft(m, z, i, e, v) => syms(m):::syms(v)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayBufferForeach(m, k, v) => k :: effectSyms(v)
    case ArrayBufferIndexWhere(m, k, v) => k :: effectSyms(m) ::: effectSyms(v)
    case ArrayBufferMinBy(m, k, v) => k :: effectSyms(v)
    case ArrayBufferFoldLeft(m, z, i, e, v) => i :: e :: effectSyms(v)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayBufferForeach(m, k, v) => freqNormal(m) ::: freqHot(v)
    case ArrayBufferMinBy(m, k, v) => freqNormal(m) ::: freqHot(v)
    case ArrayBufferFoldLeft(m, z, i, e, v) => freqNormal(m) ::: freqHot(v)
    case _ => super.symsFreq(e)
  }  

}

trait BaseGenArrayBufferOps extends GenericNestedCodegen {
  val IR: ArrayBufferOpsExp
  import IR._
}

trait ScalaGenArrayBufferOps extends BaseGenArrayBufferOps with ScalaGenEffect {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  	case a@ArrayBufferApply(x, i) => emitValDef(sym, quote(x) + "(" + quote(i) + ")")
    case a@ArrayBufferNew(xs) =>
		emitValDef(sym, "scala.collection.mutable.ArrayBuffer[" + remap(a.mA) + "](" + (xs map quote).mkString(",") + ")")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, src"$l.mkString($sep)")
    case ArrayBufferAppend(l, e) => emitValDef(sym, src"$l += $e")
    case ArrayBufferForeach(l, e, b) =>
		emitValDef(sym, quote(l) + ".foreach(" + quote(e) + "=> {")
        emitBlock(b)
        emitBlockResult(b)
        stream.println("})")
    case ArrayBufferRemove(l, e) => emitValDef(sym, src"$l.remove($e)")
    case ArrayBufferToArray(x) => emitValDef(sym, src"$x.toArray")
    case ArrayBufferToSeq(x) => emitValDef(sym, src"$x.toSeq")
    case ArrayBufferSize(x) => emitValDef(sym, src"$x.size")
    case ArrayBufferClear(x) => emitValDef(sym, src"$x.clear")
    case ArrayBufferIndexWhere(l, e, b) =>
	 	emitValDef(sym, quote(l) + ".indexWhere(" + quote(e) + "=> {")
        emitBlock(b)
        emitBlockResult(b)
    	stream.println("})")
	case ArrayBufferMinBy(l, e, b) =>
	 	emitValDef(sym, quote(l) + ".minBy(" + quote(e) + "=> {")
        emitBlock(b)
        emitBlockResult(b)
    	stream.println("})")
	case ArrayBufferFoldLeft(l, z, i, e, b) =>
		emitValDef(i, quote(z))
	 	emitValDef(sym, quote(l) + ".foldLeft(" + quote(i) + ")((" + quote(i) + "," + quote(e) + ") => {")
        emitBlock(b)
        emitBlockResult(b)
    	stream.println("})")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayBufferOps extends BaseGenArrayBufferOps with CLikeGenBase {
  val IR: ArrayBufferOpsExp
  import IR._

  override def remap[A](m: Manifest[A]) = m match {
    case s if s <:< manifest[ArrayBuffer[Any]] => "GArray*"
    case _ => super.remap(m)
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
		case a@ArrayBufferNew(xs) => 
			emitValDef(sym, "g_array_new(NULL, TRUE, sizeof(" + remap(a.mA) + "))")
  		case a@ArrayBufferApply(x, i) => 
			emitValDef(sym, "g_array_index(" + quote(x) + "," + remap(a.mA) + ", " + quote(i) + ")")
    	case ArrayBufferAppend(l, e) =>
			emitValDef(sym, "g_array_append_val(" + quote(l) + "," + quote(e) + ")")
	    case ArrayBufferSize(x) => emitValDef(sym, quote(x) + "->len")
    	case iw@ArrayBufferIndexWhere(a, e, b) =>
			// TODO: Yannis: This could be done much better through lowering. Volunteers ? :-)
			emitValDef(iw.idx, "0")
			emitValDef(sym, "-1")
			stream.println("for (;" + quote(iw.idx) + "<" + quote(a) + "->len;" + quote(iw.idx) + "+=1) {")
			emitValDef(e, "g_array_index(" + quote(a) + "," + remap(iw.mA) + "," + quote(iw.idx) + ");")
			emitBlock(b)
			stream.println("if (" + quote(getBlockResult(b)) + " == true) {")
			stream.println(quote(sym) + " = " + quote(iw.idx) + ";")
			stream.println("break;")
			stream.println("}")
			stream.println("}")
		case mb@ArrayBufferMinBy(a,e,b) =>
			// TODO: Yannis: This as well could be done much better through lowering. Volunteers ? :-)
			// Set minimum to first element
			emitValDef(e, "g_array_index(" + quote(a) + "," + remap(mb.mA) + ", 0);")
			emitBlock(b)
			emitValDef(mb.min, quote(getBlockResult(b)))
			emitValDef(sym, quote(e))
			// Iterate rest elements and update min if necessary
			emitValDef(mb.idx, "1")
			stream.println("for (;" + quote(mb.idx) + "<" + quote(a) + "->len;" + quote(mb.idx) + "+=1) {")
			emitValDef(e, "g_array_index(" + quote(a) + "," + remap(mb.mA) + "," + quote(mb.idx) + ");")
			emitBlock(b)
			stream.println("if (" + quote(getBlockResult(b)) + " < " + quote(mb.min) + ") {")
			stream.println(quote(sym) + " = " + quote(e) + ";")
			stream.println("}")
			stream.println("}")
		case afl@ArrayBufferFoldLeft(a,z,i,e,b) =>
			// TODO: Yannis: And finally the same goes here -- this could be done much better through lowering. Volunteers ? :-)
            emitValDef(i, quote(z))
			emitValDef(afl.idx, "0")
			stream.println("for (;" + quote(afl.idx) + "<" + quote(a) + "->len;" + quote(afl.idx) + "+=1) {")
			emitValDef(e, "g_array_index(" + quote(a) + "," + remap(afl.mA) + "," + quote(afl.idx) + ");")
			emitBlock(b)
            stream.println(quote(i) + " = " + quote(getBlockResult(b)) + ";")
			stream.println("}")
            emitValDef(sym,quote(i))
        case abr@ArrayBufferRemove(a,idx) => stream.println("g_array_remove_index(" + quote(a) + "," + quote(idx) + ");")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayBufferOps extends CudaGenEffect with CLikeGenArrayBufferOps
trait OpenCLGenArrayBufferOps extends OpenCLGenEffect with CLikeGenArrayBufferOps
trait CGenArrayBufferOps extends CGenEffect with CLikeGenArrayBufferOps {
  val IR: ArrayBufferOpsExp
  import IR._

  override def lowerNode[A:Manifest](sym: Sym[A], rhs: Def[A]) = rhs match {
	case a@ArrayBufferNew(xs) => sym.atPhase(LIRLowering) { arraybuffer_new(xs)(a.mA,implicitly[SourceContext]).asInstanceOf[Exp[A]] }
	case aba@ArrayBufferApply(a,e) => sym.atPhase(LIRLowering) { 
        val lar = LIRLowering(a)
        arraybuffer_apply(lar,LIRLowering(e))(aba.mA,implicitly[SourceContext]).asInstanceOf[Exp[A]] 
    }
	case abr@ArrayBufferRemove(a,idx) => sym.atPhase(LIRLowering) { 
        val lar = LIRLowering(a)
        arraybuffer_remove(lar,LIRLowering(idx))(abr.mA,implicitly[SourceContext]).asInstanceOf[Exp[A]] 
    }
	case iw@ArrayBufferIndexWhere(a,k,c) => 
		LIRTraversal(c)
		sym.atPhase(LIRLowering) { 
			val lar = LIRLowering(a)
			reflectEffect(ArrayBufferIndexWhere(lar,k,LIRLowering(c))(iw.mA)).asInstanceOf[Exp[A]] 
		}
	case mb@ArrayBufferMinBy(a,k,c) => 
		LIRTraversal(c)
		sym.atPhase(LIRLowering) { 
			val lar = LIRLowering(a).asInstanceOf[Exp[scala.collection.mutable.ArrayBuffer[A]]]
			reflectEffect(ArrayBufferMinBy(lar,k,LIRLowering(c))(mb.mA.asInstanceOf[Manifest[A]],mb.mB)).asInstanceOf[Exp[A]] 
		}
	case afl@ArrayBufferFoldLeft(a,z,i,e,b) => 
		LIRTraversal(b)
		sym.atPhase(LIRLowering) { 
			val lar = LIRLowering(a).asInstanceOf[Exp[scala.collection.mutable.ArrayBuffer[Any]]]
			reflectEffect(ArrayBufferFoldLeft(lar,LIRLowering(z),i,e,LIRLowering(b))(afl.mA,afl.mB.asInstanceOf[Manifest[A]])).asInstanceOf[Exp[A]] 
		}
	case _ => super.lowerNode(sym,rhs)
  }
}

