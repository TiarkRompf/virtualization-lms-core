package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.collection.mutable.Set
import scala.reflect.SourceContext

trait SetOps extends Base with Variables {
  object Set {
    def apply[A:Manifest](xs: Rep[A]*)(implicit pos: SourceContext) = set_new[A](xs)
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
    def foreach(block: Rep[A] => Rep[Unit])(implicit pos: SourceContext) = set_foreach(s, block)
  }

  def set_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext) : Rep[Set[A]]
  def set_contains[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Boolean]
  def set_add[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Unit]
  def set_remove[A:Manifest](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Boolean]
  def set_size[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Int]
  def set_clear[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Unit]
  def set_toseq[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def set_toarray[A:Manifest](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def set_foreach[T:Manifest](x: Rep[Set[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
}

trait SetOpsExp extends SetOps with ArrayOps with EffectExp {
  case class SetNew[A:Manifest](xs: Seq[Exp[A]], mA: Manifest[A]) extends Def[Set[A]]
  case class SetContains[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Boolean]
  case class SetAdd[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetRemove[A:Manifest](s: Exp[Set[A]], i: Exp[A]) extends Def[Boolean]
  case class SetSize[A:Manifest](s: Exp[Set[A]]) extends Def[Int]
  case class SetClear[A:Manifest](s: Exp[Set[A]]) extends Def[Unit]
  case class SetToSeq[A:Manifest](s: Exp[Set[A]]) extends Def[Seq[A]]
  case class SetToArray[A:Manifest](s: Exp[Set[A]]) extends Def[Array[A]] {
    //val array = unit(manifest[A].newArray(0))
    val array = NewArray[A](s.size)
  }
  case class SetForeach[T](a: Exp[Set[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]

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
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case SetForeach(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SetForeach(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case SetForeach(a, x, body) => freqNormal(a):::freqHot(body)
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
    case SetNew(xs, mA) => emitValDef(sym, "collection.mutable.HashSet[" + remap(mA) + "](" + (xs map {quote}).mkString(",") + ")")
    case SetContains(s,i) => emitValDef(sym, quote(s) + ".contains(" + quote(i) + ")")
    case SetAdd(s,i) => emitValDef(sym, quote(s) + ".add(" + quote(i) + ")")
    case SetRemove(s,i) => emitValDef(sym, quote(s) + ".remove(" + quote(i) + ")")
    case SetSize(s) => emitValDef(sym, quote(s) + ".size")
    case SetClear(s) => emitValDef(sym, quote(s) + ".clear()")
    case SetToSeq(s) => emitValDef(sym, quote(s) + ".toSeq")
    case n@SetToArray(s) => //emitValDef(sym, quote(s) + ".toArray")
      emitValDef(sym, "{ // workaround for refinedManifest problem" +
      "\nval out = " + quote(n.array) +
      "\nval in = " + quote(s) + ".toSeq" +
      "\nvar i = 0" +
      "\nwhile (i < in.length) {" +
      "\nout(i) = in(i)" +
      "\ni += 1" +
      "\n}" +
      "\nout" +
      "\n}")
    case SetForeach(a,x,block) => 
      emitValDef(sym, quote(a) + ".foreach{")    
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSetOps extends BaseGenSetOps with CLikeCodegen {
  val IR: SetOpsExp
  import IR._

//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
}

trait CudaGenSetOps extends CudaGenEffect with CLikeGenSetOps
trait OpenCLGenSetOps extends OpenCLGenEffect with CLikeGenSetOps
trait CGenSetOps extends CGenEffect with CLikeGenSetOps
