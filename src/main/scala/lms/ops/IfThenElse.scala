package scala.lms
package ops

import internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}

import java.io.PrintWriter
import scala.reflect.SourceContext

trait IfThenElse extends Base {
  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext): Rep[T]

  // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: =>Boolean, thenp: => T, elsep: => T) = cond match {
    case true => thenp
    case false => elsep
  }
}

// TODO: it would be nice if IfThenElseExp would extend IfThenElsePureExp
// but then we would need to use different names.

trait IfThenElsePureExp extends IfThenElse with BaseExp {

  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = IfThenElse(cond, thenp, elsep)
}


trait IfThenElseExp extends IfThenElse with EffectExp {

  abstract class AbstractIfThenElse[T] extends Def[T] {
    val cond: Exp[Boolean]
    val thenp: Block[T]
    val elsep: Block[T]
  }

  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Block[T], elsep: Block[T]) extends AbstractIfThenElse[T]

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = {
    val a = reifyEffectsHere(thenp)
    val b = reifyEffectsHere(elsep)

    ifThenElse(cond,a,b)
  }

  def ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: Block[T], elsep: Block[T])(implicit pos: SourceContext) = {
    val ae = summarizeEffects(thenp)
    val be = summarizeEffects(elsep)

    // TODO: make a decision whether we should call reflect or reflectInternal.
    // the former will look for any read mutable effects in addition to the passed
    // summary whereas reflectInternal will take ae orElse be literally.
    // the case where this comes up is if (c) a else b, with a or b mutable.
    // (see TestMutation, for now sticking to old behavior)

    ////reflectEffect(IfThenElse(cond,thenp,elsep), ae orElse be)
    reflectEffectInternal(IfThenElse(cond,thenp,elsep), ae orElse be)
  }

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case IfThenElse(c,a,b) => IfThenElse(f(c),f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), u, es) =>
      if (f.hasContext)
        __ifThenElse(f(c),f.reflectBlock(a),f.reflectBlock(b))
      else
        reflectMirrored(Reflect(IfThenElse(f(c),f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case IfThenElse(c,a,b) =>
      if (f.hasContext)
        __ifThenElse(f(c),f.reflectBlock(a),f.reflectBlock(b))
      else
        IfThenElse(f(c),f(a),f(b)) // FIXME: should apply pattern rewrites (ie call smart constructor)
    case _ => super.mirror(e,f)
  }

/*
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), u, es) => mirror(IfThenElse(c,a,b)) // discard reflect
    case IfThenElse(c,a,b) => ifThenElse(f(c),f(a),f(b)) // f.apply[A](a: Block[A]): Exp[A] mirrors the block into the current context
    case _ => super.mirror(e,f)
  }
*/



  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }


  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case IfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }

/*
  override def coldSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => syms(t) ++ syms(e)
    case _ => super.coldSyms(e)
  }
*/

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }

}

trait IfThenElseFatExp extends IfThenElseExp with BaseFatExp {

  abstract class AbstractFatIfThenElse extends FatDef {
    val cond: Exp[Boolean]
    val thenp: List[Block[Any]]
    val elsep: List[Block[Any]]

    var extradeps: List[Exp[Any]] = Nil //HACK
  }

  case class SimpleFatIfThenElse(cond: Exp[Boolean], thenp: List[Block[Any]], elsep: List[Block[Any]]) extends AbstractFatIfThenElse

/* HACK */

  override def syms(e: Any): List[Sym[Any]] = e match {
    case x@SimpleFatIfThenElse(c, t, e) => super.syms(x) ++ syms(x.extradeps)
    case _ => super.syms(e)
  }

/* END HACK */


  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case x@SimpleFatIfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)    ++ freqNormal(x.extradeps)
    case _ => super.symsFreq(e)
  }

  // aliasing / sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }
}


trait IfThenElseExpOpt extends IfThenElseExp { this: BooleanOpsExp with EqualExpBridge =>

  //TODO: eliminate conditional if both branches return same value!
  // Note: this is only correct since the condition has no side effects
  // (effects are done before the if-then-else)

  // it would be nice to handle rewrites in method ifThenElse but we'll need to
  // 'de-reify' blocks in case we rewrite if(true) to thenp.
  // TODO: make reflect(Reify(..)) do the right thing

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case Def(BooleanNegate(a)) => __ifThenElse(a, elsep, thenp)
    case Def(NotEqual(a,b)) => __ifThenElse(equals(a,b), elsep, thenp)
    case _ =>
      super.__ifThenElse(cond, thenp, elsep)
  }
}







trait BaseGenIfThenElse extends GenericNestedCodegen {
  val IR: IfThenElseExp
  import IR._

}

trait BaseGenIfThenElseFat extends BaseGenIfThenElse with GenericFatCodegen {
  val IR: IfThenElseFatExp
  import IR._

  override def fatten(e: Stm): Stm = e match {
    case TP(sym, o: AbstractIfThenElse[_]) =>
      TTP(List(sym), List(o), SimpleFatIfThenElse(o.cond, List(o.thenp), List(o.elsep)))
    case TP(sym, p @ Reflect(o: AbstractIfThenElse[_], u, es)) => //if !u.maySimple && !u.mayGlobal =>  // contrary, fusing will not change observable order
      // assume body will reflect, too...
      printdbg("-- fatten effectful if/then/else " + e)
      val e2 = SimpleFatIfThenElse(o.cond, List(o.thenp), List(o.elsep))
      e2.extradeps = es //HACK
      TTP(List(sym), List(p), e2)
    case _ => super.fatten(e)
  }
}


trait ScalaGenIfThenElse extends ScalaGenEffect with BaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenIfThenElseFat extends ScalaGenIfThenElse with ScalaGenFat with BaseGenIfThenElseFat {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatIfThenElse(c,as,bs) =>
      def quoteList[T](xs: List[Exp[T]]) = if (xs.length > 1) xs.map(quote).mkString("(",",",")") else xs.map(quote).mkString(",")
      if (symList.length > 1) stream.println("// TODO: use vars instead of tuples to return multiple values")
      stream.println("val " + quoteList(symList) + " = if (" + quote(c) + ") {")
      emitFatBlock(as)
      stream.println(quoteList(as.map(getBlockResult)))
      stream.println("} else {")
      emitFatBlock(bs)
      stream.println(quoteList(bs.map(getBlockResult)))
      stream.println("}")
    case _ => super.emitFatNode(symList, rhs)
  }

}
