package scala.lms
package common

import java.io.PrintWriter
import scala.lms.internal.GenericNestedCodegen
import scala.reflect.SourceContext

trait While extends Base {
  def __whileDo(cond: => Rep[Boolean], body: => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
}


trait WhileExp extends While with BooleanOps with EffectExp {
  case class While(cond: Block[Boolean], body: Block[Unit]) extends Def[Unit]

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit])(implicit pos: SourceContext) = {
    val c = reifyEffects(cond)
    val a = reifyEffects(body)
    val ce = summarizeEffects(c)
    val ae = summarizeEffects(a)
    reflectEffect(While(c, a), ce andThen ((ae andThen ce).star))
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case While(c, b) => syms(c):::syms(b) // wouldn't need to override...
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case While(c, b) => effectSyms(c):::effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case While(c, b) => freqHot(c):::freqHot(b)
    case _ => super.symsFreq(e)
  }

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case While(a,b) => While(f(a), f(b))
    case _ => super.mirrorDef(e,f)
  }

  override def mirror[A:Typ](d: Def[A], f: Transformer) (implicit pos: SourceContext): Exp[A] = (d match {
    case Reflect(While(a, b), u, es) =>
      if (f.hasContext)
        __whileDo(f.reflectBlock(a), f.reflectBlock(b))
      else
        reflectMirrored(Reflect(While(f(a), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case While(a, b) =>
      if (f.hasContext)
        __whileDo(f.reflectBlock(a), f.reflectBlock(b))
      else
        While(f(a), f(b))
    case _ => super.mirror(d, f)
  }).asInstanceOf[Exp[A]]

}


trait WhileExpOptSpeculative extends WhileExp with PreviousIterationDummyExp {
  
  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit])(implicit pos: SourceContext) = {

    val pc = fresh[Unit]
    val pb = fresh[Unit]

    val c = reifyEffectsHere(cond)
    val ce = summarizeEffects(c)
    val a = reifyEffectsHere { reflectPreviousDummy(pc,ce); body }
    val ae = summarizeEffects(a)
    
    val c1 = reifyEffectsHere { reflectPreviousDummy(pb,ae); cond }
    val ce1 = summarizeEffects(c1)
    val a1 = reifyEffectsHere { reflectPreviousDummy(pc,ce1); body }
    val ae1 = summarizeEffects(a1)

    val c2 = reifyEffectsHere { reflectPreviousDummy(pb,ae1/*.lastIteration*/); cond }
    val ce2 = summarizeEffects(c2)
    val a2 = reifyEffectsHere { reflectPreviousDummy(pc,ce2); body }
    val ae2 = summarizeEffects(a2)
  
    assert(ae2 == ae1, "not converged: " + ae1 + " != " + ae2)
      
    val cr = c2
    val ar = a2
    val cer = ce2
    val aer = ae2
    
/*  
    val c = reifyEffects(cond)
    val a = reifyEffects(body)
    val ce = summarizeEffects(c)
    val ae = summarizeEffects(a)
*/
    reflectEffect(While(cr, ar), cer andThen ((aer andThen cer).star))
  }

}


trait BaseGenWhile extends GenericNestedCodegen {
  val IR: WhileExp
  import IR._
}

trait ScalaGenWhile extends ScalaGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case While(c,b) =>
      stream.print("val " + quote(sym) + " = while ({")
      emitBlock(c)
      stream.print(quote(getBlockResult(c)))
      stream.println("}) {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}


trait ScalaGenWhileOptSpeculative extends ScalaGenWhile with ScalaGenPreviousIterationDummy {
  val IR: WhileExpOptSpeculative
}


trait CLikeGenWhile extends CLikeGenBase with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case While(c,b) =>
      stream.println("for (;;) {")
      emitBlock(c)
      stream.println("if (!"+quote(getBlockResult(c))+") break;")
      emitBlock(b)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenWhile extends CudaGenEffect with CLikeGenWhile

trait OpenCLGenWhile extends OpenCLGenEffect with CLikeGenWhile

trait CGenWhile extends CGenEffect with CLikeGenWhile {

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case While(c,b) =>
      stream.println("while (1) {")
      emitBlock(c)
      stream.println("if (!"+quote(getBlockResult(c))+") break;")
      emitBlock(b)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}
