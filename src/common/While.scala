package scala.lms
package common

import java.io.PrintWriter
import scala.lms.internal.GenericNestedCodegen
import scala.lms.internal.CNestedCodegen
import scala.reflect.SourceContext

trait While extends Base {
  def __whileDo(cond: => Rep[Boolean], body: => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def __doWhile(body: => Rep[Unit], cond: => Rep[Boolean])(implicit pos: SourceContext): Rep[Unit]
}


trait WhileExp extends While with EffectExp {
  case class While(cond: Block[Boolean], body: Block[Unit]) extends Def[Unit]
  case class DoWhile(body: Block[Unit], cond: Block[Boolean]) extends Def[Unit]

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit])(implicit pos: SourceContext) = {
    val c = reifyEffects(cond)
    val a = reifyEffects(body)
    val ce = summarizeEffects(c)
    val ae = summarizeEffects(a)
    reflectEffect(While(c, a), ce andThen ((ae andThen ce).star))
  }

  override def __doWhile(body: => Rep[Unit], cond: => Rep[Boolean])(implicit pos: SourceContext) = {
    val a = reifyEffects(body)
    val c = reifyEffects(cond)
    val ae = summarizeEffects(a)
    val ce = summarizeEffects(c)
    reflectEffect(DoWhile(a, c), ae andThen ((ce andThen ae).star))
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case While(c, b) => syms(c):::syms(b) // wouldn't need to override...
    case DoWhile(b, c) => syms(b):::syms(c) // wouldn't need to override...
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case While(c, b) => effectSyms(c):::effectSyms(b)
    case DoWhile(b, c) => effectSyms(b):::effectSyms(c)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case While(c, b) => freqHot(c):::freqHot(b)
    case DoWhile(b, c) => freqHot(b):::freqHot(c)
    case _ => super.symsFreq(e)
  }


}

trait WhileExpOpt extends WhileExp { this: IfThenElseExp =>

  /** Optimization technique(s):
    * - inversion : This technique changes a standard while loop into a do/while (a.k.a. repeat/until) 
    *               loop wrapped in an if conditional, reducing the number of jumps by two for cases 
    *               where the loop is executed. Doing so duplicates the condition check (increasing the
    *               size of the code) but is more efficient because jumps usually cause a pipeline stall. 
    *               Additionally, if the initial condition is known at compile-time and is known to be 
    *               side-effect-free, the if guard can be skipped.
    */
  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit])(implicit pos: SourceContext) = {
    __ifThenElse(cond, __doWhile(body, cond), ())
  }

}

trait WhileExpOptSpeculative extends WhileExpOpt with PreviousIterationDummyExp { this: IfThenElseExp =>
  
  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit])(implicit pos: SourceContext) = {

    val pc = fresh[Nothing]
    val pb = fresh[Nothing]

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
//<<<<<<< HEAD
      emitValDef(sym, "while ({")
      emitBlock(c)
      stream.println(quote(getBlockResult(c)))
      stream.println("}) {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
/*=======
      //while-do's output is unit, so why do we need to assign its result to a val
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        stream.print("while ({")
        emitBlock(c)
        stream.print(quote(getBlockResult(c)))
        stream.println("}) {")
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.print("}")
      }
      emitValDef(sym, strWriter.toString)*/
    case DoWhile(b,c) =>
      //do-while's output is unit, so why do we need to assign its result to a val
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        stream.print("do {")
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.println("} while ({")
        emitBlock(c)
        stream.print(quote(getBlockResult(c)))
        stream.print("})")
      }
      emitValDef(sym, strWriter.toString)
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
    case DoWhile(b, c) =>
      stream.println("{")
      emitBlock(b)
      stream.println("}")
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
  val IR: WhileExp
  import IR._

  override def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]) = rhs match {
    case While(cond,b) => {
        LIRTraversal(cond)
        LIRTraversal(b)
        sym.atPhase(LIRLowering) {
			val tc = LIRLowering(cond)
            val ce = summarizeEffects(tc)
            val tb = LIRLowering(b)
            val ae = summarizeEffects(tb)
            reflectEffect(While(tc, tb), ce andThen ((ae andThen ce).star)).asInstanceOf[Exp[T]]
        }
    }
    case DoWhile(b,c) =>
        sym.atPhase(LIRLowering) { reflectEffect(DoWhile(runTransformations(b),c)).asInstanceOf[Exp[T]] }
    case _ => super.lowerNode(sym, rhs)
  }
}
