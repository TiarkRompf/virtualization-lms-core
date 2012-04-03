package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen

trait While extends Base {
  def __whileDo(cond: => Rep[Boolean], body: => Rep[Unit])
}


trait WhileExp extends While with EffectExp {
  case class While(cond: Block[Boolean], body: Block[Unit]) extends Def[Unit]

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {
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


}


trait WhileExpOptSpeculative extends WhileExp with PreviousIterationDummyExp {
  
  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {

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




trait CudaGenWhile extends CudaGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case While(c,b) =>
            // Get free variables list
            //val freeVars = getFreeVarBlock(c,Nil)

            // emit function for the condition evaluation
            val (condFunc,freeVars) = emitDevFunc(c, Nil)
            val argListStr = freeVars.map(quote(_)).mkString(", ")

            // Emit while loop (only the result variable of condition)
            stream.print(addTab() + "while (")
            stream.print("%s(%s)".format(condFunc,argListStr))
            stream.println(") {")
            tabWidth += 1
            emitBlock(b)
            tabWidth -= 1
            //stream.println(quote(getBlockResult(b)))   //TODO: Is this needed?
            stream.println(addTab() + "}")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait OpenCLGenWhile extends OpenCLGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case While(c,b) =>
        // calculate condition
        emitBlock(c)
        stream.println("bool cond_%s = %s;".format(quote(sym),quote(getBlockResult(c))))
        // Emit while loop
        stream.print("while (cond_%s) {".format(quote(sym)))
        emitBlock(b)
        stream.println("}")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CGenWhile extends CGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case While(c,b) =>
        // calculate condition
        emitBlock(c)
        stream.println("bool cond_%s = %s;".format(quote(sym),quote(getBlockResult(c))))
        // Emit while loop
        stream.print("while (cond_%s) {".format(quote(sym)))
        emitBlock(b)
        stream.println("}")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
