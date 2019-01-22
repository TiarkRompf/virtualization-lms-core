package scala.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.lms.internal._

trait LiftBoolean {
  this: Base =>

  implicit def boolToBoolRep(b: Boolean) = unit(b)
}

trait BooleanOps extends Variables with Expressions {
  def infix_unary_!(x: Rep[Boolean])(implicit pos: SourceContext) = boolean_negate(x)
  def infix_&&(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext) = boolean_and(lhs,rhs)
  def infix_&&(lhs: Boolean, rhs: => Rep[Boolean])(implicit pos: SourceContext): Exp[Boolean] = {
    if (lhs == true) rhs.asInstanceOf[Exp[Boolean]]
    else Const(false)
  }
  def infix_||(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext) = boolean_or(lhs,rhs)
  def infix_||(lhs: Boolean, rhs: => Rep[Boolean])(implicit pos: SourceContext): Exp[Boolean] = {
    if (lhs == true) Const(true)
    else rhs.asInstanceOf[Exp[Boolean]]
  }

  def boolean_negate(lhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with BaseExp with EffectExp {
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Block[Boolean]) extends Def[Boolean] {
	val c = fresh[Boolean] // used in c code generation
  }
  case class BooleanOr(lhs: Exp[Boolean], rhs: Block[Boolean]) extends Def[Boolean] {
	val c = fresh[Boolean] // used in c code generation
  }

  def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], frhs: => Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = {
    lhs match {
        case x@Const(false) => x
        case x@Const(true) => frhs
        case _ => {
            val rhs = reifyEffects(frhs)
            BooleanAnd(lhs,rhs)
        }
    }
  }
  def boolean_or(lhs: Exp[Boolean], frhs: => Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = {
    lhs match {
        case x@Const(true) => x
        case x@Const(false) => frhs
        case _ => {
			val rhs = reifyEffects(frhs)
			BooleanOr(lhs,rhs)
		}
    }
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case BooleanNegate(x) => boolean_negate(f(x))
    case BooleanAnd(x,y) => toAtom(BooleanAnd(f(x),f(y)))
    case BooleanOr(x,y) => toAtom(BooleanOr(f(x),f(y)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

  override def syms(e: Any): List[Sym[Any]] = e match {
    case BooleanAnd(lhs,rhs) => syms(lhs):::syms(rhs)
    case BooleanOr(lhs,rhs) => syms(lhs):::syms(rhs)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case BooleanAnd(lhs,rhs) => effectSyms(lhs) ::: effectSyms(rhs)
    case BooleanOr(lhs,rhs) => effectSyms(lhs) ::: effectSyms(rhs)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case BooleanAnd(a, x) => freqHot(a):::freqCold(x)
    case BooleanOr(a, x) => freqHot(a):::freqCold(x)
    case _ => super.symsFreq(e)
  }


}

trait BooleanOpsExpOpt extends BooleanOpsExp {
  override def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) = lhs match {
    case Def(BooleanNegate(x)) => x
    case Const(x) => Const(!x)
    case _ => super.boolean_negate(lhs)
  }
  override def boolean_and(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext) = lhs match {
    case Const(cond) => if (cond) rhs else unit(false)
    case _ => super.boolean_and(lhs, rhs)
  }
  override def boolean_or(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext) = lhs match {
    case Const(cond) => if (cond) unit(true) else rhs
    case _ => super.boolean_or(lhs, rhs)
  }
}

trait ScalaGenBooleanOps extends ScalaGenBase with GenericNestedCodegen {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, src"!$b")
    case BooleanAnd(lhs,rhs) =>
		val strWriter = new java.io.StringWriter
		val localStream = new PrintWriter(strWriter);
      	withStream(localStream) {
      	  gen"""if ($lhs == true) {
      	       |${nestedBlock(rhs)}
      	       |$rhs
      	       |} else false"""
      	}
      	emitValDef(sym, strWriter.toString)
    case BooleanOr(lhs,rhs) =>
		val strWriter = new java.io.StringWriter
		val localStream = new PrintWriter(strWriter);
      	withStream(localStream) {
      	  gen"""if ($lhs == false) {
      	       |${nestedBlock(rhs)}
      	       |$rhs
      	       |} else true"""
      	}
      	emitValDef(sym, strWriter.toString)
    case _ => super.emitNode(sym,rhs)
  }
}

trait CLikeGenBooleanOps extends CLikeGenBase with GenericNestedCodegen {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
  	  case BooleanNegate(b) => emitValDef(sym, src"!$b")
      case b@BooleanAnd(lhs,rhs) => {
			emitValDef(b.c, quote(lhs))
        	stream.println("if (" + quote(lhs) + ") {")
			emitBlock(rhs)
    	    stream.println(quote(b.c) + " = " + quote(getBlockResult(rhs)) + ";")
        	stream.println("}")
			emitValDef(sym, quote(b.c))
	  }
      case b@BooleanOr(lhs,rhs) => {
			emitValDef(b.c, quote(lhs))
			stream.println("if (" + quote(lhs) + " == false) {")
			emitBlock(rhs)
			stream.println(quote(b.c) + " = " + quote(getBlockResult(rhs)) + ";")
        	stream.println("}")
			emitValDef(sym, quote(b.c))
	  }
      case _ => super.emitNode(sym,rhs)
    }
  }
}

trait CudaGenBooleanOps extends CudaGenBase with CLikeGenBooleanOps
trait OpenCLGenBooleanOps extends OpenCLGenBase with CLikeGenBooleanOps
trait CGenBooleanOps extends CGenBase with CLikeGenBooleanOps {
  val IR: BooleanOpsExp
  import IR._

  override def lowerNode[A:Manifest](sym: Sym[A], rhs: Def[A]) = rhs match {
	case BooleanAnd(lhs,rhs) => {
		LIRTraversal(rhs)
		sym.atPhase(LIRLowering) {
			reflectEffect(BooleanAnd(LIRLowering(lhs),LIRLowering(rhs))).asInstanceOf[Exp[A]]
		}
	}
	case BooleanOr(lhs,rhs) => {
		LIRTraversal(rhs)
		sym.atPhase(LIRLowering) {
			reflectEffect(BooleanOr(LIRLowering(lhs),LIRLowering(rhs))).asInstanceOf[Exp[A]]
		}
	}
	case _ => super.lowerNode(sym,rhs)
  }
}
