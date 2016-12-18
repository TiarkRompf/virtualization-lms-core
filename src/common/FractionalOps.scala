package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.GenericNestedCodegen

trait FractionalOps extends ImplicitOps {
  def infix_/[A,T](lhs: Rep[T], rhs: Rep[A])(implicit c: A => T, f: Fractional[T], mA: Manifest[A], mT: Manifest[T], pos: SourceContext) = fractional_divide(lhs,implicit_convert[A,T](rhs))

  def fractional_divide[T:Fractional:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
}

trait FractionalOpsExp extends FractionalOps with ImplicitOpsExp {
  
  case class FractionalDivide[T:Fractional:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[T] {
	val mT = manifest[T]
    val fr = implicitly[Fractional[T]]
  }

  def fractional_divide[T:Fractional:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Rep[T] = FractionalDivide(lhs, rhs)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
		case fd@FractionalDivide(lhs, rhs) => FractionalDivide(f(lhs),f(rhs))(fd.fr.asInstanceOf[Fractional[A]],manifest[A])
		case _ => super.mirror(e,f)
	}
  }
}

trait ScalaGenFractionalOps extends ScalaGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FractionalDivide(a,b) => emitValDef(sym, src"$a / $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenFractionalOps extends CLikeGenBase {
  val IR: FractionalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case FractionalDivide(a,b) =>
          emitValDef(sym, src"$a / $b")
        case _ => super.emitNode(sym, rhs)
     }
    }
}

trait CudaGenFractionalOps extends CudaGenBase with CLikeGenFractionalOps
trait OpenCLGenFractionalOps extends OpenCLGenBase with CLikeGenFractionalOps
trait CGenFractionalOps extends CGenBase with CLikeGenFractionalOps with GenericNestedCodegen {
	val IR: FractionalOpsExp
	import IR._
    override def lowerNode[A:Manifest](sym: Sym[A], rhs: Def[A]) = rhs match {
		case fd@FractionalDivide(a,b) => fractional_divide(LIRLowering(a), LIRLowering(b))(fd.fr.asInstanceOf[Fractional[A]],manifest[A],implicitly[SourceContext])
		case _ => super.lowerNode(sym,rhs)
	}
}
