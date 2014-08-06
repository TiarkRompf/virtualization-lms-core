/*TODO DISABLED
package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext

trait LiftEquals extends Base {
  this: Equal =>

  // TODO: these will probably never get called due to original, equally specific == and !=
  def __equal[A:Manifest,B:Manifest](a: A, b: B) : Rep[Boolean] = equals(unit(a),unit(b))
  def infix_!=[A:Manifest,B:Manifest](a: A, b: B) : Rep[Boolean] = notequals(unit(a),unit(b))
}

trait Equal extends Base with Variables with OverloadHack {
  // TODO: we need a better way of handling this, too many combinations
  // this occurs because Rep is an object, so it has an == and != method defined by default,
  // so there is no type error to force the implicit conversions
  def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a,b)
  def __equal[A,B](a: Rep[A], b: Var[B])(implicit o: Overloaded2, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: Var[A], b: Rep[B])(implicit o: Overloaded3, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a, b)
  def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(a, unit(b))
  def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(unit(a), b)
  def __equal[A,B](a: Var[A], b: B)(implicit o: Overloaded6, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(a, unit(b))
  def __equal[A,B](a: A, b: Var[B])(implicit o: Overloaded7, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(unit(a), b)
  def __equal[A,B](a: Var[A], b: Var[B])(implicit o: Overloaded8, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a,b)

  def infix_!=[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a,b)
  def infix_!=[A,B](a: Rep[A], b: Var[B])(implicit o: Overloaded2, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Var[A], b: Rep[B])(implicit o: Overloaded3, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, unit(b))
  def infix_!=[A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(unit(a), b)
  def infix_!=[A,B](a: Var[A], b: B)(implicit o: Overloaded6, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, unit(b))
  def infix_!=[A,B](a: A, b: Var[B])(implicit o: Overloaded7, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(unit(a), b)
  def infix_!=[A,B](a: Var[A], b: Var[B])(implicit o: Overloaded8, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a,b)

  def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext) : Rep[Boolean]
  def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext) : Rep[Boolean]
}

trait EqualExpBridge extends BaseExp  {

  case class Equal[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B]) extends Def[Boolean]
  case class NotEqual[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B]) extends Def[Boolean]

  def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext): Rep[Boolean] = Equal(a,b)
  def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext): Rep[Boolean] = NotEqual(a,b)

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case Equal(a, b) => Equal(f(a),f(b))
    case NotEqual(a, b) => NotEqual(f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Equal(a, b) => equals(f(a),f(b))
    case NotEqual(a, b) => notequals(f(a),f(b))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait EqualExp extends Equal with EqualExpBridge with VariablesExp

trait EqualExpBridgeOpt extends EqualExpBridge {
  override def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext): Rep[Boolean] = if (a == b) Const(true) else (a,b) match {
    case (Const(a),Const(b)) => Const(a == b)
    case _ => super.equals(a,b)
  }

  override def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext): Rep[Boolean] = if (a == b) Const(false) else (a,b) match {
    case (Const(a),Const(b)) => Const(a != b)
    case _ => super.notequals(a,b)
  }
}

trait EqualExpOpt extends EqualExp with EqualExpBridgeOpt


trait ScalaGenEqual extends ScalaGenBase {
  val IR: EqualExpBridge
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, src"$a == $b")
    case NotEqual(a,b) =>  emitValDef(sym, src"$a != $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenEqual extends CLikeGenBase {
  val IR: EqualExpBridge
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case Equal(a,b) =>
          emitValDef(sym, src"$a == $b")
        case NotEqual(a,b) =>
          emitValDef(sym, src"$a != $b")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenEqual extends CudaGenBase with CLikeGenEqual
trait OpenCLGenEqual extends OpenCLGenBase with CLikeGenEqual

trait CGenEqual extends CGenBase with CLikeGenEqual {
  val IR: EqualExpBridge
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case Equal(a,b) if(remap(a.tp) == "string" && remap(b.tp) == "string") =>
        emitValDef(sym, quote(a) + ".compare(" + quote(b) + ") == 0")
      case Equal(a,b) if (!isPrimitiveType(a.tp) && !isPrimitiveType(b.tp) && (remap(a.tp) == remap(b.tp))) =>
        emitValDef(sym, quote(a) + "->equals(" + quote(b) + ")")
      case NotEqual(a,b) if(remap(a.tp) == "string" && remap(b.tp) == "string") =>
        emitValDef(sym, quote(a) + ".compare(" + quote(b) + ") != 0")
      case NotEqual(a,b) if (!isPrimitiveType(a.tp) && !isPrimitiveType(b.tp) && (remap(a.tp) == remap(b.tp))) =>
        emitValDef(sym, "!" + quote(a) + "->equals(" + quote(b) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
*/
