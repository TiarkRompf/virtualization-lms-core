package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack

trait LiftVariables extends Base {
  this: Variables =>

  def __newVar[T:Manifest](init: T) = var_new(init)
  def __newVar[T](init: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = var_new(init)
  def __newVar[T](init: Var[T])(implicit o: Overloaded2, mT: Manifest[T]) = var_new(init)
}

// ReadVar is factored out so that it does not have higher priority than VariableImplicits when mixed in
// (which result in ambiguous conversions)
trait ReadVarImplicit {
  this: Variables =>

  implicit def readVar[T:Manifest](v: Var[T]) : Rep[T]
}

trait ReadVarImplicitExp extends EffectExp {
  this: VariablesExp =>

  implicit def readVar[T:Manifest](v: Var[T]) : Exp[T] = {
    toAtom(ReadVar(v))
  }
}

trait LowPriorityVariableImplicits extends ImplicitOps {
  this: Variables =>

  implicit def varIntToRepDouble(x: Var[Int]): Rep[Double] = implicit_convert[Int,Double](readVar(x))
  implicit def varIntToRepFloat(x: Var[Int]): Rep[Float] = implicit_convert[Int,Float](readVar(x))
  implicit def varFloatToRepDouble(x: Var[Float]): Rep[Double] = implicit_convert[Float,Double](readVar(x))
}

trait VariableImplicits extends LowPriorityVariableImplicits {
  this: Variables =>

  // we always want to prioritize a direct conversion if any Rep will do
  implicit def varIntToRepInt(v: Var[Int]): Rep[Int] = readVar(v)
  implicit def varFloatToRepFloat(v: Var[Float]): Rep[Float] = readVar(v)
}

trait Variables extends Base with OverloadHack with VariableImplicits with ReadVarImplicit {
  type Var[+T]

  //implicit def chainReadVar[T,U](x: Var[T])(implicit f: Rep[T] => U): U = f(readVar(x))
  def var_new[T:Manifest](init: Rep[T]): Var[T]
  def var_assign[T:Manifest](lhs: Var[T], rhs: Rep[T]): Rep[Unit]
  def var_plusequals[T:Manifest](lhs: Var[T], rhs: Rep[T]): Rep[Unit]

  def __assign[T:Manifest](lhs: Var[T], rhs: T) = var_assign(lhs, rhs)
  def __assign[T](lhs: Var[T], rhs: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = var_assign(lhs, rhs)
  def __assign[T](lhs: Var[T], rhs: Var[T])(implicit o: Overloaded2, mT: Manifest[T]) = var_assign(lhs, readVar(rhs))

  // TODO: why doesn't this implicit kick in automatically? <--- do they belong here? maybe better move to NumericOps
  def infix_+=[T:Manifest](lhs: Var[T], rhs: T) = var_plusequals(lhs, rhs)
  def infix_+=[T](lhs: Var[T], rhs: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = var_plusequals(lhs,rhs)
  def infix_+=[T](lhs: Var[T], rhs: Var[T])(implicit o: Overloaded2, mT: Manifest[T]) = var_plusequals(lhs,readVar(rhs))
}

trait VariablesExp extends Variables with ImplicitOpsExp with VariableImplicits with ReadVarImplicitExp {
  // TODO: make a design decision here.
  // defining Var[T] as Sym[T] is dangerous. If someone forgets to define a more-specific implicit conversion from
  // Var[T] to Ops, e.g. implicit def varToRepStrOps(s: Var[String]) = new RepStrOpsCls(varToRep(s))
  // then the existing implicit from Rep to Ops will be used, and the ReadVar operation will be lost.
  // Defining Vars as separate from Exps will always cause a compile-time error if the implicit is missing.
  //type Var[T] = Sym[T]

  // REMARK: Var[T] should (probably) be different from Rep[T] in Rep-world
  // but in Exp-world the situation is less clear. Another thing is that in general,
  // programs should live in Rep-world.
  // Currently DeliteApplication extends DeliteOpsExp and therefore
  // all DSL programs live in Exp-world.

  type Var[+T] = Variable[T]

  // read operation
  /*
  implicit def readVar[T:Manifest](v: Var[T]) : Exp[T] = { // careful with implicits...

    //reflectRead(/*v*/)(ReadVar(v)) // FIXME!!
    //reflectEffect(ReadVar(v))

    // do cse *in context*

    context.reverse.dropWhile { e =>
      e match { case Def(Reflect(ReadVar(w), _)) if w != v => true case _ => false }
    } match {
      case (e @ Def(Reflect(ReadVar(`v`), _)))::es => e.asInstanceOf[Exp[T]]
      case es =>
        val r = createDefinition(fresh[T], Reflect(ReadVar(v), es)).sym
        context = context :+ r
        r
    }
    toAtom(ReadVar(v))
  }*/

  case class ReadVar[T:Manifest](v: Var[T]) extends Def[T]
  case class NewVar[T:Manifest](init: Exp[T]) extends Def[T]
  case class Assign[T:Manifest](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]
  case class VarPlusEquals[T:Manifest](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]


  def var_new[T:Manifest](init: Exp[T]): Var[T] = {
    //reflectEffect(NewVar(init)).asInstanceOf[Var[T]]
    Variable(reflectMutable(NewVar(init)))
  }

  def var_assign[T:Manifest](lhs: Var[T], rhs: Exp[T]): Exp[Unit] = {
    reflectWrite(lhs.e)(Assign(lhs, rhs))
    Const()
  }

  def var_plusequals[T:Manifest](lhs: Var[T], rhs: Exp[T]): Exp[Unit] = {
    reflectWrite(lhs.e)(VarPlusEquals(lhs, rhs))
    Const()
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case Reflect(NewVar(a), u, es) => reflectMirrored(Reflect(NewVar(f(a)), Alloc(), f(es)))
    case Reflect(ReadVar(Variable(a)), u, es) => reflectMirrored(Reflect(ReadVar(Variable(f(a))), mapOver(f,u), f(es)))
    case Reflect(Assign(Variable(a),b), u, es) => reflectMirrored(Reflect(Assign(Variable(f(a)), f(b)), mapOver(f,u), f(es)))
    case Reflect(VarPlusEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarPlusEquals(Variable(f(a)), f(b)), mapOver(f,u), f(es)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  // TODO: not using these due to a problem with getBlockResult() getting an out-of-scope symbol without the Const
  //def var_assign[T:Manifest](lhs: Var[T], rhs: Exp[T]) = reflectMutation(Assign(lhs, rhs))
  //def var_plusequals[T:Numeric:Manifest](lhs: Var[T], rhs: Exp[T]) = reflectMutation(VarPlusEquals(lhs, rhs))
}


trait ScalaGenVariables extends ScalaGenEffect {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym, quote(getBlockResult(init)))
    case Assign(Variable(a), b) => emitAssignment(quote(a), quote(getBlockResult(b)))
    //case Assign(a, b) => emitAssignment(quote(a), quote(b))
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " += " + quote(getBlockResult(b)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenVariables extends CLikeGenBase {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case ReadVar(Variable(a)) =>
          emitValDef(sym, quote(a))
        case NewVar(init) =>
          emitVarDef(sym, quote(getBlockResult(init)))
        case Assign(Variable(a), b) =>
          emitAssignment(quote(a), quote(getBlockResult(b)))
        case VarPlusEquals(Variable(a), b) =>
          emitAssignment(quote(a), quote(a) + " + " + quote(getBlockResult(b)))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenVariables extends CudaGenEffect with CLikeGenVariables
trait CGenVariables extends CGenEffect with CLikeGenVariables
