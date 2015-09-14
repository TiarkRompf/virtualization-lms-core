package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext

trait LiftVariables extends Base {
  this: Variables =>

  def __newVar[T:Manifest](init: T)(implicit pos: SourceContext) = var_new(unit(init))
  def __newVar[T](init: Rep[T])(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_new(init)
  def __newVar[T](init: Var[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_new(init)
}

// ReadVar is factored out so that it does not have higher priority than VariableImplicits when mixed in
// (which result in ambiguous conversions)
trait ReadVarImplicit {
  this: Variables =>

  implicit def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext) : Rep[T]
}

trait ReadVarImplicitExp extends EffectExp {
  this: VariablesExp =>

  implicit def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext) : Exp[T] = ReadVar(v)
}

trait LowPriorityVariableImplicits extends ImplicitOps {
  this: Variables =>

  implicit def varIntToRepLong(x: Var[Int])(implicit pos: SourceContext): Rep[Long] = implicit_convert[Int,Long](readVar(x))
  implicit def varIntToRepDouble(x: Var[Int])(implicit pos: SourceContext): Rep[Double] = implicit_convert[Int,Double](readVar(x))
  implicit def varIntToRepFloat(x: Var[Int])(implicit pos: SourceContext): Rep[Float] = implicit_convert[Int,Float](readVar(x))
  implicit def varFloatToRepDouble(x: Var[Float])(implicit pos: SourceContext): Rep[Double] = implicit_convert[Float,Double](readVar(x))
  implicit def varLongToRepDouble(x: Var[Long])(implicit pos: SourceContext): Rep[Double] = implicit_convert[Long,Double](readVar(x))
  implicit def varLongToRepFloat(x: Var[Long])(implicit pos: SourceContext): Rep[Float] = implicit_convert[Long,Float](readVar(x))
}

trait VariableImplicits extends LowPriorityVariableImplicits {
  this: Variables =>

  // we always want to prioritize a direct conversion if any Rep will do
  implicit def varIntToRepInt(v: Var[Int])(implicit pos: SourceContext): Rep[Int] = readVar(v)
  implicit def varFloatToRepFloat(v: Var[Float])(implicit pos: SourceContext): Rep[Float] = readVar(v)
  implicit def varLongToRepLong(v: Var[Long])(implicit pos: SourceContext): Rep[Long] = readVar(v)
  implicit def varDoubleToRepDouble(v: Var[Double])(implicit pos: SourceContext): Rep[Double] = readVar(v)
}

trait Variables extends Base with OverloadHack with VariableImplicits with ReadVarImplicit {
  type Var[+T] //FIXME: should be invariant

  //implicit def chainReadVar[T,U](x: Var[T])(implicit f: Rep[T] => U): U = f(readVar(x))
  def var_new[T:Manifest](init: Rep[T])(implicit pos: SourceContext): Var[T]
  def var_assign[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def var_plusequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def var_minusequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def var_timesequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def var_divideequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit]

  def __assign[T:Manifest](lhs: Var[T], rhs: T)(implicit pos: SourceContext) = var_assign(lhs, unit(rhs))
  def __assign[T](lhs: Var[T], rhs: Rep[T])(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_assign(lhs, rhs)
  def __assign[T](lhs: Var[T], rhs: Var[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_assign(lhs, readVar(rhs))
/*
  def __assign[T,U](lhs: Var[T], rhs: Rep[U])(implicit o: Overloaded2, mT: Manifest[T], mU: Manifest[U], conv: Rep[U]=>Rep[T]) = var_assign(lhs, conv(rhs))
*/

  // TODO: why doesn't this implicit kick in automatically? <--- do they belong here? maybe better move to NumericOps
  // we really need to refactor this. +=/-= shouldn't be here or in Arith, but in some other type class, which includes Numeric variables
  def infix_+=[T](lhs: Var[T], rhs: T)(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_plusequals(lhs, unit(rhs))
  def infix_+=[T](lhs: Var[T], rhs: Rep[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_plusequals(lhs,rhs)
  def infix_+=[T](lhs: Var[T], rhs: Var[T])(implicit o: Overloaded3, mT: Manifest[T], pos: SourceContext) = var_plusequals(lhs,readVar(rhs))
  def infix_-=[T](lhs: Var[T], rhs: T)(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_minusequals(lhs, unit(rhs))
  def infix_-=[T](lhs: Var[T], rhs: Rep[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_minusequals(lhs,rhs)
  def infix_-=[T](lhs: Var[T], rhs: Var[T])(implicit o: Overloaded3, mT: Manifest[T], pos: SourceContext) = var_minusequals(lhs,readVar(rhs))
  def infix_*=[T](lhs: Var[T], rhs: T)(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_timesequals(lhs, unit(rhs))
  def infix_*=[T](lhs: Var[T], rhs: Rep[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_timesequals(lhs,rhs)
  def infix_*=[T](lhs: Var[T], rhs: Var[T])(implicit o: Overloaded3, mT: Manifest[T], pos: SourceContext) = var_timesequals(lhs,readVar(rhs))
  def infix_/=[T](lhs: Var[T], rhs: T)(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_divideequals(lhs, unit(rhs))
  def infix_/=[T](lhs: Var[T], rhs: Rep[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_divideequals(lhs,rhs)
  def infix_/=[T](lhs: Var[T], rhs: Var[T])(implicit o: Overloaded3, mT: Manifest[T], pos: SourceContext) = var_divideequals(lhs,readVar(rhs))
}

trait VariablesExp extends Variables with ImplicitOpsExp with VariableImplicits with ReadVarImplicitExp with AtomicWrites {
  // REMARK:
  // defining Var[T] as Sym[T] is dangerous. If someone forgets to define a more-specific implicit conversion from
  // Var[T] to Ops, e.g. implicit def varToRepStrOps(s: Var[String]) = new RepStrOpsCls(varToRep(s))
  // then the existing implicit from Rep to Ops will be used, and the ReadVar operation will be lost.
  // Defining Vars as separate from Exps will always cause a compile-time error if the implicit is missing.

  //case class Variable[+T](val e: Exp[Variable[T]]) // FIXME: in Expressions because used by codegen...
  type Var[+T] = Variable[T] //FIXME: should be invariant

  case class ReadVar[T:Manifest](v: Var[T]) extends Def[T]
  case class NewVar[T:Manifest](init: Exp[T]) extends Def[Variable[T]]
  case class Assign[T:Manifest](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]
  case class VarPlusEquals[T:Manifest](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]
  case class VarMinusEquals[T:Manifest](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]
  case class VarTimesEquals[T:Manifest](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]
  case class VarDivideEquals[T:Manifest](lhs: Var[T], rhs: Exp[T]) extends Def[Unit]

  // atomic write rewrite rules
  override def recurseLookup[T:Manifest](sym: Exp[Any], trace: List[AtomicTracer]): (Exp[Any],List[AtomicTracer]) = sym match {
    case Def(ReadVar(Variable(e))) => recurseLookup(e, VarTracer +: trace)
    case Def(Reflect(ReadVar(Variable(e)),_,_)) => recurseLookup(e, VarTracer +: trace)
    case _ => super.recurseLookup(sym,trace)
  }

  def var_new[T:Manifest](init: Exp[T])(implicit pos: SourceContext): Var[T] = {
    //reflectEffect(NewVar(init)).asInstanceOf[Var[T]]
    Variable(reflectMutable(NewVar(init)))
  }

  def var_assign[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(Assign(lhs, rhs))
    Const()
  }

  def var_plusequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarPlusEquals(lhs, rhs))
    Const()
  }

  def var_minusequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarMinusEquals(lhs, rhs))
    Const()
  }

  def var_timesequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarTimesEquals(lhs, rhs))
    Const()
  }

  def var_divideequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarDivideEquals(lhs, rhs))
    Const()
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => Nil
    case ReadVar(Variable(a)) => Nil
    case Assign(Variable(a),b) => Nil
    case VarPlusEquals(Variable(a),b) => Nil
    case VarMinusEquals(Variable(a),b) => Nil
    case VarTimesEquals(Variable(a),b) => Nil
    case VarDivideEquals(Variable(a),b) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => syms(a)
    case ReadVar(Variable(a)) => Nil
    case Assign(Variable(a),b) => syms(b)
    case VarPlusEquals(Variable(a),b) => syms(b)
    case VarMinusEquals(Variable(a),b) => syms(b)
    case VarTimesEquals(Variable(a),b) => syms(b)
    case VarDivideEquals(Variable(a),b) => syms(b)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => Nil
    case ReadVar(Variable(a)) => syms(a)
    case Assign(Variable(a),b) => syms(a) // Assume the assignment is in a loop and alias back to previous readers (not precise!)
    case VarPlusEquals(Variable(a),b) => syms(a)
    case VarMinusEquals(Variable(a),b) => syms(a)
    case VarTimesEquals(Variable(a),b) => syms(a)
    case VarDivideEquals(Variable(a),b) => syms(a)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => Nil
    case ReadVar(Variable(a)) => Nil
    case Assign(Variable(a),b) => Nil
    case VarPlusEquals(Variable(a),b) => Nil
    case VarMinusEquals(Variable(a),b) => Nil
    case VarTimesEquals(Variable(a),b) => Nil
    case VarDivideEquals(Variable(a),b) => Nil
    case _ => super.copySyms(e)
  }



  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ReadVar(Variable(a)) => readVar(Variable(f(a)))
    case Reflect(NewVar(a), u, es) => reflectMirrored(Reflect(NewVar(f(a)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ReadVar(Variable(a)), u, es) => reflectMirrored(Reflect(ReadVar(Variable(f(a))), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(Assign(Variable(a),b), u, es) => reflectMirrored(Reflect(Assign(Variable(f(a)), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(VarPlusEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarPlusEquals(Variable(f(a)), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(VarMinusEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarMinusEquals(Variable(f(a)), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(VarTimesEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarTimesEquals(Variable(f(a)), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(VarDivideEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarDivideEquals(Variable(f(a)), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}


trait VariablesExpOpt extends VariablesExp {

  override implicit def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext) : Exp[T] = {
    if (context ne null) {
      // find the last modification of variable v
      // if it is an assigment, just return the last value assigned
      val vs = v.e.asInstanceOf[Sym[Variable[T]]]
      //TODO: could use calculateDependencies(Read(v))

      val rhs = context.reverse.collectFirst {
        case w @ Def(Reflect(NewVar(rhs: Exp[T]), _, _)) if w == vs => Some(rhs)
        case Def(Reflect(Assign(`v`, rhs: Exp[T]), _, _)) => Some(rhs)
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.readVar(v))
    } else {
      super.readVar(v)
    }
  }

  // eliminate (some) redundant stores
  // TODO: strong updates. overwriting a var makes previous stores unnecessary
  override implicit def var_assign[T:Manifest](v: Var[T], e: Exp[T])(implicit pos: SourceContext) : Exp[Unit] = {
    if (context ne null) {
      // find the last modification of variable v
      // if it is an assigment with the same value, we don't need to do anything
      val vs = v.e.asInstanceOf[Sym[Variable[T]]]
      //TODO: could use calculateDependencies(Read(v))

      context.reverse.foreach {
        case w @ Def(Reflect(NewVar(rhs: Exp[T]), _, _)) if w == vs => if (rhs == e) return ()
        case Def(Reflect(Assign(`v`, rhs: Exp[T]), _, _)) => if (rhs == e) return ()
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) =>  // not a simple assignment
        case _ => // ...
      }
    }
    super.var_assign(v,e)
  }

}

trait ScalaGenVariables extends ScalaGenEffect with ScalaGenImplicitOps {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
    case Assign(Variable(a), b) => emitAssignment(a.asInstanceOf[Sym[Variable[Any]]],quote(b))
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " += " + quote(b))
    case VarMinusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " -= " + quote(b))
    case VarTimesEquals(Variable(a), b) => emitValDef(sym, quote(a) + " *= " + quote(b))
    case VarDivideEquals(Variable(a), b) => emitValDef(sym, quote(a) + " /= " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }

/*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
    case Assign(Variable(a), b) => emitValDef(sym, quote(a) + " = " + quote(b))
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " += " + quote(b))
    case VarMinusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " -= " + quote(b))
    case VarTimesEquals(Variable(a), b) => emitValDef(sym, quote(a) + " *= " + quote(b))
    case VarDivideEquals(Variable(a), b) => emitValDef(sym, quote(a) + " /= " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
*/
}

trait CLikeGenVariables extends CLikeGenBase with CLikeGenImplicitOps {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
        case NewVar(init) => emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
        case Assign(Variable(a), b) => stream.println(quote(a) + " = " + quote(b) + ";")
        case VarPlusEquals(Variable(a), b) => stream.println(quote(a) + " += " + quote(b) + ";")
        case VarMinusEquals(Variable(a), b) =>stream.println(quote(a) + " -= " + quote(b) + ";")
        case VarTimesEquals(Variable(a), b) => stream.println(quote(a) + " *= " + quote(b) + ";")
        case VarDivideEquals(Variable(a), b) => stream.println(quote(a) + " /= " + quote(b) + ";")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenVariables extends CudaGenEffect with CLikeGenVariables
trait OpenCLGenVariables extends OpenCLGenEffect with CLikeGenVariables
trait CGenVariables extends CGenEffect with CLikeGenVariables
