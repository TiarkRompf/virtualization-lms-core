package scala.virtualization.lms
package epfl
package test4

import common.{BlockExp,EffectExp}
import common.ScalaGenEffect // don't import FunctionsExp
import test2._
import test3._
import util.ClosureCompare
import scala.reflect.SourceContext

import scala.collection.{immutable, mutable}

trait FunctionsExpClever extends test3.FunctionsExp {

  def exec[A:Manifest,B:Manifest](fun: Exp[A]=>Exp[B], arg: Exp[A]): Exp[B]

  override def doApply[A:Manifest,B:Manifest](fun: Exp[A => B], arg: Exp[A])(implicit pos: SourceContext): Exp[B] = fun match {
    case Def(Lambda(fun)) => 
      exec(fun, arg)
    case _ => super.doApply(fun, arg)
  }
  
}


trait FunctionExpUnfoldAll extends FunctionsExpClever {
  def exec[A:Manifest,B:Manifest](fun: Exp[A]=>Exp[B], arg: Exp[A]): Exp[B] = {
    fun(arg)
  }
}

trait FunctionExpUnfoldFixedDepth extends FunctionsExpClever {

  var curDepth: Int = 0
  def maxDepth: Int = 5

  def exec[A:Manifest,B:Manifest](fun: Exp[A]=>Exp[B], arg: Exp[A]): Exp[B] = {
    if (curDepth < maxDepth) {
      curDepth += 1
      val res = fun(arg)
      curDepth -= 1
      res
    } else
      Apply(Sym[A=>B](-2), arg)
  }
}


trait FunctionExpUnfoldRecursion extends FunctionsExpClever with FunctionsCanonical {

  var recursion: List[(Function[_,_], Exp[Any], Int)] = List()
  val maxDepth: Int = 1

  def exec[A:Manifest,B:Manifest](f: Exp[A]=>Exp[B], x: Exp[A]): Exp[B] = {
    recursion.find(m => m._1 == f) match {
      case Some((_, y, `maxDepth`)) => // hit recursion bound!
        println("-- hit recursion: " + f.getClass + " " + x + " <- "+ y)
        // y should be a symbol, and it might take on value x
        Apply(Sym[A=>B](-2), x)
        
      case Some((_, y, recCount)) => // hit, but below depth bound
        val saveRecursion = recursion
        recursion = (f,x, recCount + 1)::recursion
        val res = f(x) // look for recursion
        recursion = saveRecursion
        res

      case None =>
        val saveRecursion = recursion
        recursion = (f,x, 1)::recursion
        val res = f(x) // look for recursion
        recursion = saveRecursion
        res
    }
  }
}



trait FunctionsCanonical extends FunctionsExp with ClosureCompare {

  var funTable: List[(Function[_,_], Any)] = List()
  
  def lookupFun[A:Manifest,B:Manifest](f: Exp[A]=>Exp[B]): (Exp[A]=>Exp[B]) = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((g, _)) =>
        //println("-- found fun: " + g.getClass.getName)
        g.asInstanceOf[Exp[A]=>Exp[B]]
      case _ =>
        funTable = (f,can)::funTable
        f
    }
  }


  override def doLambda[A:Manifest,B:Manifest](fun: Exp[A]=>Exp[B])(implicit pos: SourceContext) = {
    super.doLambda(lookupFun(fun))
  }
}


trait FunctionsExternalDef0 extends FunctionsExp with BlockExp {
  case class DefineFun[A,B](res: Block[B])(val arg: Sym[A]) extends Def[A=>B]

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case f@DefineFun(y) => f.arg::effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case DefineFun(y) => freqHot(y)
    case _ => super.symsFreq(e)
  }

}


trait FunctionsExternalDef01 extends FunctionsExternalDef0 { // not used

  override def doLambda[A:Manifest,B:Manifest](f: Exp[A]=>Exp[B])(implicit pos: SourceContext): Exp[A=>B] = {
    var funSym = fresh[A=>B]
    var argSym = fresh[A]//Sym(-1)
      
    createDefinition(funSym, DefineFun[A,B](Block(f(argSym)))(argSym)) //FIXME: use reify (conflict with test3.effects)
    funSym
  }

}

trait FunctionsExternalDef1 extends FunctionsExternalDef0 with ClosureCompare { // not used (New: used by TestMatcherNew)

  var funTable: List[(Function[_,_], Any, Sym[_])] = List()
  
  override def doLambda[A:Manifest,B:Manifest](f: Exp[A]=>Exp[B])(implicit pos: SourceContext): Exp[A=>B] = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((g, _, funSym)) =>
        //println("-- found fun: " + g.getClass.getName)
        funSym.asInstanceOf[Sym[A=>B]]
      case _ =>
      
        var funSym = fresh[A=>B]
        var argSym = fresh[A]//Sym(-1)
      
        val g = (x: Exp[A]) => Apply(funSym, x): Exp[B]
        funTable = (g,can,funSym)::funTable
        
        val y = Block(f(argSym)) // should use reifyEffects!
        
        createDefinition(funSym, DefineFun[A,B](y)(argSym))
        funSym
    }
  }



}

trait FunctionsExternalDef2 extends FunctionsCanonical with FunctionsExternalDef0 {

  override def lookupFun[A:Manifest,B:Manifest](f: Exp[A]=>Exp[B]): (Exp[A]=>Exp[B]) = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((g, _)) =>
        //println("-- found fun: " + g.getClass.getName)
        g.asInstanceOf[Exp[A]=>Exp[B]]
      case _ =>
      
        var funSym = fresh[A=>B]
        var argSym = fresh[A]//Sym(-1)
      
        val g = (x: Exp[A]) => Apply(funSym, x): Exp[B]
        funTable = (g,can)::funTable
        
        Block(f(argSym)) match { //FIXME: use reify (conflict with test3.effects)
          case Block(c @ Const(_)) => 
            val g = (x: Exp[A]) => c
            funTable = (g,can)::funTable // ok?
            g
          case e => 
            createDefinition(funSym, DefineFun[A,B](e)(argSym))
            g
        }
    }
  }

}

trait ScalaGenFunctionsExternal extends ScalaGenEffect {
  val IR: FunctionsExternalDef0 with EffectExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@DefineFun(y) =>
      emitValDef(sym, "{" + quote(e.arg) + ": (" + remap(e.arg.tp) + ") => "/*}*/)
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case Apply(fun, arg) => 
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case Apply2(fun, arg1, arg2) => 
      emitValDef(sym, quote(fun) + "(" + quote(arg1) + ", " + quote(arg2) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
