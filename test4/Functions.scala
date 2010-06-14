package test4

import test1._
import test2._
import test3._


trait FunctionsBase extends FunctionExp {

  def exec[A,B](fun: Exp[A]=>Exp[B], arg: Exp[A]): Exp[B]

  override def doApply[A,B](fun: Exp[A => B], arg: Exp[A]): Exp[B] = fun match {
    case Def(Lambda(fun)) => 
      exec(fun, arg)
    case _ => super.doApply(fun, arg)
  }
  
}


trait FunctionExpUnfoldAll extends FunctionsBase {
  def exec[A,B](fun: Exp[A]=>Exp[B], arg: Exp[A]): Exp[B] = {
    fun(arg)
  }
}

trait FunctionExpUnfoldFixedDepth extends FunctionsBase {

  var curDepth: Int = 0
  def maxDepth: Int = 5

  def exec[A,B](fun: Exp[A]=>Exp[B], arg: Exp[A]): Exp[B] = {
    if (curDepth < maxDepth) {
      curDepth += 1
      val res = fun(arg)
      curDepth -= 1
      res
    } else
      Apply(Sym[A=>B](-2), arg)
  }
}


trait FunctionExpUnfoldRecursion extends FunctionsBase with FunctionsCanonical {

  var recursion: List[(Function[_,_], Exp[Any], Int)] = List()
  val maxDepth: Int = 1

  def exec[A,B](f: Exp[A]=>Exp[B], x: Exp[A]): Exp[B] = {
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



trait FunctionsCanonical extends FunctionExp with ClosureCompare {

  var funTable: List[(Function[_,_], Any)] = List()
  
  def lookupFun[A,B](f: Exp[A]=>Exp[B]): (Exp[A]=>Exp[B]) = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((g, _)) =>
        println("-- found fun: " + g.getClass.getName)
        g.asInstanceOf[Exp[A]=>Exp[B]]
      case _ =>
        funTable = (f,can)::funTable
        f
    }
  }


  override def doLambda[A,B](fun: Exp[A]=>Exp[B]) = {
    super.doLambda(lookupFun(fun))
  }
}


trait FunctionsExternalDef0 extends FunctionExp {
  case class DefineFun[A,B](res: Exp[B]) extends Def[A=>B]
}

trait FunctionsExternalDef01 extends FunctionsExternalDef0 { // not used

  override def doLambda[A,B](f: Exp[A]=>Exp[B]): Exp[A=>B] = {
    var funSym = fresh[A=>B]
    var argSym = fresh[A]//Sym(-1)
      
    createDefinition(funSym, DefineFun[A,B](f(argSym)))
    funSym
  }

}

trait FunctionsExternalDef1 extends FunctionsExternalDef0 with ClosureCompare { // not used

  var funTable: List[(Function[_,_], Any)] = List()
  
  override def doLambda[A,B](f: Exp[A]=>Exp[B]): Exp[A=>B] = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((g, _)) =>
        println("-- found fun: " + g.getClass.getName)
        Lambda(g.asInstanceOf[Exp[A]=>Exp[B]])
      case _ =>
      
        var funSym = fresh[A=>B]
        var argSym = fresh[A]//Sym(-1)
      
        val g = (x: Exp[A]) => Apply(funSym, x): Exp[B]
        funTable = (g,can)::funTable
        
        f(argSym) match {
          case c @ Const(_) => 
            val g = (x: Exp[A]) => c
            funTable = (g,can)::funTable // ok?
            Lambda(g)
          case e => 
            createDefinition(funSym, DefineFun[A,B](e))
            funSym
        }
    }
  }

}

trait FunctionsExternalDef2 extends FunctionsCanonical with FunctionsExternalDef0 {

  override def lookupFun[A,B](f: Exp[A]=>Exp[B]): (Exp[A]=>Exp[B]) = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((g, _)) =>
        println("-- found fun: " + g.getClass.getName)
        g.asInstanceOf[Exp[A]=>Exp[B]]
      case _ =>
      
        var funSym = fresh[A=>B]
        var argSym = fresh[A]//Sym(-1)
      
        val g = (x: Exp[A]) => Apply(funSym, x): Exp[B]
        funTable = (g,can)::funTable
        
        f(argSym) match {
          case c @ Const(_) => 
            val g = (x: Exp[A]) => c
            funTable = (g,can)::funTable // ok?
            g
          case e => 
            createDefinition(funSym, DefineFun[A,B](e))
            g
        }
    }
  }

}
