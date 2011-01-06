package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

// very preliminary!

trait Fusion extends internal.GenericNestedCodegen {
  import IR._


}


trait Traversing extends internal.GenericNestedCodegen { // should be other way round!!
  
  
}


trait Transforming extends Traversing {
  import IR._  


  class Traverser {
    val subst = new scala.collection.mutable.HashMap[Exp[_], Exp[_]]
    
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
      case Some(y) => apply(y.asInstanceOf[Exp[A]]) case None => x 
    }
    
    def transform[A](s: Sym[A], x: Def[A]): Exp[A] = {
      if (subst.contains(s)) return apply(s)
      
      val y = mirror(x, this)
      if (s != y) {
        if (y.isInstanceOf[Sym[_]] && findDefinition(y.asInstanceOf[Sym[_]]).nonEmpty)
          println("--> replace " + s+"="+x + " by " + y+"="+findDefinition(y.asInstanceOf[Sym[_]]).get.rhs)
        else
          println("--> replace " + s+"="+x + " by " + y)
        subst(s) = y
      }
      y
    }
  }


  def mirror[A](e: Def[A], f: Traverser): Exp[A] = system.error("don't know how to mirror " + e)
    
}


trait FatTransforming extends Transforming { // TODO: shouldn't depend on loops?
  val IR: FatLoopsExp with EffectExp
  import IR._
  
  def foobar(t: Traverser)(scope: List[TTP]) = scope flatMap {
    case TTP(List(sym), ThinDef(rhs)) =>
      t.transform(sym, rhs) match {
        case s @ Def(r) => List(TTP(List(s.asInstanceOf[Sym[_]]), ThinDef(r)))
        case _ => Nil
      }
    case TTP(lhs, FatLoop(s,x,rhs)) =>
      val lhs2 = (lhs zip rhs).map(p=>t.transform(p._1,p._2)).collect { case s: Sym[_] => s }.distinct.asInstanceOf[List[Sym[_]]]
      lhs2 match {
        case Nil => Nil
        case _ => 
          val rhs2 = lhs2 map (findDefinition(_).get.rhs)
          val args2 = x map (t(_))
          List(TTP(lhs2, FatLoop(s,args2.asInstanceOf[List[Sym[Int]]],rhs2)))
      }
  }

  def simplify(scope: List[TTP])(results: List[Exp[_]]): (List[TTP], List[Exp[_]]) = {
    
    val t = new Traverser    
    
    val scope2 = foobar(t)(scope) // ugly
    val results2 = results map (t(_))
    
    (scope2, results2)
  }

}


trait FatLoopsExp extends FatStuffExp with LoopsExp {
  
  case class FatLoop(s: Rep[Int], x: List[Sym[Int]], rhs: List[Def[_]]) extends FatDef(rhs)    
  
  case class ArrayElem(y: Exp[Double]) extends Def[Array[Double]]
  case class ReduceElem(y: Exp[Double]) extends Def[Double]
  
}



trait TransformingStuff extends Transforming {
  val IR: FatLoopsExp with ArithExp with PrintExp // TODO: refactor
  import IR._  

  override def mirror[A](e: Def[A], f: Traverser): Exp[A] = (e match {
    //case Copy(a) => f(a)
    case ArrayElem(y) => toAtom(ArrayElem(f(y)))
    case ReduceElem(y) => toAtom(ReduceElem(f(y)))
    case ArrayIndex(a,i) => toAtom(ArrayIndex(f(a), f(i)))
    case Plus(x,y) => infix_+(f(x), f(y))
    case Minus(x,y) => infix_-(f(x), f(y))
    case Times(x,y) => infix_*(f(x), f(y))
    case Div(x,y) => infix_/(f(x), f(y))
    case Reflect(Print(x), es) => toAtom(Reflect(Print(f(x)), es map (e => f(e))))
    case Reify(x, es) => toAtom(Reify(f(x), es map (e => f(e))))
  }).asInstanceOf[Exp[A]]
    
}


trait FatStuffExp extends BaseExp {
  
  class FatDef(val elems: List[Def[_]])

  case class TTP(val lhs: List[Sym[_]], val rhs: FatDef)
  
  case class ThinDef(rhs: Def[_]) extends FatDef(List(rhs))
  
}


trait FatScheduling extends internal.Scheduling {
  val IR: FatStuffExp 
  import IR._  
  
  import util.GraphUtil

  def fatten(defs: List[TP[_]]): List[TTP] = defs.map {
    case TP(sym, rhs) => TTP(List(sym), ThinDef(rhs))
  }
  
  def getSchedule(scope: List[TP[_]])(result: Any): List[TP[_]] = {
    def deps(st: List[Sym[_]]): List[TP[_]] =
      scope.filter(st contains _.sym)

    GraphUtil.stronglyConnectedComponents[TP[_]](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatSchedule(scope: List[TTP])(result: Any): List[TTP] = {
    def deps(st: List[Sym[_]]): List[TTP] =
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    GraphUtil.stronglyConnectedComponents[TTP](deps(syms(result)), t => deps(syms(t.rhs))).flatten.reverse
  }

  def getFatDependentStuff(scope: List[TTP])(st: List[Sym[_]]): List[TTP] = {
    st.flatMap(getFatDependentStuff0(scope)(_)).distinct
  }
    
  def getFatDependentStuff0(scope: List[TTP])(st: Sym[_]): List[TTP] = {
    def uses(s: Sym[_]): List[TTP] = {
      scope.filter { d =>
        (d.lhs contains s) || // include the definition itself
        syms(d.rhs).contains(s) && !boundSyms(d.rhs).contains(st) // don't extrapolate outside the scope
      }
    }
    GraphUtil.stronglyConnectedComponents[TTP](uses(st), t => t.lhs flatMap uses).flatten
  }
    
}



trait ScalaGenFatStuff extends ScalaGenEffect with FatScheduling {
  val IR: FatStuffExp with EffectExp
  import IR._  
  
  case class Combine(a: List[Exp[Any]]) extends Exp[Any]


  override def emitBlockFocused(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    var currentScope = fatten(innerScope)
    currentScope = getFatSchedule(currentScope)(result) // clean things up!
    emitFatBlockFocused(currentScope)(result)
  }

  def emitFatBlockFocused(currentScope: List[TTP])(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    // do what super does, modulo fat stuff
    focusExactScopeFat(currentScope)(result) { levelScope => 
      for (TTP(syms, rhs) <- levelScope) {
        emitFatNode(syms, rhs)
      }
    }
  }
  
  
  

  def focusExactScopeFat[A](currentScope: List[TTP])(result: Exp[_])(body: List[TTP] => A): A = {
    
    val saveInner = innerScope
    
    val e1 = currentScope
    shallow = true
    val e2 = getFatSchedule(currentScope)(result) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false

    // shallow is 'must outside + should outside' <--- currently shallow == deep for lambdas, meaning everything 'should outside'
    // bound is 'must inside'

    // find transitive dependencies on bound syms, including their defs (in case of effects)
    val bound = e1.flatMap(z => boundSyms(z.rhs))
    val g1 = getFatDependentStuff(currentScope)(bound)
    
    val levelScope = e1.filter(z => (e2 contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound

/*
    // sanity check to make sure all effects are accounted for
    result match {
      case Def(Reify(x, effects)) =>
        val actual = levelScope.filter(effects contains _.sym)
        assert(effects == actual.map(_.sym), "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
      case _ =>
    }
*/
    val innerScope2 = e1 diff levelScope // delay everything that remains

    innerScope = innerScope2 flatMap { 
      case TTP(List(sym), ThinDef(rhs)) => List(TP(sym, rhs))
      case e => 
      println("ignore: " + e)
      Nil
    }

    val rval = body(levelScope)
    
    innerScope = saveInner
    rval
  }


  def emitFatNode(sym: List[Sym[_]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case ThinDef(a) => emitNode(sym(0), a)
    case _ => system.error("don't know how to generate code for: "+rhs)
  }

  def emitFatBlock(rhs: List[Exp[_]])(implicit stream: PrintWriter): Unit = {
    emitBlock(Combine(rhs))
  }

  
}




trait ScalaGenFatLoops extends ScalaGenFatStuff {
  val IR: FatLoopsExp
  import IR._
  
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case FatLoop(s, x, rhs) => syms(s):::syms(rhs)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case FatLoop(s, x, rhs) => x
    case _ => super.boundSyms(e)
  }

  override def fatten(defs: List[TP[_]]) = defs map { // TODO: reorg to call super
    case e @ TP(sym, LoopArray(s,x,y)) => TTP(List(sym), FatLoop(s,List(x),List(ArrayElem(y))))
    case e @ TP(sym, LoopReduce(s,x,y)) => TTP(List(sym), FatLoop(s,List(x),List(ReduceElem(y))))
    case TP(sym, rhs) => TTP(List(sym), ThinDef(rhs))
  }
  
  override def emitFatNode(sym: List[Sym[_]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case FatLoop(s,x,rhs) => 
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println("var " + quote(l) + " = new Array[]("+quote(s)+")")
          case ReduceElem(y) =>
            stream.println("var " + quote(l) + " = 0")
        }
      }
      val ii = x(0)
//      stream.println("var " + quote(ii) + " = 0")
//      stream.println("while ("+quote(ii)+" < "+quote(s)+") {")
      stream.println("for ("+quote(ii)+" <- 0 until "+quote(s)+") {")
      for (jj <- x.drop(1)) {
        stream.println(quote(jj)+" = "+quote(ii))
      }
      emitFatBlock(syms(rhs))
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println(quote(l) + "("+quote(ii)+") = " + quote(getBlockResult(y)))
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
        }
      }
//      stream.println(quote(ii)+" += 1")
      stream.println("}")
    case _ => super.emitFatNode(sym, rhs)
  }  
}



trait ScalaGenFatLoopsFusionOpt extends ScalaGenFatLoops with ScalaGenLoops with 
    TransformingStuff with FatTransforming with Fusion with FatScheduling {
  val IR: FatLoopsExp with ArithExp with PrintExp  // TODO: refactor
  import IR._  
  

  override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: Exp[_])(body: List[TTP] => A): A = {
    var result: Exp[_] = result0
    var currentScope = currentScope0

    // find loops at current top level
    var Wloops = super.focusExactScopeFat(currentScope)(result) { levelScope => 
      levelScope collect { case e @ TTP(_, FatLoop(_,_,_)) => e }
    }
    
    // FIXME: more than one super call means exponential cost -- is there a better way?
    // ---> implicit memoization or explicit data structure
    
    /* problem: fusion might change currentScope quite drastically
       is there some kind of strength reduction transformation to go from here
       to the fused version without recomputing as much as we do now?
    */   
    
    if (Wloops.nonEmpty) {
      var done = false

      do {
        // utils
        def WgetLoopVar(e: TTP): List[Sym[Int]] = e.rhs match { case FatLoop(s,x,rhs) => x }
        def WgetLoopRes(e: TTP): List[Def[_]] = e.rhs match { case FatLoop(s,x,rhs) => rhs }

        val loopSyms = Wloops flatMap (_.lhs)
        val loopVars = Wloops flatMap WgetLoopVar

        val WloopSyms = Wloops map (_.lhs)
        val WloopVars = Wloops map WgetLoopVar

        
        // find negative dependencies (those that block fusion)
        
        // might be costly: resolve and traverse complete input deps for each loop body
        // O(nloops*currentScope.length)   assuming schedule is linear (NOT true currently)
        
        val WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies
          val thisLoopSyms = WgetLoopVar(dx)
          val otherLoopSyms = loopSyms diff (dx.lhs)
          getFatSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
            case TTP(_, ThinDef(ArrayIndex(a, i))) if (thisLoopSyms contains i) =>
              Nil // direct deps on this'loops induction var don't count
            case sc =>
              syms(sc.rhs).intersect(otherLoopSyms) flatMap { otherLoop => dx.lhs map ((otherLoop, _)) }
          }
        }.distinct
      
        // TODO: have WtableNeg keep track of the statements that prevent fusion
        // then we can later remove the entry and see if the dependency goes away...
      
        println("wtableneg: " + WtableNeg)
        
        
        // partitioning: build maximal sets of loops to be fused
        
        def canFuse(a: TTP, b: TTP): Boolean = {
          val cartesian = a.lhs.flatMap(x=>b.lhs.flatMap(y=>List((x,y),(y,x))))
          (WtableNeg intersect cartesian).isEmpty
        }

      
        val t = new Traverser

        var partitionsIn = Wloops
        var partitionsOut = Nil:List[TTP]
      
        for (b <- partitionsIn) {
          // try to add to an item in partitionsOut, if not possible add as-is
          partitionsOut.find(a => canFuse(a,b)) match {
            case Some(a) => 
              val shape = a.rhs match { case FatLoop(s,_,_) => s } // TODO!
              val targetVar = WgetLoopVar(a)(0)
              //currentScope = currentScope ::: (WgetLoopVar(b) map (v => TTP(List(v), ThinDef(Copy(targetVar))))) // TODO: maybe not here?
              for (w <- WgetLoopVar(b))
                t.subst(w) = targetVar
              
              val fused = TTP(a.lhs:::b.lhs, FatLoop(shape, List(targetVar), WgetLoopRes(a):::WgetLoopRes(b)))
              partitionsOut = fused :: (partitionsOut diff List(a))
            case None => partitionsOut = b::partitionsOut
          }
        }
      
        println("partitions: " + partitionsOut)
      
      
        // actually do the fusion: 
      
        if ((partitionsOut intersect partitionsIn) != partitionsOut) {

          // equalize loop variables (TODO!)

          // within fused loops, remove accesses to outcomes of the fusion

          currentScope.foreach {
            case e@TTP(List(s), ThinDef(ArrayIndex(a, i))) =>
              println("considering " + e)
              partitionsOut.find(_.lhs contains a) match {
                case Some(fused) if WgetLoopVar(fused) contains t(i) => 
                  val index = fused.lhs.indexOf(a)
                  
                  println("replace " + e + " at " + index + " within " + fused)

                  val rhs = WgetLoopRes(fused)(index) match { case ArrayElem(y) => y }
                  
                  t.subst(s) = rhs
                case _ => //e
              }
            case _ => //e
          }
          
          currentScope = getFatSchedule(currentScope)(result) // clean things up!

          // SIMPLIFY! <--- multiple steps necessary???
          
          currentScope = foobar(t)(currentScope)
          result = t(result)
          currentScope = getFatSchedule(currentScope)(result) // clean things up!

          currentScope = foobar(t)(currentScope)
          result = t(result)
          currentScope = getFatSchedule(currentScope)(result) // clean things up!
          
          //Wloops = currentScope collect { case e @ TTP(_, FatLoop(_,_,_)) => e }

          Wloops = foobar(t)(partitionsOut)
          
          println("try once more ...")
        } else {
          println("no changes, we're done")
          done = true
        }
      
      } while (!done)
     
      
      // prune Wloops (some might be no longer necessary!)
      Wloops = Wloops map {
        case TTP(lhs, FatLoop(s, x, rhs)) =>
          val ex = lhs map (s => currentScope exists (_.lhs == List(s)))
          def select[A](a: List[A], b: List[Boolean]) = (a zip b) collect { case (w, true) => w }
          TTP(select(lhs, ex), FatLoop(s, x, select(rhs, ex)))
      }
      
      // FIXME: don't throw out all loops, might have some that are *not* in levelScope
      currentScope = currentScope.filter { case TTP(_, FatLoop(_,_,_)) => false case _ => true } ::: Wloops

      // schedule and emit
      currentScope = getFatSchedule(currentScope)(result) // clean things up!
     
    }


    // do what super does ...
    super.focusExactScopeFat(currentScope)(result)(body)
  }

}




// trait NestLambdaProg extends Arith with Functions with Print 
// --> from TestCodeMotion.scala

trait FusionProg extends Arith with Loops with Print {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {
    
    val constant = array(100) { i => 1 }

    val linear = array(100) { i => 2*i }

    val affine = array(100) { i => constant.at(i) + linear.at(i) }
    
    def square(x: Rep[Double]) = x*x
    def mean(x: Rep[Array[Double]]) = reduce(x.length) { i => x.at(i) } / x.length
    def variance(x: Rep[Array[Double]]) = reduce(x.length) { i => square(x.at(i)) } / x.length - square(mean(x))
    
    val data = affine
    
    val m = mean(data)
    val v = variance(data)

    print(m)
    print(v)
  }
  
}


class TestFusion extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testFusion1 = {
    withOutFile(prefix+"fusion1") {
      new FusionProg with ArithExp with LoopsExp with PrintExp { self =>
        val codegen = new ScalaGenLoops with ScalaGenArith with ScalaGenPrint with Fusion { val IR: self.type = self }
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion1")
  }

  def testFusion2 = {
    withOutFile(prefix+"fusion2") {
      // LoopsExp2 with ArithExp with PrintExp with FatStuffExp
      new FusionProg with ArithExp with LoopsExp with PrintExp with FatLoopsExp { self =>
        val codegen = new ScalaGenFatLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with Fusion { val IR: self.type = self }
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion2")
  }
 
}