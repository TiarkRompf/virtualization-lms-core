package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

// very preliminary!

trait Liveness extends internal.GenericNestedCodegen {
  import IR._

  var defuse: Seq[(Sym[Any],Sym[Any])] = Nil

  override def traverseBlockFocused[A](result: Block[A]): Unit = {
    focusExactScope(result) { levelScope => 
      
      // TODO: what is the intended behavior for uses in innerScope?
      // this will likely depend on the node, i.e. ifThenElse vs. loop

      // a possible first step: handle only straightline code and mark
      // everything used by innerScope as escaping (plus the result) 
      
      def usesOf(s: Sym[Any]): Seq[TP[Any]] = levelScope.flatMap {
        case TP(s1, Reify(rhs1,_,_)) => // reify nodes are eliminated, so we need to find all uses of the reified thing
          if (syms(rhs1).contains(s)) usesOf(s1) else Nil
        case d@TP(_, rhs1) =>
          if (syms(rhs1).contains(s)) List(d) else Nil
      }
      defuse = levelScope.flatMap {
        case TP(sym, Reify(_,_,_)) => Nil
        case TP(sym, rhs) =>
          usesOf(sym).map(d => (sym,d.sym):(Sym[Any],Sym[Any]))
      }

      for (TP(sym, rhs) <- levelScope) {
        emitNode(sym, rhs)

        rhs match {
          case Reify(s, u, effects) =>
          case _ =>
            // remove everything only used here from defuse
            // output dealloc for stuff that goes away
          
            val livebefore = defuse.map(_._1).distinct
            defuse = defuse.filterNot(_._2 == sym)
            val liveafter = defuse.map(_._1).distinct
            val killed = livebefore diff liveafter
            if (killed.nonEmpty) stream.println("// kill: " + killed.map(quote).mkString(", "))
            //stream.println("// def->use: " + defuse.map(p=>quote(p._1)+"->"+quote(p._2)).mkString(", "))
        }

      }
    }
  }


}


trait ScalaGenArraysLiveOpt extends ScalaGenArrays with Liveness {
  val IR: ArraysExp
  import IR._
  
  def canKill(e: Exp[Any], u: Sym[Any]) = {
    !defuse.exists(p => p._1 == e && p._2 != u)
  }

  def tryKill(e: Exp[Any], u: Sym[Any]) = {
    if (!defuse.exists(p => p._1 == e && p._2 != u)) {
      defuse = defuse.filterNot(p => p._1 == e)
      true
    } else false
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayZero(n) =>  
      emitValDef(sym, "new Array[Int](" + quote(n) + ")")
    case ArrayUpdate(a,x,v) =>  
      if (tryKill(a, sym))
        emitValDef(sym, quote(a))
      else
        emitValDef(sym, quote(a) +".clone()")
      stream.println(quote(sym) + "(" + quote(x) + ") = " + quote(v))
    case ArrayPlus(a,b) =>
      if (tryKill(a, sym))
        emitValDef(sym, quote(a))
      else if (canKill(b, sym))
        emitValDef(sym, quote(b))
      else 
        emitValDef(sym, "new Array[Int](" + quote(a) + ".length)")
      stream.println("arrayPlus("+ quote(sym) + "," + quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}




// trait NestLambdaProg extends Arith with Functions with Print 
// --> from TestCodeMotion.scala

trait LiveProg extends Arith with Arrays with Print {
  
  def test(x: Rep[Unit]) = {
    val a = zeroes(100) // allocation

    val ab = a.update(7, 42) // in place, a not used below

    val abc = ab.update(2, 42) // not in place (will copy), b is used again

    val abd = ab.update(4, 42) // in place again

    val e = abc + abd   // in place addition, dealloc one

    print(e) // dealloc the other one
  }
  
}


class TestLiveness extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test7-"
  
  def testLiveness1 = {
    withOutFile(prefix+"liveness1") {
      new LiveProg with ArithExp with ArraysExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenArrays with ScalaGenPrint with Liveness { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"liveness1")
  }

  def testLiveness2 = {
    withOutFile(prefix+"liveness2") {
      new LiveProg with ArithExp with ArraysExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenArraysLiveOpt with ScalaGenPrint with Liveness { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"liveness2")
  }
 
}
