package scala.virtualization.lms
package epfl
package test8

import common._
import internal.{NestedTraversal}
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

trait WhileExpOptSpeculative extends WhileExp {
  
  case class PreviousIteration() extends Def[Unit]
  
  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {

    def reflectDummy(u: Summary) = reflectEffect(PreviousIteration(), u)

    val c = reifyEffectsHere(cond)
    val ce = summarizeEffects(c)
    val a = reifyEffectsHere { reflectDummy(ce); body }
    val ae = summarizeEffects(a)
    
    val c1 = reifyEffectsHere { reflectDummy(ae); cond }
    val ce1 = summarizeEffects(c1)
    val a1 = reifyEffectsHere { reflectDummy(ce1); body }
    val ae1 = summarizeEffects(a1)

    val c2 = reifyEffectsHere { reflectDummy(ae1/*.lastIteration*/); cond }
    val ce2 = summarizeEffects(c2)
    val a2 = reifyEffectsHere { reflectDummy(ce2); body }
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


trait ScalaGenWhileOptSpeculative extends ScalaGenWhile {
  val IR: WhileExpOptSpeculative
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case PreviousIteration() =>
      // dummy ...
      //emitValDef(sym, "() // dummy placeholder for previous iteration")
    case _ => super.emitNode(sym, rhs)
  }

}



trait OrderingOpsExpOpt extends OrderingOpsExp {
  override def ordering_lt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = (lhs,rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].lt(a,b))
    case _ => super.ordering_lt(lhs,rhs)
  }
  override def ordering_gt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = (lhs,rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].gt(a,b))
    case _ => super.ordering_gt(lhs,rhs)
  }
}


class TestSpeculative extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test8-"
  
  trait DSL extends ArrayMutation with Arith with OrderingOps with BooleanOps with LiftVariables with IfThenElse with While with RangeOps with Print {
    def zeros(l: Rep[Int]) = array(l) { i => 0 }
    def mzeros(l: Rep[Int]) = zeros(l).mutable
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

    def test(x: Rep[Int]): Rep[Any]
  }
  trait Impl extends DSL with ArrayMutationExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
      with EqualExpOpt with VariablesExpOpt 
      with IfThenElseExpOpt with WhileExpOptSpeculative with RangeOpsExp with PrintExp { self => 
    override val verbosity = 2
    val codegen = new ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhileOptSpeculative with ScalaGenRangeOps 
      with ScalaGenPrint with LivenessOpt { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }
  
  def testSpeculative1 = {
    withOutFile(prefix+"speculative1") {
     // test simple copy propagation through variable
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var x = 7
          
          if (x > 3) // should remove conditional
            print(x)
          else
            print("no")
          print(x)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"speculative1")
  }

  def testSpeculative1b = {
    withOutFile(prefix+"speculative1b") {
     // test simple copy propagation through variable
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var x = 7
          
          if (x > 3) // should remove conditional
            x = 5
          else
            print("no")
          
          print(x) // should be const 5
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"speculative1b")
  }

  def testSpeculative1c = {
    withOutFile(prefix+"speculative1c") {
     // test simple copy propagation through variable
      trait Prog extends DSL {
        def test(y: Rep[Int]) = {
          var x = 7
          
          if (x > y) // cannot remove conditional
            x = 5
          else
            print("no")
          
          print(x) // should be var read
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"speculative1c")
  }
  
  def testSpeculative3 = {
    withOutFile(prefix+"speculative3") {
     // test simple copy propagation through variable
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var x = 7
          var c = 0.0
          while (c < 10) {
            print(x) // should be const 7
            print(c)
            var z = 2
            c = c + 1
            print(z) // should be const 2
          }          
          print(x) // should be const 7
          print(c)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"speculative3")
  }

  def testSpeculative4 = {
    withOutFile(prefix+"speculative4") {
     // test simple copy propagation through variable
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = 0.0
          while (c < 10) {
            print("booooring!")
          }
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"speculative4")
  }
  
  def testSpeculative5 = {
    withOutFile(prefix+"speculative5") {
     // test simple copy propagation through variable
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var x = 7
          var c = 0.0
          while (c < 10) {
            if (x < 10)
              print("test")
            print(x)
            c += 1
          }
          print(x)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"speculative5")
  }

}