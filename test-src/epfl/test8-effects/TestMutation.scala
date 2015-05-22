package scala.virtualization.lms
package epfl
package test8

import common._
import internal.NestedBlockTraversal
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


/*
  if there's a crash here during compilation, it's likely due to #4363 (need latest scala-virtualized for fix)
*/

trait ArrayMutation extends ArrayLoops {
  
  def infix_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]

  def infix_mutable[T:Manifest](a: Rep[Array[T]]): Rep[Array[T]]
  def infix_clone[T:Manifest](a: Rep[Array[T]]): Rep[Array[T]]
  
}


trait ArrayMutationExp extends ArrayMutation with ArrayLoopsExp {
  
  case class ArrayUpdate[T](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]) extends Def[Unit]
  case class ArrayMutable[T](a: Rep[Array[T]]) extends Def[Array[T]]
  case class ArrayClone[T](a: Rep[Array[T]]) extends Def[Array[T]]
  
  def infix_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]) = reflectWrite(a)(ArrayUpdate(a,i,x))

  def infix_mutable[T:Manifest](a: Rep[Array[T]]) = reflectMutable(ArrayMutable(a))
  def infix_clone[T:Manifest](a: Rep[Array[T]]) = ArrayClone(a)
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => Nil
    case SimpleLoop(s,i, ReduceElem(y)) => syms(y) // could also return zero value
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => syms(y) // could also return zero value
    case ArrayIndex(a,i) => Nil
    case ArrayLength(a) => Nil
    case ArrayUpdate(a,i,x) => Nil // syms(a) <-- any use to return a?
    case ArrayMutable(a) => Nil
    case ArrayClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => syms(y)
    case SimpleLoop(s,i, ReduceElem(y)) => Nil
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => syms(y)
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => Nil
    case ArrayIndex(a,i) => Nil
    case ArrayLength(a) => Nil
    case ArrayUpdate(a,i,x) => syms(x)
    case ArrayMutable(a) => Nil
    case ArrayClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => Nil
    case SimpleLoop(s,i, ReduceElem(y)) => Nil
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => Nil
    case ArrayIndex(a,i) => syms(a)
    case ArrayLength(a) => Nil
    case ArrayUpdate(a,i,x) => Nil
    case ArrayMutable(a) => Nil
    case ArrayClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => Nil
    case SimpleLoop(s,i, ReduceElem(y)) => Nil
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => Nil
    case ArrayIndex(a,i) => Nil
    case ArrayLength(a) => Nil
    case ArrayUpdate(a,i,x) => syms(a)
    case ArrayMutable(a) => syms(a)
    case ArrayClone(a) => syms(a)
    case _ => super.copySyms(e)
  }  
  
  
}

trait ScalaGenArrayMutation extends ScalaGenArrayLoops {
  val IR: ArrayMutationExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayUpdate(a,i,x) => 
      emitValDef(sym, quote(a) + ".update(" + quote(i) + ", " + quote(x) + ")")
    case ArrayMutable(a) =>  
      emitValDef(sym, quote(a) + ".clone // mutable")
    case ArrayClone(a) =>  
      emitValDef(sym, quote(a) + ".clone")
    case _ => super.emitNode(sym, rhs)
  }
}


class TestMutation extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test8-"
  
  trait DSL extends ArrayMutation with Arith with OrderingOps with Variables with IfThenElse with While with RangeOps with Print {
    def zeros(l: Rep[Int]) = array(l) { i => 0 }
    def mzeros(l: Rep[Int]) = zeros(l).mutable
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

    def test(x: Rep[Int]): Rep[Unit]
  }

  trait ImplPrinter extends NestedBlockTraversal {
    import IR._

    override def traverseStm(stm: Stm): Unit = {
      super.traverseStm(stm)
      stm match { 
        case TP(s,d) => printmsg(strDef(s))
        case _ => //
      }
    }
  }

  trait Impl extends DSL with ArrayMutationExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with WhileExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 2

    val codegen = new ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps 
    with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenRangeOps 
    with ScalaGenPrint{val IR: self.type = self}

    val printer = new ImplPrinter{val IR: self.type = self}

    val in = fresh[Int]
    val body = reifyEffects(test(in))

    printer.traverseBlock(body)
    codegen.emitSource(List(in), body, "Test", new PrintWriter(System.out))
  }
  
  def testMutation1 = {
    withOutFile(prefix+"mutation1") {
     // a write operation must unambigously identify the object being mutated
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val vector1 = mzeros(100)
          val vector2 = mzeros(100)
          val a = if (x > 7) vector1 else vector2
          
          a.update(40,40) // error: not clear which object is mutated (vector1 or vector2)

          print(a.at(50))
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation1")
  }

  def testMutation1b = {
    withOutFile(prefix+"mutation1b") {
     // a write operation must unambigously identify the object being mutated
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val vector1 = mzeros(100)
          val vector2 = mzeros(100)
          val a = if (x > 7) vector1 else vector2
          
          val a2 = a.mutable
          a2.update(40,40) // ok: we have made a copy

          print(a2.at(50))
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation1b")
  }

  def testMutation2 = {
    withOutFile(prefix+"mutation2") {
      // an operation that might read from mutable data v will be serialized with all writes to v
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val vector1 = mzeros(100)
          val vector2 = mzeros(100)
          val a = if (x > 7) vector1 else vector2
          
          val x0 = a.at(10)
          
          vector1.update(10,10) // must come after x0
          vector2.update(10,20) // must come after x0
          
          val x1 = a.at(10) // must come after both writes, no cse with x0

          print(x1-x0) // minus should not have effect dep
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation2")
  }


  def testMutation3 = {
    withOutFile(prefix+"mutation3") {
      // vars may not reference mutable objects
      trait Prog extends DSL with LiftVariables {
        def test(x: Rep[Int]) = {
          var a = zeros(100)
          val b = mzeros(100)
          for (i <- 0 until b.length) { // this is also a curious case: range creation must not be reflected
            val x1 = a.at(i)
            b.update(i,8)
            val x2 = a.at(i) // must be cse'd
            a = b // error: here we learn that reads on a would need to be serialized with b but it's too late...
          }
        }
      }      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation3")
  }

  def testMutation3b = {
    withOutFile(prefix+"mutation3b") {
      // vars may not reference mutable objects
      trait Prog extends DSL with LiftVariables {
        def test(x: Rep[Int]) = {
          var a = zeros(100)
          val b = mzeros(100)
          for (i <- 0 until b.length) {
            val x1 = a.at(i)
            b.update(i,8)
            val x2 = a.at(i) // must be cse'd
            a = b.clone // ok: making a copy
          }
        }
      }      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation3b")
  }

  def testMutation4 = {
    withOutFile(prefix+"mutation4") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.mutable // error: internal arrays are mutable on their own
          val x1 = b1.at(5).at(50)
          print(x1)
        }
      }      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4")
  }

  def testMutation4b = {
    withOutFile(prefix+"mutation4b") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.clone
          val b2 = b1.mutable // error: internal arrays are *still* mutable, despite shallow clone
          val x1 = b2.at(5).at(50)
          print(x1)
        }
      }      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4b")
  }

  def testMutation4c = {
    withOutFile(prefix+"mutation4c") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a.clone } // nested array
          val b1 = b.mutable // ok: internal arrays are immutable
          val x1 = b1.at(5).at(50)
          print(x1)
        }
      }      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4c")
  }


  def testMutation5 = {
    withOutFile(prefix+"mutation5") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = zeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.mutable

          val c = mzeros(20)
          b1.update(4,a) // ok: insert immutable array
          b1.update(5,c) // error: cannot insert mutable array
          
          c.update(50,50)
          val x1 = b1.at(5).at(50)
          print(x1)
        }
      }
      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation5")
  }

  def testMutation6 = {
    withOutFile(prefix+"mutation6") {
      // mutate nested object (within an immutable one)
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a } // nested array
          val u = array(10) { i => zeros(100) }
          val c = if (x > 7) b else u

          val x1 = c.at(5).at(50)

          a.update(50,50)
          
          val x2 = c.at(5).at(50) // no cse, must serialize with update to a
          
          print(x2-x1)
        }
      }
      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation6")
  }

  def testMutation7 = {
    withOutFile(prefix+"mutation7") {
      // local variables of primitive type
      trait Prog extends DSL with LiftVariables {
        def test(x0: Rep[Int]) = {
          val x = x0.toDouble // avoid codegen for implicit convert
          var c = 0.0
          while (c < x) {
            c = c + 1
          }
          if (c < x)
            c = 8
          print(c)
        }
      }
      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation7")
  }

}
