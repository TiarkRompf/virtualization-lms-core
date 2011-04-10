package scala.virtualization.lms
package epfl
package test8

import common._
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
  
}

trait ScalaGenArrayMutation extends ScalaGenArrayLoops {
  val IR: ArrayMutationExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
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
  
  val prefix = "test-out/epfl/test8-"
  
  trait DSL extends ArrayMutation with Arith with OrderingOps with Variables with IfThenElse with RangeOps with Print {
    def zeros(l: Rep[Int]) = array(l) { i => 0 }
    def mzeros(l: Rep[Int]) = zeros(l).mutable

    def test(x: Rep[Int]): Rep[Any]
  }
  trait Impl extends DSL with ArrayMutationExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 2
    val codegen = new ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
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

  def testMutation4 = {
    withOutFile(prefix+"mutation4") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.mutable // error: internal arrays are mutable
          val x1 = b1.at(5).at(50)
          print(x1)
        }
      }      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4")
  }

  def testMutation5 = {
    withOutFile(prefix+"mutation5") {
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = zeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.mutable

          val c = mzeros(20)
          b1.update(4,a) // ok
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

}