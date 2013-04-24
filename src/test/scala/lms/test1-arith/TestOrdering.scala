package scala.lms
package epfl
package test1

import ops._

import scala.reflect.SourceContext

/*
 * Test OrderingOpsExpOpt.
 */

trait OrderingProg extends OrderingOps with MiscOps with LiftBoolean with Equal {
  def test1(i: Rep[Int]) = {
    println((unit(1) < unit(2)) == true)
    println((unit(1) <= unit(2)) == true)
    println((unit(1) > unit(2)) == false)
    println((unit(1) >= unit(2)) == false)
    println((unit(1) equiv unit(2)) == false)
    println((unit(1) min unit(2)) == unit(1))
    println((unit(1) max unit(2)) == unit(2))
    
    println((unit(1) < unit(1)) == false)
    println((unit(1) <= unit(1)) == true)
    println((unit(1) > unit(1)) == false)
    println((unit(1) >= unit(1)) == true)
    println((unit(1) equiv unit(1)) == true)
    println((unit(1) min unit(1)) == unit(1))
    println((unit(1) max unit(1)) == unit(1))

    println((unit(1) < unit(0)) == false)
    println((unit(1) <= unit(0)) == false)
    println((unit(1) > unit(0)) == true)
    println((unit(1) >= unit(0)) == true)
    println((unit(1) equiv unit(0)) == false)
    println((unit(1) min unit(0)) == unit(0))
    println((unit(1) max unit(0)) == unit(1))

    println((unit(1) < i))
    println((unit(1) <= i))
    println((unit(1) > i))
    println((unit(1) >= i))
    println((unit(1) equiv i))
    println((unit(1) min i))
    println((unit(1) max i))
  }
}

class TestOrdering extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test1-"

  def testOrdering1 = {
    withOutFile(prefix+"ordering1"){
      new OrderingProg with OrderingOpsExp with OrderingOpsExpOpt with MiscOpsExp with EqualExp with EqualExpOpt { self =>
        val codegen = new ScalaGenEffect with ScalaGenOrderingOps with ScalaGenMiscOps with ScalaGenEqual { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new java.io.PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"ordering1")
  }
}
