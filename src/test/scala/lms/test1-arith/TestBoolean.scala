package scala.lms
package epfl
package test1

import ops._

import scala.reflect.SourceContext

/*
 * Test BooleanOpsExpOpt.
 */

trait BooleanProg extends BooleanOps with MiscOps with LiftBoolean with Equal {
  def test1(i: Rep[Boolean]) = {
    println(!true == false)
    println(!false == true)
    
    println((true && true) == true)
    println((true && false) == false)
    println((false && true) == false)
    println((false && false) == false)
    
    println((true && i) == i)
    println((false && i) == false)
    println((i && true) == i)
    println((i && false) == false)
    
    println((true || i) == true)
    println((false || i) == i)
    println((i || true) == true)
    println((i || false) == i)
    
    println(!i)
    println(i && i)
    println(i || i)
  }
}

class TestBoolean extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test1-"

  def testBoolean1 = {
    withOutFile(prefix+"boolean1"){
      new BooleanProg with BooleanOpsExp with BooleanOpsExpOpt with MiscOpsExp with EqualExp with EqualExpOpt { self =>
        val codegen = new ScalaGenEffect with ScalaGenBooleanOps with ScalaGenMiscOps with ScalaGenEqual { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new java.io.PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"boolean1")
  }
}
