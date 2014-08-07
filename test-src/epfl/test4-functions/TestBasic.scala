package scala.virtualization.lms
package epfl
package test4

import common._
import test1._
import test2._
import test3._

import org.scala_lang.virtualized.virtualize

@virtualize
trait BasicProg { this: Arith with Functions with Equal with IfThenElse =>
  def f(b: Rep[Boolean]) = {
    if (b) 1 else 2
  }
}


class TestBasic extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test4-"

  def testBasic1 = {
    withOutFile(prefix+"basic1") {
      object BasicProgExp extends BasicProg
        with ArithExpOpt with EqualExp with IfThenElseExp 
        with FunctionsExternalDef1
      import BasicProgExp._

      val p = new ScalaGenArith with ScalaGenEqual with 
        ScalaGenIfThenElse with ScalaGenFunctionsExternal { val IR: BasicProgExp.type = BasicProgExp }
      p.emitSource(f, "Basic", new java.io.PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"basic1")
  }
}
