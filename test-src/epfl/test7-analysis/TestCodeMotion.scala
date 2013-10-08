package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait NestLambdaProg1 extends Arith with Functions with Print { // also used by TestLambdaLift
  
  def test(x: Rep[Unit]) = {
    val f = doLambda { x: Rep[Double] =>
      val g = doLambda { y: Rep[Double] =>
        print("yo")
        y + (unit(4.0) * unit(3.0))
      }
      g
    }
    f
  }
  
}

trait NestCondProg2 extends Arith with Functions with IfThenElse with Print {
  
  /* Previously this program exhibited behavior that is likely undesired in many
  cases. The definition of f was moved *into* g and into the conditional.
  The doLambda in the else branch would not be hoisted out of g either.
  
  Although there are situations where this particular kind of code motion
  is an improvement (namely, if the probability of y == true is very low
  and the else branch would be cheap).
  */
  
  
  def test(x: Rep[Unit]) = {
    val f = doLambda { x: Rep[Double] => 2 * x }
    
    val g = doLambda { y: Rep[Boolean] =>
      print("yo")
      if (y)
        f
      else
        doLambda { x: Rep[Double] => x + 1 }
    }
    g
  }
  
}


trait NestCondProg3 extends Arith with Functions with IfThenElse with Print {
  
  def test(x: Rep[Unit]) = {
    val f = if (unit(true)) doLambda { x: Rep[Double] => 2 * x } else doLambda { x: Rep[Double] => 4 * x }
    
    val g = doLambda { y: Rep[Boolean] =>
      print("yo")
      if (y) {
        print("then")
        f
      } else {
        print("else")
        if (unit(false)) doLambda { x: Rep[Double] => x + 1 } else doLambda { x: Rep[Double] => x + 2 }
      }
    }
    g
  }
  
}

trait NestCondProg4 extends Arith with Functions with IfThenElse with Print {
  
  def test(x: Rep[Unit]) = {
    val g = doLambda { y: Rep[Double] =>
      if (unit(true)) {
        val x = y + 1.0
        print(x)
        ()
      } else {
      }
    }
    g
  }
  
}


trait NestCondProg5 extends Arith with Functions with IfThenElse with Print {
  
  def test(x: Rep[Unit]) = {
    if (unit(true)) {
      // should place 7 + 9 here
      doLambda { y: Rep[Double] =>
        print(unit(7.0) + unit(9.0))
      }
    } else {
    }
  }
  
}


// FIXME: this one doesn't work yet!!!
/*trait NestCondProg6 extends Arith with Functions with IfThenElse with Print {

  def test(x: Rep[Unit]) = {
    val z = unit(7.0) + unit(9.0) // should move into the conditional (but isn't currently)
    val x = if (unit(true)) {
      print(z)
    } else {
    }
    doLambda { y: Rep[Boolean] => 
      print(x)
    }
  }
  
}*/


trait NestCondProg7 extends Arith with OrderingOps with Functions with IfThenElse with Print {

  def test(x: Rep[Unit]) = {    
    doLambda { y: Rep[Double] => 
      if (y < 100) {
        val z = y + unit(9.0) // should stay inside conditional: 
                              // apparently z was moved up because it is also used in the lambda (z+u)
        doLambda { u: Rep[Double] =>
          z + u
        }
      } else {
      }
    }
  }
  
}

/*
seems to be another incarnation of test6

trait NestCondProg8 extends Arith with OrderingOps with Functions with IfThenElse with Print {
  
  // FIXME

  def test(x: Rep[Unit]) = {    
    doLambda { y: Rep[Double] => 
      if (y < 100) {
        val z = y + unit(9.0) // should stay inside conditional
        z + unit(1.0)
      } else {
        val z = y + unit(9.0) // should stay inside conditional, although used again
        z + unit(2.0)
      }
    }
  }
  
}
*/


class TestCodemotion extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testCodemotion1 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion1") {
      new NestLambdaProg1 with ArithExp with FunctionsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource1(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion1")
  }

  def testCodemotion2 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion2") {
      new NestCondProg2 with ArithExp with FunctionsExp with IfThenElseExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource1(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion2")
  }

  def testCodemotion3 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion3") {
      new NestCondProg3 with ArithExp with FunctionsExp with IfThenElseExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource1(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion3")
  }

  def testCodemotion4 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion4") {
      new NestCondProg4 with ArithExp with FunctionsExp with IfThenElseExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource1(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion4")
  }

  def testCodemotion5 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion5") {
      new NestCondProg5 with ArithExp with FunctionsExp with IfThenElseExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource1(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion5")
  }

 /* def testCodemotion6 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion6") {
      new NestCondProg6 with ArithExp with FunctionsExp with IfThenElseExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource1(test, "Test", new PrintWriter(System.out))
        println("// NOTE: generated code is not ideal yet (x1=7+9 should be moved inside conditional). see source for discussion.")
      }
    }
    assertFileEqualsCheck(prefix+"codemotion6")
  }*/

  def testCodemotion7 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion7") {
      new NestCondProg7 with ArithExp with OrderingOpsExp with FunctionsExp with IfThenElseExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenOrderingOps with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource1(test, "Test", new PrintWriter(System.out))
        println("// was a Delite issue (Scratchpad in optiml-beta).")
      }
    }
    assertFileEqualsCheck(prefix+"codemotion7")
  }


}
