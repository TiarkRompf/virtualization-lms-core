package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}




trait FusionProg21 extends Arith with ArrayLoops with Print with OrderingOps {
  
  def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
  
  def test(x: Rep[Unit]) = {
    
    // there was a bug were this would lead to a recursive schedule:
    // as,bs are fused (no dependencies)
    // cs,ds are fused (no dependencies)
    // but there are cross deps ds->as, bs->cs ...
    
    val cs = array(100) { i => 9.0 }

    val as = array(50) { i => 3.0 }
    val bs = array(50) { i => cs.foo }

    val ds = array(100) { i => as.foo }
    
    print(as)
    print(bs)
    print(cs)
    print(ds)
  }
  
}


trait FusionProg22 extends Arith with ArrayLoops with Print with OrderingOps {
  
  def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
  
  def test(x: Rep[Unit]) = {
    
    // test some nested loops - the inner ones should be moved to the top level and be fused there

    // previously this did not work completely:
    // if an inner loop can be hoisted as a result of fusing two outer loops
    // it would not be considered for outer-level fusion (Wloops is not recalculated)
    // between iterations.
    
    // the question is how far this can be taken: fusion at the innermost level could
    // cause a loop to be moved to the top level, in which case the top level would
    // need to apply fusion all over again.
  

    val as = array(100) { i => 
      array(50) { j => 1.0 }
    }

    val bs = array(100) { i => 
      array(50) { j => 2.0 }
    }

    val cs = array(100) { i => 
      sum(50) { j => 4.0 }
    }

    val ds = array(100) { i => 
      val x = sum(50) { j => as.at(i).at(j) + bs.at(i).at(j) } // this one depends on i, cannot move up
      val y = cs.at(i)
      x + y
    }

    print(as)
    print(bs)
    print(cs)
    print(ds)
  }
  
}


trait FusionProg23 extends Arith with ArrayLoops with Print with OrderingOps {
  
  def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
  
  def test(x: Rep[Unit]) = {
    
  // test some nested loops - this times they are tryly nested (inner ones depend on loop var)
  
    val as = array(100) { i => 
      array(i) { j => 1.0 }
    }

    val bs = array(100) { i => 
      array(i) { j => 2.0 }
    }

    val cs = array(100) { i => 
      sum(i) { j => 4.0 }
    }

    val ds = array(100) { i => 
      val x = sum(i) { j => as.at(i).at(j) + bs.at(i).at(j) }
      val y = cs.at(i)
      x + y
    }


    print(as)
    print(bs)
    print(cs)
    print(ds)
  }
  
}


trait FusionProg24 extends Arith with ArrayLoops with Print with OrderingOps {
  
  def infix_foo(x: Rep[Array[Double]]): Rep[Double] = x.at(0)
  
  def test(x: Rep[Unit]) = {
    
  // test some nested loops
  
    // there is a question related to DCE: we might have fused two loops early, 
    // and by some later fusion one of them becomes dead.
    
    // currently this is not handled because fusion goes from outermost inwards
  
    val as = array(200) { i => 
      1.0
    }

    val bs = array(200) { i => 
      2.0
    }

    val cs = array(100) { i => 
      array(i) { j => as.foo }
    }

    val ds = array(100) { i => 
      array(i) { j => cs.at(i).at(j) - as.foo }
      // this will become as.foo - as.foo = 0  -->  i.e. as becomes dead but is already fused with bs, which is used...
    }
    

    //print(as)
    print(bs)
    //print(cs)
    print(ds)
  }
  
}





class TestFusion2 extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testFusion21 = {
    withOutFile(prefix+"fusion21") {
      new FusionProg21 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion21")
  } 

  def testFusion22 = {
    withOutFile(prefix+"fusion22") {
      new FusionProg22 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion22")
  } 

  def testFusion23 = {
    withOutFile(prefix+"fusion23") {
      new FusionProg23 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion23")
  } 

  def testFusion24 = {
     withOutFile(prefix+"fusion24") {
       new FusionProg24 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>

         override def infix_-(x: Exp[Double], y: Exp[Double]) = if (x == y) {
           println("*** removing self subtraction " + x + " - " + y)
           0
         } else super.infix_-(x,y) //  optimizations to trigger test behavior

         override val verbosity = 1
         val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
           with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
             override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
         codegen.emitSource(test, "Test", new PrintWriter(System.out))
         println("// generated code is not ideal yet. see source for discussion")
       }
     }
     assertFileEqualsCheck(prefix+"fusion24")
   } 
}