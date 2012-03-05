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



class TestFusion2 extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testFusion21 = {
    withOutFile(prefix+"fusion21") {
      new FusionProg21 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion21")
  } 

}