package scala.lms
package epfl
package test4

import common._
//import internal.GraphVizExport
import test1._
import test2._
import test3._


trait AckProg { this: LiftPrimitives with PrimitiveOps with Functions with Equal with IfThenElse =>

  class LambdaOps[A:Typ,B:Typ](f: Rep[A=>B]) {
    def apply(x:Rep[A]): Rep[B] = doApply(f, x)
  }
  implicit def lam[A:Typ,B:Typ](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  //implicit def toLambdaOps[A,B](f: Rep[A=>B]) = new LambdaOps(f)


  def ack(m: Double): Rep[Int=>Int] = lam { n =>
    if (m == 0) n+1 else
    if (n == 0) ack(m-1)(1) else
    ack(m-1)(ack(m)(n-1))
  }
  
  /* Example due to Neil Jones, via Oleg on LtU (http://lambda-the-ultimate.org/node/4039#comment-61431)
  
  ack(2,n) should specialize to:
  
  ack_2(n) =  if n=0 then ack_1(1) else  ack_1(ack_2(n-1))
  ack_1(n) =  if n=0 then ack_0(1) else  ack_0(ack_1(n-1))
  ack_0(n) =  n+1
  
  this actually "just works"
  
  */

}



class TestAck extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test4-"

  def testAck1 = {
    withOutFile(prefix+"ack1") {
      object AckProgExp extends AckProg
        with EqualExp with IfThenElseExp with LiftPrimitives with PrimitiveOpsExpOpt
        with FunctionsExternalDef1
      import AckProgExp._

      val f = (x:Rep[Int]) => ack(2)(x)
      //val r = ack(2)(fresh)
      //println(globalDefs.mkString("\n"))
      //println(r)
      //val p = new ExtractorsGraphViz with FunctionsGraphViz { val IR: AckProgExp.type = AckProgExp }
      //p.emitDepGraph(r, prefix+"ack1-dot")
      val p = new ScalaGenPrimitiveOps with ScalaGenEqual with 
        ScalaGenIfThenElse with ScalaGenFunctionsExternal { val IR: AckProgExp.type = AckProgExp }
      p.emitSource(f, "Ack", new java.io.PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"ack1")
    //assertFileEqualsCheck(prefix+"ack1-dot")
  }


}
