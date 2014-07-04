package scala.virtualization.lms
package epfl
package test4

import common._
import internal.GraphVizExport
import test1._
import test2._
import test3._


trait FacProg { this: Arith with Matching with Extractors =>

  def fac(n: Rep[Double]): Rep[Double] = n switch {
    case n if n guard(0) => unit(1.0) + unit(0.0)
  } orElse {
    case n => n * fac(n-1.0)
  }

}

trait FacProg2 { this: Arith with Functions with Equal with IfThenElse =>

  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A=>B]) {
    def apply(x:Rep[A]): Rep[B] = doApply(f, x)
  }
  implicit def lam[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  //implicit def toLambdaOps[A,B](f: Rep[A=>B]) = new LambdaOps(f)


  def fac: Rep[Double=>Double] = lam { n =>
    if (n == 0) 1.0 else n * fac(n - 1.0)
  }

}



class TestFac extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test4-"

  def testFac1 = {
    withOutFile(prefix+"fac1") {
      object FacProgExp extends FacProg with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with FunctionsExternalDef0
      import FacProgExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz with FunctionsGraphViz { val IR: FacProgExp.type = FacProgExp }
      p.emitDepGraph(r, prefix+"fac1-dot")
    }
    assertFileEqualsCheck(prefix+"fac1")
    assertFileEqualsCheck(prefix+"fac1-dot")
  }

  def testFac2 = {
    withOutFile(prefix+"fac2") {
      object FacProgExp extends FacProg with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with FunctionExpUnfoldAll with FunctionsExternalDef2
//        with FunctionsExternalDef01
        with FunctionsExternalDef0
      import FacProgExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz with FunctionsGraphViz { val IR: FacProgExp.type = FacProgExp }
      p.emitDepGraph(r, prefix+"fac2-dot")
    }
    assertFileEqualsCheck(prefix+"fac2")
    assertFileEqualsCheck(prefix+"fac2-dot")
  }

  def testFac3 = {
    withOutFile(prefix+"fac3") {
      object FacProgExp extends FacProg with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with FunctionExpUnfoldRecursion 
        with FunctionsExternalDef0
      import FacProgExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz with FunctionsGraphViz { val IR: FacProgExp.type = FacProgExp }
      p.emitDepGraph(r, prefix+"fac3-dot")
    }
    assertFileEqualsCheck(prefix+"fac3")
    assertFileEqualsCheck(prefix+"fac3-dot")
  }

  def testFac4 = {
    withOutFile(prefix+"fac4") {
      object FacProgExp extends FacProg2
        with ArithExpOpt with EqualExp with IfThenElseExp
        with FunctionExpUnfoldRecursion 
        with FunctionsExternalDef2
      import FacProgExp._

      val r = { val x = fresh; fac(x) + fac(2*x) }
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new FunctionsGraphViz { val IR: FacProgExp.type = FacProgExp }
      p.emitDepGraph(r, prefix+"fac4-dot")
    }
    assertFileEqualsCheck(prefix+"fac4")
    assertFileEqualsCheck(prefix+"fac4-dot")
  }
  
  def testFac5 = {
    withOutFile(prefix+"fac5") {
      object FacProgExp extends FacProg2
        with ArithExpOpt with EqualExp with IfThenElseExp 
        with FunctionExpUnfoldRecursion 
        with FunctionsExternalDef2
      import FacProgExp._

      val f = (x:Rep[Double]) => fac(x) + fac(2*x)
      println(globalDefs.mkString("\n"))
      println(f)
      val p = new ScalaGenArith with ScalaGenEqual with 
        ScalaGenIfThenElse with ScalaGenFunctionsExternal { val IR: FacProgExp.type = FacProgExp }
      p.emitSource1(f, "Fac", new java.io.PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"fac5")
  }

  def testFac6 = {
    withOutFile(prefix+"fac6") {
      object FacProgExp extends FacProg2
        with ArithExpOpt with EqualExp with IfThenElseExp 
        with FunctionsRecursiveExp
      import FacProgExp._

      val f = (x:Rep[Double]) => fac(x) + fac(2*x)
      println(globalDefs.mkString("\n"))
      println(f)
      val p = new ScalaGenArith with ScalaGenEqual with 
        ScalaGenIfThenElse with ScalaGenFunctions { val IR: FacProgExp.type = FacProgExp }
      p.emitSource1(f, "Fac", new java.io.PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"fac6")
  }
}

