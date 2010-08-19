package scala.virtualization.lms
package epfl
package test4

import common._
import test1._
import test2._
import test3._


trait TestFac { this: Arith with Matching with Extractors =>

  def fac(n: Rep[Double]): Rep[Double] = n switch {
    case n if n guard(0) => unit(1.0) + unit(0.0)
  } orElse {
    case n => n * fac(n-1.0)
  }

}


object TestTestFac {
  
  def main(args: Array[String]) = {
    
    println {
      object TestFacExp extends TestFac with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import TestFacExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test4-fac1-dot")
    }

    println {
      object TestFacExp extends TestFac with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with FunctionExpUnfoldAll with FunctionsExternalDef2
//        with FunctionsExternalDef01
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import TestFacExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test4-fac2-dot")
    }

    println {
      object TestFacExp extends TestFac with Matching with Extractors
        with ArithExpOpt with MatchingExtractorsExpOpt
        with FunctionExpUnfoldRecursion 
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import TestFacExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test4-fac3-dot")
    }
  }
}