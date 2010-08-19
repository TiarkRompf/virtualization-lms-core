package scala.virtualization.lms
package epfl
package test3

import test1._
import test2._


trait TestParsers extends Parsers { this: Matching with Extractors =>
  
  def toElem(c: Char): Elem
  
  def acceptChar(c: Char) = acceptElem(toElem(c))
  def acceptString(s: String) = acceptElems(s.toList.map(toElem))
  
  val scala = acceptString("scala")
  val rules = acceptString("rules")
  val rocks = acceptString("rocks")
  val blank = acceptChar(' ')

  val phrase1 = seq(scala, seq(blank, rules))
  val phrase2 = seq(scala, seq(blank, rocks))

  val head = alt(phrase1, phrase2)
  
}

object TestTestParsers {
  
  def main(args: Array[String]) = {
    
    println {
      object TestParsersExp extends TestParsers with Matching with Extractors
        with MatchingExtractorsExpOpt with FunctionsExpUnfoldAll // with ControlOpt
        with ExtractorsGraphViz with DisableCSE {
          type Elem = Char
          def toElem(c: Char) = c
        }
      import TestParsersExp._

      case class Result(x:Any) extends Def[Any]

      val r = reifyEffects(head(fresh))
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(toAtom(Result(r)), "test3-parse1-dot")
    }

    println {
      object TestParsersExp extends TestParsers with Matching with Extractors 
        with MatchingExtractorsExpOpt with FunctionsExpUnfoldAll // with ControlOpt
        with ExtractorsGraphViz {
          type Elem = Char
          def toElem(c: Char) = c
        }
      import TestParsersExp._

      case class Result(x:Any) extends Def[Any]

      val r = reifyEffects(head(fresh))
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(toAtom(Result(r)), "test3-parse2-dot")
    }
  }

}