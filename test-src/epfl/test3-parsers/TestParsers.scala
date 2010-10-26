package scala.virtualization.lms
package epfl
package test3

import test1._
import test2._


trait ParsersProg extends Parsers { this: Matching with Extractors =>
  
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

class TestParsers extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test3-"
  
  def testParse1 = {
    withOutFile(prefix+"parse1") {
      object ParsersProgExp extends ParsersProg with Matching with Extractors
        with MatchingExtractorsExpOpt with FunctionsExpUnfoldAll // with ControlOpt
        with DisableCSE {
          type Elem = Char
          def toElem(c: Char) = c
        }
      import ParsersProgExp._

      case class Result(x:Any) extends Def[Any]

      val r = reifyEffects(head(fresh))
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz { val IR: ParsersProgExp.type = ParsersProgExp }
      p.emitDepGraph(toAtom(Result(r)), prefix+"parse1-dot")
    }
    assertFileEqualsCheck(prefix+"parse1")
    assertFileEqualsCheck(prefix+"parse1-dot")
  }

  def testParse2 = {
    withOutFile(prefix+"parse2") {
      object ParsersProgExp extends ParsersProg with Matching with Extractors 
        with MatchingExtractorsExpOpt with FunctionsExpUnfoldAll // with ControlOpt
        {
          type Elem = Char
          def toElem(c: Char) = c
        }
      import ParsersProgExp._

      case class Result(x:Any) extends Def[Any]

      val r = reifyEffects(head(fresh))
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz { val IR: ParsersProgExp.type = ParsersProgExp }
      p.emitDepGraph(toAtom(Result(r)), prefix+"parse2-dot")
    }
    assertFileEqualsCheck(prefix+"parse2")
    assertFileEqualsCheck(prefix+"parse2-dot")
  }

}