package scala.lms
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

trait ParsersProgExp0 extends common.BaseExp with ParsersProg { this: Matching with Extractors =>

  implicit def inputTyp: Typ[Input] = listTyp
  implicit def resultTyp: Typ[ParseResult] = ManifestTyp(implicitly)
  implicit def successTyp: Typ[Success] = ManifestTyp(implicitly)
  implicit def failureTyp: Typ[Failure] = ManifestTyp(implicitly)

  implicit def listTyp[T:Typ]: Typ[List[T]] = {
    implicit val ManifestTyp(m) = typ[T]
    ManifestTyp(implicitly)
  }
  implicit def consTyp[T:Typ]: Typ[::[T]] = {
    implicit val ManifestTyp(m) = typ[T]
    ManifestTyp(implicitly)
  }
}

class TestParsers extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test3-"
  
  def testParse1 = {
    withOutFile(prefix+"parse1") {
      object ParsersProgExp extends ParsersProgExp0 with Matching with Extractors
        with MatchingExtractorsExpOpt with FunctionsExpUnfoldAll with FlatResult // with ControlOpt
        with DisableCSE {
          type Elem = Char
          implicit val mE: Typ[Char] = ManifestTyp(implicitly)
          def toElem(c: Char) = c
        }
      import ParsersProgExp._

      val r = reifyEffects(head(fresh[Input]))
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz { val IR: ParsersProgExp.type = ParsersProgExp }
      p.emitDepGraph(result(r), prefix+"parse1-dot")
    }
    assertFileEqualsCheck(prefix+"parse1")
    assertFileEqualsCheck(prefix+"parse1-dot")
  }

  def testParse2 = {
    withOutFile(prefix+"parse2") {
      object ParsersProgExp extends ParsersProgExp0 with Matching with Extractors 
        with MatchingExtractorsExpOpt with FunctionsExpUnfoldAll with FlatResult // with ControlOpt
        {
          type Elem = Char
          implicit val mE: Typ[Char] = ManifestTyp(implicitly)
          def toElem(c: Char) = c
        }
      import ParsersProgExp._

      val r = reifyEffects(head(fresh[Input]))
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz { val IR: ParsersProgExp.type = ParsersProgExp }
      p.emitDepGraph(result[Unit](r), prefix+"parse2-dot")
    }
    assertFileEqualsCheck(prefix+"parse2")
    assertFileEqualsCheck(prefix+"parse2-dot")
  }

}
