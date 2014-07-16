package scala.virtualization.lms
package epfl
package test4

import test2._
import test3._


trait ListMatch extends Extractors {
  
  object :!: {
    def apply[A:Manifest](x: Rep[A], xs: Rep[List[A]]) = construct(classOf[::[A]], (::.apply[A] _).tupled, tuple(x, xs))
//    def unapply[A](x: Rep[::[A]]) = deconstruct2(classOf[::[A]], ::.unapply[A], x) // doesn't work: hd is private in :: !
    def unapply[A:Manifest](x: Rep[::[A]]) = deconstruct2(classOf[::[A]], (x: ::[A]) => Some(x.head, x.tail), x)
  }

}


trait MatcherProg { this: Matching with ListMatch =>
  
  type Input = List[Char]
  
  def find(p: Input, s: Rep[Input]) = loop(p,s,p,s)

  def loop(p0: Input, s0: Rep[Input], pr: Input, sr: Rep[Input]): Rep[Boolean] = p0 match {
    case p::pp =>
      s0 switch {
        case (s: Rep[Char]):!:(ss: Rep[Input]) if s guard p => // matched p
          println("match")
          loop(pp,ss,pr,sr)
      } orElse {
        case s:!:ss => // no match for p
          println("no match")
          next(pr,sr)
      } orElse {
        case _ => unit(false)
      } end
    case _ => unit(true)
  }
    
  def next(p: Input, s: Rep[Input]): Rep[Boolean] = s switch {
    case s:!:(ss: Rep[Input]) => loop(p,ss,p,ss)
  } orElse {
    case _ => unit(false)
  } end

}






class TestMatcher extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test4-"
  
  def testMatcher1 = {
    withOutFile(prefix+"matcher1") {
      object MatcherProgExp extends MatcherProg with Matching with Extractors with ListMatch
        with MatchingExtractorsExpOpt
        with FunctionExpUnfoldRecursion with FunctionsExternalDef2
      import MatcherProgExp._

      val r = find("AAB".toList, fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExtractorsGraphViz with FunctionsGraphViz { val IR: MatcherProgExp.type = MatcherProgExp }
      p.emitDepGraph(r, prefix+"matcher1-dot")
    }
    assertFileEqualsCheck(prefix+"matcher1")
    assertFileEqualsCheck(prefix+"matcher1-dot")
  }

}
