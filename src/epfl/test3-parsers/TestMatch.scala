package scala.virtualization.lms
package epfl
package test3

import test1._
import test2._


trait TestMatch { this: Matching with Extractors =>
  
  case class Success(x: Int)
  
  object SuccessR {
    def apply(x: Rep[Int]): Rep[Success] = construct(classOf[Success], Success.apply, x)
    def unapply(x: Rep[Success]): Option[Rep[Int]] = deconstruct(classOf[Success], Success.unapply, x)
  }
  
  object :!: {
    def apply[A](x: Rep[A], xs: Rep[List[A]]) = construct(classOf[::[A]], (::.apply[A] _).tupled, tuple(x, xs))
//    def unapply[A](x: Rep[::[A]]) = deconstruct2(classOf[::[A]], ::.unapply[A], x) // doesn't work: hd is private in :: !
    def unapply[A](x: Rep[::[A]]) = deconstruct(classOf[::[A]], (x: ::[A]) => Some(x.head, x.tail), x)
  }
  
  def infix_unapply(o: SuccessR.type, x: Rep[Success]): Option[Rep[Int]] = deconstruct(classOf[Success], Success.unapply, x)
  // doesn't work...
  
  def unit[A](x: A): Rep[A]
  
  def test(x: Rep[Success]): Rep[String] = x switch {
    case SuccessR(x) if x guard 7 => unit("yes")
  } orElse {
    case SuccessR(x) => unit("maybe")
  } orElse {
    case _ => unit("no")
  }
  
  def testXX(x: Rep[Success]): Rep[String] = _match(x)({
    case SuccessR(x) if x guard 7 => unit("yes")
  },{
    case SuccessR(x) => unit("maybe")
  },{
    case _ => unit("no")
  })
}



object TestTestMatch {
  
  def main(args: Array[String]) = {
    
/*
    println {
      object TestMatchString extends TestMatch with Matching with Extractors with MatchingExtractorsRepString
      import TestMatchString._
      test(SuccessR("7"))
    }
*/    

    println {
      object TestMatchExp extends TestMatch with Matching with Extractors
        with MatchingExtractorsExp with FunctionsExpUnfoldAll
        with ExtractorsGraphViz with DisableCSE
      import TestMatchExp._

      case class Result(x:Any) extends Def[Any]

      val r = reifyEffects(test(fresh))
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(toAtom(Result(r)), "test3-match1-dot")
    }
    
    println {
      object TestMatchExp extends TestMatch with Matching with Extractors
        with MatchingExtractorsExpOpt with FunctionsExpUnfoldAll
        with ExtractorsGraphViz
      import TestMatchExp._

      case class Result(x:Any) extends Def[Any]

      val r = reifyEffects(test(fresh))
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(toAtom(Result(r)), "test3-match2-dot")
    }

  }

}