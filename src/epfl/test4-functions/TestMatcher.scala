package scala.virtualization.lms
package epfl
package test4

import test2._
import test3._


trait ListMatch extends Extractors {
  
  object :!: {
    def apply[A](x: Rep[A], xs: Rep[List[A]]) = construct(classOf[::[A]], (::.apply[A] _).tupled, tuple(x, xs))
//    def unapply[A](x: Rep[::[A]]) = deconstruct2(classOf[::[A]], ::.unapply[A], x) // doesn't work: hd is private in :: !
    def unapply[A](x: Rep[::[A]]) = deconstruct2(classOf[::[A]], (x: ::[A]) => Some(x.head, x.tail), x)
  }

}


trait TestMatcher { this: Matching with ListMatch =>
  
  def unit[A](x: A): Rep[A]
  
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




trait ExtractorsGraphViz2 extends ExtractorsGraphViz { this: MatchingExtractorsExp with FunctionsExternalDef0 =>
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: java.io.PrintWriter) = rhs match {
/*
    case Result(x) =>
      super.emitNode(sym, rhs)
      stream.println("shape=point")
*/
    case Test(x, y) =>
      super.emitNode(sym, rhs)
      stream.println("color=red")
    case DefineFun(x) =>
      super.emitNode(sym, rhs)
      stream.println("color=green")
      stream.println("style=filled")
    case Apply(x, y) =>
      super.emitNode(sym, rhs)
      stream.println("color=blue")
      stream.println("style=filled")
    case _ =>
      super.emitNode(sym, rhs)
//      stream.println("shape=point")
  }
  
/*
  override def emitDeps(sym: Sym[_], rhs: Def[_], deps: List[Sym[Any]], stream: java.io.PrintWriter) = rhs match {
    case AndAlso(x, effects) =>
      super.emitDeps(sym, rhs, deps -- effects.asInstanceOf[List[Sym[Any]]]) // TODO: cast
      for (dep <- effects) {
        stream.println("\"" + dep + "\" -> \"" + sym + "\"" + " [color=red]")
      }
    case _ =>
      super.emitDeps(sym, rhs, deps)
  }
*/

}


object TestTestMatcher {
  
  def main(args: Array[String]) = {
    
    println {
      object TestMatcherExp extends TestMatcher with Matching with Extractors with ListMatch
        with MatchingExtractorsExpOpt
        with FunctionExpUnfoldRecursion with FunctionsExternalDef2
        with ExtractorsGraphViz2
      import TestMatcherExp._

      val r = find("AAB".toList, fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test4-matcher1-dot")
    }

  }

}