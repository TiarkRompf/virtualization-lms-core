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


trait TestFac { this: Arith with Matching with Extractors =>

  def fac(n: Rep[Double]): Rep[Double] = n switch {
    case n if n guard(0) => unit(1.0) + unit(0.0)
  } orElse {
    case n => n * fac(n-1.0)
  }

}


trait ExtractorsGraphViz2 extends ExtractorsGraphViz { this: MatchingExtractorExp with FunctionsExternalDef0 =>
  
  override def emitNode(sym: Sym[_], rhs: Def[_], stream: java.io.PrintWriter) = rhs match {
/*
    case Result(x) =>
      super.emitNode(sym, rhs, stream)
      stream.println("shape=point")
*/
    case Test(x, y) =>
      super.emitNode(sym, rhs, stream)
      stream.println("color=red")
    case DefineFun(x) =>
      super.emitNode(sym, rhs, stream)
      stream.println("color=green")
      stream.println("style=filled")
    case Apply(x, y) =>
      super.emitNode(sym, rhs, stream)
      stream.println("color=blue")
      stream.println("style=filled")
    case _ =>
      super.emitNode(sym, rhs, stream)
//      stream.println("shape=point")
  }
  
/*
  override def emitDeps(sym: Sym[_], rhs: Def[_], deps: List[Sym[Any]], stream: java.io.PrintWriter) = rhs match {
    case AndAlso(x, effects) =>
      super.emitDeps(sym, rhs, deps -- effects.asInstanceOf[List[Sym[Any]]], stream) // TODO: cast
      for (dep <- effects) {
        stream.println("\"" + dep + "\" -> \"" + sym + "\"" + " [color=red]")
      }
    case _ =>
      super.emitDeps(sym, rhs, deps, stream)
  }
*/

}


object Test {
  
  def main(args: Array[String]) = {
    
    println {
      object TestFacExp extends TestFac with Matching with Extractors
        with ArithExpOpt with MatchingExtractorExpOpt
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import TestFacExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test4-fac1-dot")
    }

    println {
      object TestFacExp extends TestFac with Matching with Extractors
        with ArithExpOpt with MatchingExtractorExpOpt
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
        with ArithExpOpt with MatchingExtractorExpOpt
        with FunctionExpUnfoldRecursion 
        with ExtractorsGraphViz2 with FunctionsExternalDef0
      import TestFacExp._

      val r = fac(fresh)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test4-fac3-dot")
    }

    println {
      object TestMatcherExp extends TestMatcher with Matching with Extractors with ListMatch
        with MatchingExtractorExpOpt
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