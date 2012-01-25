package scala.virtualization.lms
package epfl
package test4

import common._
import test1._
import test2._
import test3._




/*
  def findAAB(in: Rep[Input]): Rep[Boolean] = in match {
    case 'A'::'A'::'B'::rest => true
    case _::rest => findAAB(rest)
    case _ => false
  }
*/

/*
  def findAAB(in: Rep[Input]): Rep[Boolean] = List.unapply(in) match {
    case Some(('A', r1)) =>
      List.unapply(r1) match {
        case Some(('A', r2)) =>
          List.unapply(r2) match {
            case Some(('B', rest)) =>
              Some(true)
            case None => None
          }
        case None => None
      }
    case None => None
  } orElse {
    List.unapply(in) match {
      case Some((_, r1)) =>
        findAAB(rest)
      case None => None
    }
  } orElse {
    false
  }
*/

/*
def findAAB(in: Rep[Input]): Rep[Boolean] = Some(in) flatMap { r0 => 
  List.unapply(r0) flatMap { p1 =>
    val (h1,r1) = p1
    if (h1 == 'A')
      List.unapply(r1) flatMap { p2 => 
        val (h2,r2) = p2
        if (h2 == 'A')
          List.unapply(r2) flatMap { p3 =>
            val (h3,r3) = p3
            if (h3 == 'B')
              Some(true)
            else None
          }
        else None
      }
    else None
  }
} orElse {
  List.unapply(r0) flatMap { p1 =>
    val (h1,rest) = p1
    findAAB(rest)
  }
} orElse {
  Some(false)
} get;
*/

/*
def findAAB(in: Rep[Input]): Rep[Boolean] = Some(in) flatMap { r0 => 
  List.unapply(r0) flatMap { p1 =>
    val (h1,r1) = p1
    if (h1 == 'A')
      List.unapply(r1) flatMap { p2 => 
        val (h2,r2) = p2
        if (h2 == 'A')
          List.unapply(r2) flatMap { p3 =>
            val (h3,r3) = p3
            if (h3 == 'B')
              Some(true)
            else findAAB(r1)
          }
        else findAAB(r1)
      }
    else findAAB(r1)
  }
} orElse {
  Some(false)
} get;



def findAAB(r0: Rep[Input]): Rep[Boolean] = advance(r0) { (h1,r1) =>
  if (h1 == 'A')
    advance(r1) { (h2,r2) =>
      if (h2 == 'A')
        advance(r2) { (h3,r3) =>
          if (h3 == 'B') true
          else findAAB(r1)
        }
      else findAAB(r1)
    }
  else findAAB(r1)
}


var handler: Rep[(Char,Input)] => Rep[Boolean] = _

def advance(in: Rep[Input])(k: Rep[(Char,Input)] => Rep[Boolean]): Rep[Boolean] = {
  handler = k
}

def emit(c: Char) = {
  handler(c)
}

findAAB(input)

while(true)
  pushChar(c);
*/

/*

  var next: Rep[(Char, Input)] => Rep[Boolean] = { p => ... }
  
  def listUnapplyFlatMap(list, k: (Char,Input)=>Boolean)
  
*/


/*
trait MatcherNewProg { this: Matching with ListMatch with Equal =>  
  
  abstract class Regex
  
  case object Fail extends Regex
  case object Eps extends Regex
  case object Wildcard extends Regex
  
  case class Atom(c: Char) extends Regex

  case class And(a: Regex, b: Regex) extends Regex

  case class Or(a: Regex, b: Regex) extends Regex

  case class Star(a: Regex) extends Regex

  case class DIF(c: Rep[Boolean], a: Regex, b: Regex) extends Regex


  def derive(r: Regex, c: Rep[Char]): Regex = r match {
    case Fail => Fail
    case Eps => Fail
    case Wildcard => Eps
    case Atom(c1) => DIF(c1 == c, Eps, Fail) // CAVEAT: dynamic test!
    case Or(a,b) => Or(derive(a,c), derive(b,c))
    case And(a,b) => Or(And(derive(a,c), b), And(delta(a), derive(b,c)))
    case Star(a) => And(derive(a,c), Star(a))
  }
  
  def delta(r: Regex): Regex = r match {
    case Fail => Fail
    case Eps => Eps
    case Wildcard => Fail
    case Atom(c1) => Fail
    case Or(a,b) => Or(delta(a), delta(a))
    case And(a,b) => Or(delta(a), delta(a))
    case Star(a) => Eps
  }


  def testMatching(x: Rep[List[Char]]) = {
    val exp = And(Star(Wildcard), And(Atom('A'), And(Atom('A'), Atom('B'))))
    
    def unfold(exp: Regex)(x: Rep[List[Char]]): Rep[Boolean] = exp match {
      case Fail => false
      case DIF(c,a,b) => if (c) unfold(a) else unfold(b)
      case e => 
        if (x.isEmpty) {
          delta(exp) == Eps
        } else {
          val hd = x.head
          val tl = x.tail
          val d = derive(exp, hd)
          unfold(d, tl)
        }
      
      unfold(exp, x)
    }
    
    unfold(exp)(x)
    
  }

}
*/

trait MatcherNewProgTrivialA {

  type IO = Unit
  
  var threads: List[Char => IO] = Nil
  
  def findAAB(): IO = {
    advance { a1 => 
      if (a1 == 'A') 
        advance { a2 => 
          if (a2 == 'A') 
            advance { b =>
              if (b == 'B')
                success() }}}

    advance { _ => findAAB() } // in parallel...
  }



  def advance(f: Char => IO): IO = {
    threads = threads :+ f
  }

  var rest: List[Char] = Nil

  def success(): IO = {
    println("found at " + rest)
  }


  def testMatching() = {

    val xs = List('X','A','Z','A','A','B','W','A','A','A','A','B','Q')

    findAAB()
    
    rest = xs
    while (rest.nonEmpty) {
      var c = rest.head
      rest = rest.tail
      
      val current = threads
      threads = Nil
    
      if (current.isEmpty)
        println("done at " + rest)
    
      for (t <- current) {
        t(c)
      }
      
    }
    
  }

}


trait MatcherNewProgTrivialB {

  case class PF[T](f: T => T) { def apply(x:T) = f(x) }
  
  case class TH(f: Char => PF[TH]) { def apply(x:Char) = f(x) }
  
  type IO = PF[TH]
  
  def findAAB(): IO = {
    val t1 = advanceIf(true) { a1 => 
        advanceIf(a1 == 'A') { a2 => 
          advanceIf(a2 == 'A') { b =>
              if (b == 'B')
                success() 
              else 
                fail() }}}

    val t2 = advanceIf(true) { _ => findAAB() } // in parallel...
    PF[TH] { z => t2(t1(z)) }
  }



  def advanceIf(cond: Boolean)(f: Char => IO): IO = {
    if (cond)
      PF[TH] { z => TH { c => val t1 = z(c); val t2 = f(c); PF[TH] { z2 => t2(t1(z2)) } } } // execute f on next character ...
    else
      PF[TH] { z => z }
  }

  var rest: List[Char] = Nil

  def success(): IO = {
    println("found at " + rest)
    PF[TH] { z => z }
  }

  def fail(): IO = {
    PF[TH] { z => z }
  }


  def testMatching() = {

    val xs = List('X','A','Z','A','A','B','W','A','A','A','A','B','Q')

    val emptyStepper = TH { c => PF[TH] { z => z } }

    var stepperBuilder: IO = findAAB()
    
    rest = xs
    while (rest.nonEmpty) {
      val c = rest.head
      rest = rest.tail
      
      val stepper = stepperBuilder(emptyStepper)
      
      stepperBuilder = stepper(c)
      
    }
    
  }

}


trait MatcherNewProgTrivialC {

  abstract class IO
  
  case class ID(s:String) extends IO
  case class OR(a: IO, b: IO) extends IO
  case class ADD(f: Char => IO) extends IO
  
  def findAAB(): IO = {
    val t1 = advanceIf(true) { a1 => 
        advanceIf(a1 == 'A') { a2 => 
          advanceIf(a2 == 'A') { b =>
              if (b == 'B')
                success() 
              else 
                fail() }}}

    val t2 = advanceIf(true) { _ => findAAB() } // in parallel...
    OR(t1,t2)
  }



  def advanceIf(cond: Boolean)(f: Char => IO): IO = {
    if (cond)
      ADD(f) // execute f on next character ...
    else
      ID("")
  }

  var rest: List[Char] = Nil

  def success(): IO = {
    println("found at " + rest)
    ID("found at " + rest)
  }

  def fail(): IO = {
    ID("fail")
  }



  def testMatching() = {

    val xs = List('X','A','Z','A','A','B','W','A','A','A','A','B','Q')

    var script: IO = findAAB()
    
    
    def interpret(p: IO): Char => IO = p match {
      case ID(s) => { c => ID("") }
      case OR(a,b) => { c => OR(interpret(a)(c), interpret(b)(c)) }
      case ADD(f) => f
    }
    
    rest = xs
    while (rest.nonEmpty) {
      val c = rest.head
      rest = rest.tail
      
      val stepper = interpret(script)
      
      script = stepper(c)
      
    }
    
  }

}






trait MatcherNewProgA { this: Arith with Functions with Equal with IfThenElse =>

  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A=>B]) {
    def apply(x:Rep[A]): Rep[B] = doApply(f, x)
  }
  implicit def lam[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  //implicit def toLambdaOps[A,B](f: Rep[A=>B]) = new LambdaOps(f)

  implicit def toDouble(f: Rep[Int]): Rep[Double] = f.asInstanceOf[Rep[Double]]
  
  // -- end boilerplate


  type IO = Rep[Unit]
  
  var threads: List[(Rep[Boolean], Rep[Char] => IO)] = Nil
  
  def findAAB(): IO = {
    advanceIf(unit(true)) { a1 => 
        advanceIf(a1 == 'A') { a2 => 
            advanceIf(a2 == 'A') { b =>
              if (b == 'B')
                success() }}}

    advanceIf(unit(true)) { _ => findAAB() } // in parallel...
  }


  def advanceIf(c: Rep[Boolean])(f: Rep[Char] => IO): IO = {
    threads = threads :+ (c,f)
  }

  def success(): IO = {
    collectall(List(unit("found")))
  }


  type TT = (Char => Any) // should be recursive Char => TT
  type Thread = Rep[Char]=>Rep[Unit]
  type CondThread = (Rep[Boolean], Thread)

  def collectall(in: List[Rep[Any]]): Rep[Unit]
  def printL(in: Rep[Any]): Rep[Unit]

  def testMatching(xs: Rep[List[Char]]) = {

    findAAB()
    
    var collect: List[Rep[Any]] = Nil
    
    // return a stepper function that runs the given list of threads
    def stepthreads(inthr: List[Thread])(handler: List[CondThread]=> Rep[Any]): Rep[TT] = lam { c =>
      threads = Nil
      val xs = for (t <- inthr) yield {
        t(c)
      }
      val next = threads
      collectall(handler(threads)::xs)
    }
    
    // run all combinations of conditions
    def combinations[A,B:Manifest](rest: List[(Rep[Boolean], List[A])], 
      acc: List[A] = Nil)(handler: List[A] => Rep[B]): Rep[B] = rest match {
      
      case Nil =>
        handler(acc)
        
      case (a,b)::xs =>
        println("switch cond "+a)
        if (a)
          combinations(xs, acc ++ b)(handler)
        else
          combinations(xs, acc)(handler)
    }

    // utility
    def groupByFirst[A,B](in: List[(A, B)]): List[(A, List[B])] = in.groupBy(_._1).toList.map(p => (p._1, p._2.map(_._2)))

    def iterate(current: List[CondThread]): Rep[Any] = {
      if (collect.length == 50)
        return collectall(((lam { c => collectall(Nil) }):Rep[TT]) :: Nil)
    
      // produce one lambda for each possible combination

      combinations(groupByFirst(current)) { inthr => stepthreads(inthr) { next => iterate(next) } }
    }

    
    val fun = iterate(threads)
    collectall(fun :: collect)
    
    // TODO: respect conditional control flow! there is no single follow state, but it depends on if/else stms
  }


}


trait MemoUtils extends util.ClosureCompare {
  val m = new scala.collection.mutable.HashMap[Any, Any]

  def memo[T](x:T, hint: String = "", trace: Boolean = false): T = {
    println("-- lookup " + hint + " " + x.getClass.getName)
    if (trace) {
      println(canon(x))
    }
    m.getOrElseUpdate(canon(x),{ println("-- not found"); x }).asInstanceOf[T]
  }
  
  def canon(x:Any): String = {
    val s = new java.io.ByteArrayOutputStream()
    val o = new java.io.ObjectOutputStream(s)
    o.writeObject(x)
    s.toString("ASCII")
  }
}


trait MatcherNewProgB { this: Arith with Functions with Equal with IfThenElse =>

  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A=>B]) {
    def apply(x:Rep[A]): Rep[B] = doApply(f, x)
  }
  implicit def lam[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  //implicit def toLambdaOps[A,B](f: Rep[A=>B]) = new LambdaOps(f)

  implicit def toDouble(f: Rep[Int]): Rep[Double] = f.asInstanceOf[Rep[Double]]
  
  // -- end boilerplate


  def collectall(in: List[Rep[Any]]): Rep[Unit]
  def printL(in: Rep[Any]): Rep[Unit]

  // -- end util


  abstract class EX
  
  case class ID(s:String, e: Rep[Unit]) extends EX
  case class ADD(f: Rep[Char] => IO) extends EX
  case class GUARD(cond: Rep[Boolean], e: IO) extends EX
  
  type IO = List[EX]
    
  def findAAB(): IO = {
    advance { a1 => 
      guard(a1 == 'A') {
        advance { a2 => 
          guard(a2 == 'X') {
            advance { b =>
              guard(b == /*a2*/'B') { //cannot re
                success()
    }}}}}} ++
    advance { _ => findAAB() } // in parallel...
  }

  
  def guard(cond: Rep[Boolean])(e: IO): IO = {
    List(GUARD(cond, e))
  }
  
  def advance(f: Rep[Char] => IO): IO = {
    List(ADD(f))
  }

  def advanceIf(cond: Rep[Boolean])(f: Rep[Char] => IO): IO = {
    guard(cond) { advance(f) }
  }

  def exec(f: => Rep[Unit]): IO = {
    List(ID("exec", f))
  }

  def success(): IO = exec {
    println("XXX success")
    collectall(List(unit("found!")))
  }


  def interpret[A:Manifest](xs: IO)(k: List[Rep[Char]=>IO] => Rep[A]): Rep[A] = xs match {
    case Nil => k(Nil)
    case ID(s,e)::rest =>
      val rec = interpret(rest)(k)
      collectall(List(unit("exec"), e, rec)).asInstanceOf[Rep[A]]
    case GUARD(cond, e)::rest =>
      if (cond)
        interpret(e:::rest)(k)
      else
        interpret(rest)(k)
    case ADD(f)::rest =>
      interpret(rest)(acc => k(acc :+ f))
  }


  // --- test

  var nest: Int = 0

  def checkNest(b: => Rep[Unit]): Rep[Unit] = {
    if (nest == 7) collectall(List(unit("overflow"+nest)))
    else {
      nest += 1
      val r = b  
      nest -= 1
      r
    }
  }


  def testMatching(xs: Rep[List[Char]]) = {

    def iterate(script: IO): Rep[Unit] = {
      println("iterate " + nest + " " + script)
  
      // see: SI-5367 - Closures hang on to free variables of all their parents, causing subtle memory leaks
      // this also means equality using canonicalize may be problemtic for nested closures
  
      def continue(funs: List[Rep[Char]=>IO]) = lam { c: Rep[Char] =>
        println("within arg " + c)
        val next = funs.flatMap(_.apply(c))
        
        // next should not reference c ...
        
        val r = iterate(next)
        println("leave arg " + c)
        r
      }
  
      checkNest {
        interpret(script) { funs => 
          println(funs)
          collectall(List(unit("goto"), continue(funs)))
        }
      }
  
    }

    val script = findAAB()
    iterate(script)
  }

}


trait MatcherNewProg { this: Arith with Functions with Equal with IfThenElse =>

  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A=>B]) {
    def apply(x:Rep[A]): Rep[B] = doApply(f, x)
  }
  implicit def lam[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  //implicit def toLambdaOps[A,B](f: Rep[A=>B]) = new LambdaOps(f)

  implicit def toDouble(f: Rep[Int]): Rep[Double] = f.asInstanceOf[Rep[Double]]
  
  // -- end boilerplate


  def collectall(in: List[Rep[Any]]): Rep[Unit]
  def printL(in: Rep[Any]): Rep[Unit]

  // -- end util


  // -- begin general automaton
  
  type GIO = List[GTrans] // unstaged Automaton type, also State type (State = list of possible transitions)
  
  case class GTrans(f: Char => List[GTrans])
  
  def gtrans(f: Rep[Char] => Rep[GIO]): Rep[GIO] = collectall(List(unit("gtrans"), lam(f))).asInstanceOf[Rep[GIO]]
  def gcall(f: Rep[GIO], c: Rep[Char]): Rep[GIO] = collectall(List(unit("gcall"), f, c)).asInstanceOf[Rep[GIO]]
  
  def gguard(c: Rep[Boolean], s: Boolean = false)(d: Rep[GIO]): Rep[GIO] = if (c) d else collectall(Nil).asInstanceOf[Rep[GIO]]

  def gstop(): Rep[GIO] = collectall(List(unit("stop"))).asInstanceOf[Rep[GIO]]
  def gor[A](a: Rep[List[A]], b: Rep[List[A]]): Rep[List[A]] = collectall(List(unit("++"),a,b)).asInstanceOf[Rep[List[A]]]
  
  def gfindAAB(): Rep[GIO] = {
    gor(gtrans { a1 =>
      gguard (a1 == 'A') { 
        gtrans { a2 =>
          gguard (a2 == 'A') { 
            gtrans { a3 =>
              gguard (a3 == 'B', true) {
                gstop()
    }}}}}},
    gtrans { _ => gfindAAB() }) // in parallel...
  }
  
  def ginterpret(xs: Rep[GIO], cin: Rep[Char]): Rep[GIO] = gcall(xs, cin) // xs flatMap { f => f(cin) }
  
  // -- end general automaton



  // -- begin NFA
  
  type IO = List[Trans]
  
  case class Trans(c: Option[Char], e: Option[Rep[Unit]], s: () => IO)
  
  def trans(c: Option[Char])(s: () => IO): IO = List(Trans(c, None, s))
  
  
  def interpret[A:Manifest](xs: IO, cin: Rep[Char])(k: IO => Rep[A]): Rep[A] = xs match {
    case Nil => k(Nil)
    case Trans(Some(c), e, s)::rest =>
      if (cin == c) {
        val xs1 = rest collect { case Trans(Some(`c`) | None,e,s) => Trans(None,e,s) }
        val rec = interpret(xs1, cin)(acc => k(acc ++ s()))
        e map (dfa_exec(_, rec)) getOrElse rec
      }
      else {
        val xs1 = rest filter { case Trans(Some(`c`),_,_) => false case _ => true }
        interpret(xs1, cin)(k)
      }
    case Trans(None, e, s)::rest =>
      val rec = interpret(rest,cin)(acc => k(acc ++ s()))
      e map (dfa_exec(_, rec)) getOrElse rec
  }

  // -- end NFA

  // -- begin DFA
  
  def dfa_exec[A:Manifest](e: Rep[Any], rec: Rep[A]): Rep[A] = collectall(List(unit("exec"), e, rec)).asInstanceOf[Rep[A]]
  
  type DIO = Rep[Unit]
  
  //case class DTrans(c: Option[Char], s: () => DIO)
  
  def dfa_trans(f: Rep[Char] => DIO): DIO = collectall(List(unit("dfagoto"), lam(f)))
  
  
  // -- end DFA


  def findAAB(): IO = {
    guard(Some('A')) {
      guard(Some('A')) {
        guard(Some('B'), true) {
          stop()
    }}} ++
    guard(None) { findAAB() } // in parallel...
  }

  
  def guard(cond: Option[Char], found: Boolean = false)(e: => IO): IO = {
    List(Trans(cond, if (found) Some(unit("found").asInstanceOf[Rep[Unit]]) else None, () => e))
  }

  def stop(): IO = Nil


  // --- test

  var nest: Int = 0

  def checkNest(b: => Rep[Unit]): Rep[Unit] = {
    if (nest == 7) collectall(List(unit("overflow"+nest)))
    else {
      nest += 1
      val r = b  
      nest -= 1
      r
    }
  }


  /*
  TODO:
  implement dfa_trans
  automata 
  compile and run
  benchmark
  */


  def testMatchingG(xs: Rep[List[Char]]) = {

    def iterate: Rep[GIO => Unit] = lam { state: Rep[GIO] => dfa_trans { c: Rep[Char] =>
      
      iterate(gcall(state,c))
    }}
    iterate(gfindAAB())

  }

  def testMatching(xs: Rep[List[Char]]) = {

    def iterate(state: IO): DIO = dfa_trans { c: Rep[Char] =>
      interpret(state, c) { next =>
        iterate(next)
      }
    }
    
    iterate(findAAB())


    // see: SI-5367 - Closures hang on to free variables of all their parents, causing subtle memory leaks
    // this also means equality using canonicalize may be problemtic for nested closures

    /*def continue(funs: IO) = dfa_trans { c: Rep[Char] =>
      println("within arg " + c)
      
      val r = interpret(funs,c) { next => 
        println(next)
        iterate(next)
      }

      println("leave arg " + c)
      r
    }

    def iterate(script: IO): Rep[Unit] = {
      println("iterate " + nest + " " + script)
  
      checkNest {
        continue(script)
      }  
    }

    val script = findAAB()
    iterate(script)*/
  }

}




class TestMatcherNew extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test4-"
  
  def testMatcherTriv1 = {
    withOutFile(prefix+"matchertriv1") {
      object MatcherProgExp extends MatcherNewProgTrivialA
      MatcherProgExp.testMatching()
    }
    assertFileEqualsCheck(prefix+"matchertriv1")
  }

  def testMatcherTriv2 = {
    withOutFile(prefix+"matchertriv2") {
      object MatcherProgExp extends MatcherNewProgTrivialB
      MatcherProgExp.testMatching()
    }
    assertFileEqualsCheck(prefix+"matchertriv2")
  }

  def testMatcherTriv3 = {
    withOutFile(prefix+"matchertriv3") {
      object MatcherProgExp extends MatcherNewProgTrivialC
      MatcherProgExp.testMatching()
    }
    assertFileEqualsCheck(prefix+"matchertriv3")
  }


  // William Cook's test case: (a | b)∗ (abb | (a + b))     [sic! + vs |]
  // compare with dk.brics.automaton, input size 10^7
  // civet 573 ms, dk.brics.automaton 816 ms



  def testMatcher1 = {
    withOutFile(prefix+"matchernew1") {
      object MatcherProgExp extends MatcherNewProg 
      with ArithExpOpt with EqualExpOpt with BooleanOpsExp with IfThenElseExpOpt 
      //with FunctionExpUnfoldRecursion 
      with FunctionsExternalDef1 /* was 2 */ {
        case class Result(in: List[Exp[Any]]) extends Def[Unit] //FIXME
        def collectall(in: List[Rep[Any]]): Rep[Unit] = Result(in)
        def printL(in: Rep[Any]): Rep[Unit] = /*reflectEffect*/(Result(List(in))) //FIXME violate ordering
        override val verbosity = 1
      }
      import MatcherProgExp._


      val f = (x:Rep[List[Char]]) => testMatching(x)
      val g = (x:Rep[List[Char]]) => testMatchingG(x)
      
      val p = new ScalaGenArith with ScalaGenEqual with 
        ScalaGenIfThenElse with ScalaGenFunctionsExternal { val IR: MatcherProgExp.type = MatcherProgExp 
          
          import java.io.PrintWriter
          override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
            case Result(xs) => emitValDef(sym, "(" + (xs map {quote}).mkString(",") + ") // DUMMY")
            //case Lambda(f) => emitNode(sym, DefineFun(f)) // FIXME
            case _ => 
              //println("emit super on " + sym + " = " + rhs + " / " + rhs.getClass)
              super.emitNode(sym, rhs)
          }
        }
      p.emitSource(f, "Match", new java.io.PrintWriter(System.out))
      p.emitSource(g, "MatchG", new java.io.PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"matchernew1")
    //assertFileEqualsCheck(prefix+"matcher1-dot")
  }

}