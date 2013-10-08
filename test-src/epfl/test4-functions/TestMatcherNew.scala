package scala.virtualization.lms
package epfl
package test4

import common._
import test1._
import test2._
import test3._

/*
TODO:
+implement dfa_trans
+automata 
+compile and run

benchmark
cleanup
parsing / tree construction?
http parser??
iteratee interface?
*/


/*
type NAutomaton[I,O] = List[NState[I,O]]

case class NState[I,O](out: O, next: I => NAutomaton[I,O])


def convertNAtoDA[I,O](na: NAutomaton[I,O])(m: Monoid[O]): Automaton[I,O] =
  Automaton(m reduce (na map (_.out)), x => na flatMap (_.next(x)))


type NAUT = NAutomaton[Char,Boolean]

def advance(f: Char => NAUT) = List(NState(false, f))

def guard(c: Boolean)(k: => NAUT) = if (c) k else List()

def found = List(NState(true, x => Nil))

def findAAB(): NAUT = {
  advance { a1 =>
    guard(a1 == 'A') {
      advance { a2 =>
        guard(a2 == 'A') {
          advance { a3 =>
            guard(a3 == 'B') {
              done(true)
  }}}}}} ++
  advance { _ => gfindAAB() } // in parallel...
}
*/






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






trait MatcherNewProgA extends Util { this: Arith with Functions with Equal with IfThenElse =>

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


trait MatcherNewProgB extends Util { this: Arith with Functions with Equal with IfThenElse =>

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
              guard(b == /*a2*/'B') { //cannot reach back: *finite* automaton!
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

// see: SI-5367 - Closures hang on to free variables of all their parents, causing subtle memory leaks
// this also means equality using canonicalize may be problemtic for nested closures

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




trait Util extends Base with Arith with Functions {
  
  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A=>B]) {
    def apply(x:Rep[A]): Rep[B] = doApply(f, x)
  }
  implicit def lam[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  //implicit def toLambdaOps[A,B](f: Rep[A=>B]) = new LambdaOps(f)

  implicit def toDouble(f: Rep[Int]): Rep[Double] = f.asInstanceOf[Rep[Double]]
  
  def collectall(in: List[Rep[Any]]): Rep[Unit]
  def protect[A:Manifest](x: Rep[A], in: List[Rep[Any]]): Rep[A]
}



trait MatcherNewProg extends DFAOps with GAOps with NFAtoDFA with GAtoDA with Util { this: Arith with Functions with Equal with IfThenElse =>

  // -- begin general automaton
  
  def gfindAAB(): Rep[GIO] = {
    gor(gtrans { a1 =>
      gguard (a1 == 'A') { 
        gtrans { a2 =>
          gguard (a2 == a1/*'A'*/) { 
            gtrans { a3 =>
              gguard (a3 == 'B', true) {
                //gstop()
                gtrans(unit(List(1))) { a4 => gstop() }
    }}}}}},
    gtrans { _ => gfindAAB() }) // in parallel...
  }
  
  
  // -- begin DFA
  
  def findAAB(): NIO = {
    guard(Some('A')) {
      guard(Some('A')) {
        guard(Some('B'), true) {
          stop()
    }}} ++
    guard(None) { findAAB() } // in parallel...
  }


  def testMatchingG() = {

    convertGAtoDA(gfindAAB())
  }

  def testMatching() = {

    convertNFAtoDFA(findAAB())
  }
}




class TestMatcherNew extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test4-"
  
  trait DSL extends MatcherNewProg with Arith with Functions with Equal with IfThenElse {
    def bare[T:Manifest](x: Rep[Any], f: String => String): Rep[T]
    def test(x: Rep[Unit]): DIO
  }

  trait RunTest {
    val fc: Unit => Automaton[Char,List[Any]]
    val input = List('X','A','B','Z','A','A','B','W','A','A','A','A','B','Q')
    def runtest() {
      var state = fc()

      var idx = 0
      input foreach { c =>
        println("idx:   " + idx)
        println("out:   " + state.out)
        println("char:  " + c)

        idx += 1
        state = state.next(c)
      }
      
      println("idx:   " + idx)
      println("out:   " + state.out)
    }
  }

  trait Impl extends DSL with RunTest with DFAOpsExp with GAOpsExp
    with ArithExpOpt with EqualExpOpt with TupleOpsExp with OrderingOpsExp 
    with BooleanOpsExp with IfThenElseExpOpt with IfThenElseFatExp with ListOpsExp
    with SplitEffectsExpFat // temporary!
    //with FunctionExpUnfoldRecursion 
    with FunctionsExternalDef1 /* was 2 */ 
    with CompileScala { q =>
      case class Result(in: List[Exp[Any]]) extends Def[Unit] //FIXME
      case class ResultA[A](x: Exp[A], in: List[Exp[Any]]) extends Def[A] //FIXME
      case class Bare[T](x: Exp[Any], f: String => String) extends Def[T] //FIXME
      def collectall(in: List[Rep[Any]]): Rep[Unit] = Result(in)
      def protect[A:Manifest](x: Exp[A], in: List[Rep[Any]]): Rep[A] = ResultA(x,in)
      def bare[T:Manifest](x: Exp[Any], f: String => String): Exp[T] = Bare[T](x,f)
      //def printL(in: Rep[Any]): Rep[Unit] = /*reflectEffect*/(Result(List(in))) //FIXME violate ordering
      //override val verbosity = 1
      object codegen extends ScalaGenArith with ScalaGenEqual with ScalaGenListOps with ScalaGenTupleOps
          with ScalaGenIfThenElseFat with ScalaGenSplitEffects with ScalaGenOrderingOps
          with ScalaGenDFAOps with ScalaGenGAOps
          with ScalaGenFunctionsExternal { 
        val IR: q.type = q
        //import IR._   
        import java.io.PrintWriter
        override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
          case Result(xs) => emitValDef(sym, "(" + (xs map {quote}).mkString(",") + ") // DUMMY")
          case ResultA(x,xs) => emitValDef(sym, quote(x) + " // " + (xs map quote).mkString(","))
          case Bare(x,f) => emitValDef(sym, f(quote(x)))
          case _ => 
            super.emitNode(sym, rhs)
        }
      }
      
      val f = (x:Rep[Unit]) => test(x)
      codegen.emitSource1(f, "Match", new java.io.PrintWriter(System.out))
      val fc = compile1(f)
      runtest()
  }




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


  def testMatcherNew1 = withOutFileChecked(prefix+"matchernew1") {
    trait Prog extends DSL {
      def test(x: Rep[Unit]) = {
        
        def findAAB(): NIO = {
          guard(Some('A')) {
            guard(Some('A')) {
              guard(Some('B'), true) {
                stop()
          }}} ++
          guard(None) { findAAB() } // in parallel...
        }
        
        convertNFAtoDFA(findAAB())
      }
    }
    new Prog with Impl
  }

  def testMatcherNew1b = withOutFileChecked(prefix+"matchernew1b") {
    trait Prog extends DSL {
      def test(x: Rep[Unit]) = {
        
        def star(c: Option[Char])(k: => NIO): NIO = {
          guard(c)(star(c)(k)) ++ k
        }
        
        def findAAB(): NIO = {
          star(None) {
            guard(Some('A')) {
              guard(Some('A')) {
                guard(Some('B'), true) {
                  stop()
          }}}}
        }
        
        convertNFAtoDFA(findAAB())
      }
    }
    new Prog with Impl
  }


  def testMatcherNew2 = withOutFileChecked(prefix+"matchernew2") {
    trait Prog extends DSL {
      def test(x: Rep[Unit]) = {
        
        def gfindAAB(): Rep[GIO] = {
          gor(gtrans { a1 =>
            gguard (a1 == 'A') { 
              gtrans { a2 =>
                gguard (a2 == a1/*'A'*/) { 
                  gtrans { a3 =>
                    gguard (a3 == 'B', true) {
                      //gstop()
                      gtrans(unit(List(1))) { a4 => gstop() }
          }}}}}},
          gtrans { _ => gfindAAB() }) // in parallel...
        }
        
        convertGAtoDA(gfindAAB())        
      }
    }
    new Prog with Impl
  }


  def testMatcherNew2b = withOutFileChecked(prefix+"matchernew2b") {
    trait Prog extends DSL {
      def test(x: Rep[Unit]) = {
        
        def star(p: Rep[Char] => Rep[Boolean])(k0: => Rep[GIO]): Rep[GIO] = {
          val k = k0
          gor(gtrans(a => gguard(p(a))(star(p)(k))), k)
        }
        
        def gfindAAB(): Rep[GIO] = {
          star(a => unit(true))(gtrans { a1 =>
            gguard (a1 == 'A') { 
              gtrans { a2 =>
                gguard (a2 == a1/*'A'*/) { 
                  gtrans { a3 =>
                    gguard (a3 == 'B'/*, true*/) {
                      //gstop()
                      gtrans(unit(List(1))) { a4 => gstop() }
          }}}}}})
        }
        
        convertGAtoDA(gfindAAB())        
      }
    }
    new Prog with Impl
  }


  def testCounter1 = withOutFileChecked(prefix+"counter1") {
    trait Prog extends DSL with ListOps with Arith {

      def protect[A:Manifest](a:Rep[A],b:Rep[Any]): Rep[A] = protect(a, Seq(b).toList)

      def test(x: Rep[Unit]) = {
        
        def count: Rep[Double => DfaState] = lam { s: Rep[Double] =>
          dfa_trans(NewList(s)) { a1 =>
            count(protect(s,a1) + 1)
          }
        }
        
        count(0.0)
      }
    }
    new Prog with Impl
  }


  trait StreamHelpers extends DSL with ListOps with Arith with BooleanOps with TupleOps with OrderingOps with StepperOps {
    def countChar(c:Rep[Char]) = Stream[Char] filter (_ == c) into fcount

    def pcount(n: Rep[Double]) = Prod[Double](0, _ < n, _ + 1)
    def ptails[T:Manifest](xs: Rep[List[T]]) = Prod[List[T]](xs, !list_isEmpty(_), list_tail(_))
    def plist[T:Manifest](xs: Rep[List[T]]) = ptails(xs) map (list_head(_))

    def fcount[T:Manifest] = Foreach[T,Double](0, c => s => s + 1)
    def fwhile[T:Manifest](p: Rep[T] => Rep[Boolean]) = Foreach[T,Boolean](unit(true), c => s => if (s && p(c)) unit(true) else unit(false))
    def flast[T:Manifest](s: Rep[T]) = Foreach[T,T](s, c => s => c)
    def fcollect[T:Manifest] = Foreach[T,List[T]](NewList(), c => s => list_concat(s,NewList(c)))

    def switcher[T:Manifest,S1:Manifest,S2:Manifest,O1:Manifest,O2:Manifest](s: Stream[T])(p: Rep[T]=>Rep[Boolean])(a: Stepper2[T,S1,O1], b: Stepper2[O1,S2,O2]) = 
      Stepper2[T,(S1,S2),O2]((a.s,b.s), ss => b.cond(ss._2), c => ss => if (p(c)) (a.s, b.yld(a.res(ss._1))(ss._2)) else (a.yld(c)(ss._1), ss._2), ss => b.res(ss._2))
      
  }

  def testCounter2 = withOutFileChecked(prefix+"counter2") {
    trait Prog extends DSL with StreamHelpers {

      def test(x: Rep[Unit]): DIO = {
        
        val nested = Stream[Char] flatMap { c => plist(NewList(c,c,c)) } into fcollect

        val listhandler = Stream[Char] split(_ == 'A', fcount) filter (_ != 0) into fcount
        
        val countA = countChar('A') zip listhandler
        val countB = countChar('B');
        val countC = countChar('C');
        
        val d = countA zip countB zip countC zip nested
        
        stepthrough(d)
      }
    }
    new Prog with Impl
  }

  def testStream1 = withOutFileChecked(prefix+"stream1") {
    trait Prog extends DSL with StreamHelpers {
      
      def iseven(n: Rep[Double]) = bare[Boolean](n,s=>s+"%2 == 0")
      
      def test(x: Rep[Unit]): DIO = {
        
        val nums = pcount(8)
        val even = pcount(8) /*until (_ == 5.0)*/ filter (n => iseven(n))
        
        val pairs = nums zip even
        
        val nested = Stream[Char] flatMap { c => pairs } into fcollect

        stepthrough(nested)
      }
    }
    new Prog with Impl
  }
  

/*
  val req = "PUT /file HTTP/1.1\r\n"+
  "Host: example.com\r"+
  "User-agent: X\n"+ 
  "content-type: text/plain\r\n"+ 
  "\r\n"+
  "1C\r\n"+
  "body line 1lf body line 2\r\n"+
  "7\r\n"+
  "body li\r\n"+
  "37\r\n"+
  "ne 3cr body line 4\n body line 5\n\r\n"+
  "0crlfcrlf"

  assume input is given as buffers: List[String]
  
  val chars = Stream[String] flatMap (buf => pstring(buf)) 
  
  val headerLines = Stream[Char] split ("\r\n|\r|\n", fcollect) until (_.isEmpty) into handleHeaderLine

  val chunked = ...
  
  
  def parse = buffers into (chars into (parseHeader andThen parseBody))

*/

}
