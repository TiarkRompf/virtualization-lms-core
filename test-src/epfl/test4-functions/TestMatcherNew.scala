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




abstract class DfaState {
  def hasFlag(x: Any): Boolean
  def next(c: Char): DfaState
}
case class dfaFlagged(flag: Any, link: DfaState) extends DfaState {
  def hasFlag(x: Any) = x == flag || link.hasFlag(x)
  def next(c: Char) = link.next(c)
}
case class dfaTrans(f: Char => DfaState) extends DfaState {
  def hasFlag(x: Any) = false
  def next(c: Char) = f(c)
}

trait DFAOps extends Base {

  type DIO = Rep[DfaState]

  def dfa_flagged(e: Rep[Any])(rec: DIO): DIO
  def dfa_trans(f: Rep[Char] => DIO): DIO 
}


trait DFAOpsExp extends BaseExp with DFAOps { this: Functions => 

  case class DFAFlagged(e: Rep[Any], link: DIO) extends Def[DfaState]
  case class DFAState(f: Rep[Char => DfaState]) extends Def[DfaState]
  
  def dfa_flagged(e: Rep[Any])(rec: DIO): DIO = DFAFlagged(e,rec)
  def dfa_trans(f: Rep[Char] => DIO): DIO = DFAState(doLambda(f))
  
}


trait ScalaGenDFAOps extends ScalaGenBase {
  val IR: DFAOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DFAState(f) => emitValDef(sym, "scala.virtualization.lms.epfl.test4.dfaTrans(" + quote(f) + ")")
    case DFAFlagged(e,l) => emitValDef(sym, "scala.virtualization.lms.epfl.test4.dfaFlagged(" + quote(e) + ", " + quote(l) + ")")
    case _ => super.emitNode(sym, rhs)
  }  
}


trait NFAToDFA extends DFAOps { this: Arith with Functions with Equal with IfThenElse =>

  // TODO: copy from below

}



case class gTrans(e: List[Any], f: Char => List[gTrans])

trait GAOps extends Base {

  type GIO = List[gTrans] // unstaged Automaton type, also State type (State = list of possible transitions)
  
  def gtrans(f: Rep[Char] => Rep[GIO]): Rep[GIO] = gtrans(unit(Nil))(f)
  def gtrans(e: Rep[List[Any]])(f: Rep[Char] => Rep[GIO]): Rep[GIO] // = collectall(List(unit("gtrans"), lam(f))).asInstanceOf[Rep[GIO]]
  def gcall(f: Rep[GIO], c: Rep[Char]): Rep[GIO] // = collectall(List(unit("gcall"), f, c)).asInstanceOf[Rep[GIO]]

  def gflags(f: Rep[GIO]): Rep[List[Any]]
  
  def gguard(c: Rep[Boolean], s: Boolean = false)(d: Rep[GIO]): Rep[GIO]

  def gstop(): Rep[GIO]
  def gor(a: Rep[GIO], b: Rep[GIO]): Rep[GIO]

}


trait GAOpsExp extends BaseExp with GAOps { this: ListOps with IfThenElse with Functions =>

  case class GTrans(e: Rep[Any], f: Rep[Char => GIO]) extends Def[GIO]
  case class GCall(f: Rep[GIO], c: Rep[Char]) extends Def[GIO]
  case class GFlags(f: Rep[GIO]) extends Def[List[Any]]

  def gtrans(e: Rep[List[Any]])(f: Rep[Char] => Rep[GIO]): Rep[GIO] = GTrans(e, doLambda(f))
  def gcall(f: Rep[GIO], c: Rep[Char]): Rep[GIO] = GCall(f,c)

  def gflags(f: Rep[GIO]): Rep[List[Any]] = GFlags(f)
  
  def gguard(c: Rep[Boolean], s: Boolean = false)(d: Rep[GIO]): Rep[GIO] = if (c) d else gstop()

  def gstop(): Rep[GIO] = list_new(Nil)
  def gor(a: Rep[GIO], b: Rep[GIO]): Rep[GIO] = list_concat(a,b)

}

trait ScalaGenGAOps extends ScalaGenBase {
  val IR: GAOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case GTrans(e,f) => emitValDef(sym, "List(scala.virtualization.lms.epfl.test4.gTrans(" + quote(e) + "," + quote(f) + "))")
    case GCall(f,c) => emitValDef(sym, quote(f) + ".flatMap(_.f.apply(" + quote(c) + "))")
    case GFlags(f) => emitValDef(sym, quote(f) + ".flatMap(_.e)")
    case _ => super.emitNode(sym, rhs)
  }  
}




trait MatcherNewProg extends DFAOps with GAOps { this: Arith with Functions with Equal with IfThenElse =>

  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A=>B]) {
    def apply(x:Rep[A]): Rep[B] = doApply(f, x)
  }
  implicit def lam[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  //implicit def toLambdaOps[A,B](f: Rep[A=>B]) = new LambdaOps(f)

  implicit def toDouble(f: Rep[Int]): Rep[Double] = f.asInstanceOf[Rep[Double]]
  
  // -- end boilerplate


  def collectall(in: List[Rep[Any]]): Rep[Unit]
  //def printL(in: Rep[Any]): Rep[Unit]

  // -- end util


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
  
  def ginterpret(xs: Rep[GIO], cin: Rep[Char]): Rep[GIO] = gcall(xs, cin)
  
  // -- end general automaton



  // -- begin NFA
  
  type IO = List[Trans]
  
  case class Trans(c: Option[Char], e: Option[Rep[Unit]], s: () => IO)
  
  def trans(c: Option[Char])(s: () => IO): IO = List(Trans(c, None, s))
  
  
  def exploreNFA[A:Manifest](xs: IO, cin: Rep[Char])(flag: Rep[Any] => Rep[A] => Rep[A])(k: IO => Rep[A]): Rep[A] = xs match {
    case Nil => k(Nil)
    case Trans(Some(c), e, s)::rest =>
      if (cin == c) {
        val xs1 = rest collect { case Trans(Some(`c`) | None,e,s) => Trans(None,e,s) }
        val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
        maybeFlag(exploreNFA(xs1, cin)(flag)(acc => k(acc ++ s())))
      } else {
        val xs1 = rest filter { case Trans(Some(`c`),_,_) => false case _ => true }
        exploreNFA(xs1, cin)(flag)(k)
      }
    case Trans(None, e, s)::rest =>
      val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
      maybeFlag(exploreNFA(rest,cin)(flag)(acc => k(acc ++ s())))
  }

  // -- end NFA




  // -- begin DFA
  

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




  def testMatchingG(xs: Rep[List[Char]]) = {

    def iterate: Rep[GIO => DfaState] = lam { state: Rep[GIO] => dfa_flagged(gflags(state))(dfa_trans { c: Rep[Char] =>
      
      iterate(gcall(state,c))
      
    })}
    iterate(gfindAAB())

  }

  def testMatching(xs: Rep[List[Char]]) = {

    def iterate(state: IO): DIO = dfa_trans { c: Rep[Char] =>
      exploreNFA(state, c)(dfa_flagged) { next =>
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



  def testMatcherNew1 = {
    withOutFile(prefix+"matchernew1") {
      object MatcherProgExp extends MatcherNewProg with DFAOpsExp with GAOpsExp
      with ArithExpOpt with EqualExpOpt with BooleanOpsExp with IfThenElseExpOpt with ListOpsExp
      //with FunctionExpUnfoldRecursion 
      with FunctionsExternalDef1 /* was 2 */ 
      with CompileScala {
        case class Result(in: List[Exp[Any]]) extends Def[Unit] //FIXME
        def collectall(in: List[Rep[Any]]): Rep[Unit] = Result(in)
        //def printL(in: Rep[Any]): Rep[Unit] = /*reflectEffect*/(Result(List(in))) //FIXME violate ordering
        override val verbosity = 1
        lazy val codegen = p
      }
      object p extends ScalaGenArith with ScalaGenEqual with ScalaGenListOps with ScalaGenDFAOps with ScalaGenGAOps
        with ScalaGenIfThenElse with ScalaGenFunctionsExternal { 
          val IR: MatcherProgExp.type = MatcherProgExp 
          import IR._          
          import java.io.PrintWriter
          override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
            case Result(xs) => emitValDef(sym, "(" + (xs map {quote}).mkString(",") + ") // DUMMY")
            //case Lambda(f) => emitNode(sym, DefineFun(f)) // FIXME
            case _ => 
              //println("emit super on " + sym + " = " + rhs + " / " + rhs.getClass)
              super.emitNode(sym, rhs)
          }
      }
      
      import MatcherProgExp.{List=>_,_}
      val f = (x:Rep[List[Char]]) => testMatching(x)
      
      p.emitSource(f, "Match", new java.io.PrintWriter(System.out))
      val fc = compile(f)
      var state = fc(Nil)
      
      val input = List('X','A','B','Z','A','A','B','W','A','A','A','A','B','Q')
      var idx = 0
      input foreach { c =>
        println("idx:   " + idx)
        println("found: " + state.hasFlag("found"))
        println("char:  " + c)
        
        idx += 1
        state = state.next(c)
      }
    }
    assertFileEqualsCheck(prefix+"matchernew1")
  }

  def testMatcherNew2 = {
    withOutFile(prefix+"matchernew2") {
      object MatcherProgExp extends MatcherNewProg with DFAOpsExp with GAOpsExp
      with ArithExpOpt with EqualExpOpt with BooleanOpsExp with IfThenElseExpOpt with ListOpsExp
      //with FunctionExpUnfoldRecursion 
      with FunctionsExternalDef1 /* was 2 */ 
      with CompileScala {
        case class Result(in: List[Exp[Any]]) extends Def[Unit] //FIXME
        def collectall(in: List[Rep[Any]]): Rep[Unit] = Result(in)
        //def printL(in: Rep[Any]): Rep[Unit] = /*reflectEffect*/(Result(List(in))) //FIXME violate ordering
        override val verbosity = 1
        lazy val codegen = p
      }
      object p extends ScalaGenArith with ScalaGenEqual with ScalaGenListOps with ScalaGenDFAOps with ScalaGenGAOps
        with ScalaGenIfThenElse with ScalaGenFunctionsExternal { 
          val IR: MatcherProgExp.type = MatcherProgExp
          import IR._          
          import java.io.PrintWriter
          override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
            case Result(xs) => emitValDef(sym, "(" + (xs map {quote}).mkString(",") + ") // DUMMY")
            //case Lambda(f) => emitNode(sym, DefineFun(f)) // FIXME
            case _ => 
              //println("emit super on " + sym + " = " + rhs + " / " + rhs.getClass)
              super.emitNode(sym, rhs)
          }
      }
      import MatcherProgExp.{List=>_,_}

      val f = (x:Rep[List[Char]]) => testMatchingG(x)
      
      // FIXME: problem with recursive codegen
        
      p.emitSource(f, "MatchG", new java.io.PrintWriter(System.out))
      val fc = compile(f)
      var state = fc(Nil)
      
      val input = List('X','A','B','Z','A','A','B','W','A','A','A','A','B','Q')
      var idx = 0
      input foreach { c =>
        println("idx:   " + idx)
        println("found: " + state.hasFlag(List(1)))
        println("char:  " + c)
        
        idx += 1
        state = state.next(c)
      }
    }
    assertFileEqualsCheck(prefix+"matchernew2")
  }

}