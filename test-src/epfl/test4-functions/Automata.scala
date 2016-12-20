package scala.lms
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


// careful with @specialized blowup
case class Automaton[@specialized(Boolean,Char,Int) I, @specialized(Boolean,Char,Int) O](out: O, next: I => Automaton[I,O])

case class NAutomaton[@specialized(Boolean,Char,Int) I, @specialized(Boolean,Char,Int) O](out: O, next: I => List[NAutomaton[I,O]])

//case class GAutomaton[M[_], @specialized(Boolean,Char,Int) I, @specialized(Boolean,Char,Int) O](out: O, next: I => M[GAutomaton[M,I,O]])

// type Automaton = O x (I => Automaton)
// type Automaton = S x O x (I => (S => O x S))

trait DFAOps extends Base {

  type DfaState = Automaton[Char,List[Any]]

  type DIO = Rep[DfaState]

  def dfa_flagged(e: Rep[Any])(rec: DIO): DIO
  def dfa_trans(f: Rep[Char] => DIO): DIO = dfa_trans(unit(Nil))(f)
  def dfa_trans(e: Rep[List[Any]])(f: Rep[Char] => DIO): DIO
}


trait DFAOpsExp extends BaseExp with DFAOps { this: Functions => 

  case class DFAFlagged(e: Rep[Any], link: DIO) extends Def[DfaState]
  case class DFAState(e: Rep[List[Any]], f: Rep[Char => DfaState]) extends Def[DfaState]
  
  def dfa_flagged(e: Rep[Any])(rec: DIO): DIO = DFAFlagged(e,rec)
  def dfa_trans(e: Rep[List[Any]])(f: Rep[Char] => DIO): DIO = DFAState(e, doLambda(f))
  
}


trait ScalaGenDFAOps extends ScalaGenBase {
  val IR: DFAOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DFAState(e,f) => emitValDef(sym, "scala.lms.epfl.test4.Automaton(" + quote(e) + "," + quote(f) + ")")
    case DFAFlagged(e,l) => emitValDef(sym, quote(l) + ".copy(out = " + quote(e) + "::" + quote(l) + ".out)")
    case _ => super.emitNode(sym, rhs)
  }
}


trait NFAtoDFA extends DFAOps { this: Arith with Functions with Equal with IfThenElse =>

/*
case class Automaton[I,O](out: O, next: I => Automaton[I,O])
type DIO = Rep[Automaton[Char,List[Any]]]

type NIO = List[NFATrans[Char,Any]]
case class NFATrans[I,O](out: Rep[O], in: Set[I], s: () => NIO)

def convertNFAtoDFA(in: NIO): DIO = {
  def iterate(state: NIO): DIO = 
    dfa_trans(state map (_.out)) { c: Rep[Char] =>
      exploreNFA(state, c) { next =>
        iterate(next)
      }
    }
  iterate(in)
}
*/



  type NIO = List[NTrans]
  
  case class NTrans(c: CharSet, e: Option[Rep[Unit]], s: () => NIO)
  
  def trans(c: CharSet)(s: () => NIO): NIO = List(NTrans(c, None, s))

  def guard(cond: CharSet, found: Boolean = false)(e: => NIO): NIO = {
    List(NTrans(cond, if (found) Some(unit("found").asInstanceOf[Rep[Unit]]) else None, () => e))
  }

  def stop(): NIO = Nil
  

  type CharSet = Option[Char]
  
  def infix_contains(s: CharSet, c: Rep[Char]): Rep[Boolean] = s match {
    case Some(c1) => c == c1
    case None => unit(true)
  }

  def infix_intersect(s1: CharSet, s2: CharSet): CharSet = (s1,s2) match {
    case (Some(c1), Some(c2)) if c1 == c2 => None
    case (Some(c1), Some(c2)) => None
    case _ => None
  }

  def infix_diff(s1: CharSet, s2: CharSet): CharSet = (s1,s2) match {
    case (Some(c1), Some(c2)) if c1 == c2 => None
    case (Some(c1), Some(c2)) => None
    case _ => None
  }

  
  
  def exploreNFA[A:Manifest](xs: NIO, cin: Rep[Char])(flag: Rep[Any] => Rep[A] => Rep[A])(k: NIO => Rep[A]): Rep[A] = xs match {
    case Nil => k(Nil)
    case NTrans(cset@Some(c), e, s)::rest =>
      if (cset contains cin) {
        val xs1 = rest collect { case NTrans(Some(`c`) | None,e,s) => NTrans(None,e,s) }
        val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
        maybeFlag(exploreNFA(xs1, cin)(flag)(acc => k(acc ++ s())))
      } else {
        val xs1 = rest filter { case NTrans(Some(`c`),_,_) => false case _ => true }
        exploreNFA(xs1, cin)(flag)(k)
      }
    case NTrans(None, e, s)::rest =>
      val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
      maybeFlag(exploreNFA(rest,cin)(flag)(acc => k(acc ++ s())))
  }


  def convertNFAtoDFA(in: NIO): DIO = {

    def iterate(state: NIO): DIO = dfa_trans { c: Rep[Char] =>
      exploreNFA(state, c)(dfa_flagged) { next =>
        iterate(next)
      }
    }

    iterate(in)
  }


}



trait GAOps extends Base {

  type gTrans = NAutomaton[Char, List[Any]]

  type GIO = List[gTrans] // unstaged Automaton type = State type (State = list of possible transitions)
  
  def gtrans(f: Rep[Char] => Rep[GIO]): Rep[GIO] = gtrans(unit(Nil))(f)
  def gtrans(e: Rep[List[Any]])(f: Rep[Char] => Rep[GIO]): Rep[GIO]
  def gcall(f: Rep[GIO], c: Rep[Char]): Rep[GIO]

  def gflags(f: Rep[GIO]): Rep[List[Any]]
  
  def gguard(c: Rep[Boolean], s: Boolean = false)(d: Rep[GIO]): Rep[GIO]

  def gstop(): Rep[GIO]
  def gor(a: Rep[GIO], b: Rep[GIO]): Rep[GIO]

}


trait GAtoDA extends DFAOps with GAOps { this: Functions =>
  
  def convertGAtoDA(in: Rep[GIO]): Rep[DfaState] = {

    def iterate: Rep[GIO => DfaState] = doLambda { state: Rep[GIO] => 
      dfa_trans(gflags(state)) { c: Rep[Char] =>
        iterate(gcall(state,c))
      }
    }
    
    iterate(in)
  }
  
}


trait GAOpsExp extends BaseExp with GAOps { this: ListOps with IfThenElse with Functions =>

  case class GTrans(e: Rep[List[Any]], f: Rep[Char => GIO]) extends Def[GIO]
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
    case GTrans(e,f) => emitValDef(sym, "List(" + remap(sym.tp.typeArguments(0)) + /*scala.lms.epfl.test4.NAutomaton*/ "(" + quote(e) + "," + quote(f) + "))")
    case GCall(f,c) => emitValDef(sym, quote(f) + ".flatMap(_.next.apply(" + quote(c) + "))")
    case GFlags(f) => emitValDef(sym, quote(f) + ".flatMap(_.out)")
    case _ => super.emitNode(sym, rhs)
  }
}



trait StepperOps extends DFAOps with Util { this: IfThenElse with ListOps with TupleOps with /*While with Variables with*/ Functions =>
  
  // Producers: produce values by executing state transitions
  
  def Prod[S:Manifest](s0: Rep[S], cond: Rep[S] => Rep[Boolean], step: Rep[S] => Rep[S]) = 
    Prod2(Stepper2[Unit,S,S](s0,cond,x=>step,x=>x))
  
  type Prod[S] = Prod2[S,S]
  
  case class Prod2[S,O:Manifest](st: Stepper2[Unit,S,O]) {
    implicit def mfs = st.mfs
    
    def step = st.yld(unit())
    
    def run() = {
      def iter: Rep[S => S] = lam { s: Rep[S] => 
        if (st.cond(s)) iter(step(s)) else s
      }
      st.res(iter(st.s))
    }

    def foreach[S1:Manifest](yld: Rep[O] => Rep[S1] => Rep[S1]) = { s1: Rep[S1] =>
      into(Foreach[O,S1](s1, yld)).run()
    }

    def until(p: Rep[O] => Rep[Boolean]) = { // TODO: may want to bundle output with state to avoid recomputation
    /*  Prod2[(S,O),O](Stepper2((st.s,st.res(st.s)),
        { ss => band(st.cond(ss._1),p(ss._2)) },
        { ss=> val s1 = st.step(ss._1); (s1,st.res(s1)) },
        { ss=> ss._2}))*/
      Prod2[S,O](Stepper2(st.s,
            { s => bandnot(st.cond(s), p(st.res(s))) },
            st.yld, st.res))
    }

    def filter(p: Rep[O] => Rep[Boolean]) = {
      val stu = until(p).st
      def seek(sx: Rep[S]) = Prod2(Stepper2[Unit,S,S](sx, stu.cond, stu.yld, s => s)).run()
      
      Prod2[S,O](Stepper2(seek(st.s), 
        { s => band(st.cond(s), p(st.res(s))) }, 
        { x => s => seek(step(s)) }, st.res))
    }

    def map[O1:Manifest](f: Rep[O] => Rep[O1]) = {
      Prod2[S,O1](Stepper2(st.s,st.cond,st.yld,x=>f(st.res(x))))
    }

    def zip[S1,O1:Manifest](b: Prod2[S1,O1]) = {
      Prod2(st.zip(b.st))
    }
    
    def into[S1,O1:Manifest](b: Stepper2[O,S1,O1]) = {
      Prod2(st.into(b))
    }

  }



  // Steppers: do a state transition for each input value. can calculate output from each state.
  
  def Foreach[T:Manifest,S:Manifest](s0: Rep[S], yld: Rep[T] => Rep[S] => Rep[S]) = 
    Stepper2[T,S,S](s0, s => unit(true), yld, s => s)
  
  type Foreach[T,S] = Stepper2[T,S,S]
  
  case class Stepper2[I:Manifest,S:Manifest,O:Manifest](s: Rep[S], cond: Rep[S] => Rep[Boolean], yld: Rep[I] => Rep[S] => Rep[S], res: Rep[S] => Rep[O]) {
    def mfs = manifest[S]
    
    def zip[S1,O1:Manifest](o: Stepper2[I,S1,O1]) = {
      implicit val mfs1 = o.mfs
      Stepper2[I,(S,S1),(O,O1)]((s,o.s), 
        ss => band(cond(ss._1), o.cond(ss._2)),
        x => frtuple(yld(x), o.yld(x)), 
        ss => (res(ss._1), o.res(ss._2)))
    }
    
    def into[S1,O1:Manifest](b: Stepper2[O,S1,O1]) = {
      implicit val mfs1 = b.mfs
      Stepper2[I,(S,S1),O1]((s,b.s), 
        ss => band(cond(ss._1), b.cond(ss._2)),
        x => ss => (yld(x)(ss._1), b.yld(res(ss._1))(ss._2)),
        ss => (b.res(ss._2)))
    }

    //def andThen

    def result = new Stream[O] {
      type XI[I1] = I
      type XS[S1] = (S,S1)
      //def into[S:Manifest](x: Foreach[T,S]) = x
      def into[S1:Manifest,O1:Manifest](x: Stepper2[O,S1,O1]) = Stepper2.this.into(x)
    }
    
  }


  def band(a: Rep[Boolean], b: Rep[Boolean]) = if (a) b else unit(false)
  def bandnot(a: Rep[Boolean], b: Rep[Boolean]) = if (b) unit(false) else a
  def ftuple[S,S1](f1:S=>S, f2:S1=>S1) = (ss: (S,S1)) => (f1(ss._1), f2(ss._2))
  def frtuple[S:Manifest,S1:Manifest](f1:Rep[S]=>Rep[S], f2:Rep[S1]=>Rep[S1]) = (ss: Rep[(S,S1)]) => make_tuple2(f1(ss._1), f2(ss._2))


  // Streams: streams are stepper transformers

  def Stream[T:Manifest] = new Stream[T] {
    def mf = manifest[T]
    type XI[I] = T
    type XS[S] = S
    //def into[S:Manifest](x: Foreach[T,S]) = x
    def into[S:Manifest,O:Manifest](x: Stepper2[T,S,O]): Stepper2[XI[T],XS[S],O] = x
  }

  abstract class Stream[T:Manifest] extends Serializable { o =>
    type XI[I]
    type XS[S]
    //def into[S:Manifest](x: Foreach[T,S]): Foreach[XI[T],XS[S]]
    def into[S:Manifest,O:Manifest](x: Stepper2[T,S,O]): Stepper2[XI[T],XS[S],O]

    def filter(p: Rep[T] => Rep[Boolean]) = new Stream[T] {
      type XI[I] = o.XI[T]
      type XS[S] = o.XS[S]
      //def into[S:Manifest](x: Foreach[T,S]) = o.into(x.prefilter(p))
      def into[S:Manifest,O:Manifest](k: Stepper2[T,S,O]) = {
        o.into(k.copy(yld = { x => s => if (p(x)) k.yld(x)(s) else s }))
      }
    }

    def map[U:Manifest](f: Rep[T] => Rep[U]) = new Stream[U] {
      type XI[I] = o.XI[T]
      type XS[S] = o.XS[S]
      /*def into[S:Manifest](x: Foreach[U,S]) = {
        o.into(x.premap(f))
      }*/
      def into[S:Manifest,O:Manifest](k: Stepper2[U,S,O]) = {
        o.into(k.copy(yld = { x => k.yld(f(x)) }))
      }
    }

    def flatMap[S1,U:Manifest](f: Rep[T] => Prod2[S1,U]) = new Stream[U] {
      type XI[I] = o.XI[T]
      type XS[S] = o.XS[S]
      /*def into[S:Manifest](x: Foreach[U,S]) = {
        o.into(x.preflatMap(f))
      }*/
      def into[S:Manifest,O:Manifest](k: Stepper2[U,S,O]) = {
        o.into(Stepper2(k.s, k.cond, { x => f(x).foreach(k.yld) }, k.res))
      }
    }

    def split[S1:Manifest,O1:Manifest](p: Rep[T]=>Rep[Boolean], a: Stepper2[T,S1,O1]) = new Stream[O1] {
     type XI[I] = o.XI[T]
     type XS[S2] = o.XS[(S1,S2)]
     
      def into[S2:Manifest,O2:Manifest](b: Stepper2[O1,S2,O2]) = {
        o.into(Stepper2[T,(S1,S2),O2]((a.s,b.s), 
          ss => b.cond(ss._2), 
          c => ss => if (p(c)) (a.s, b.yld(a.res(ss._1))(ss._2)) else (a.yld(c)(ss._1), ss._2), 
          ss => b.res(ss._2)))
        // TODO: should last run anticipate one more iteration?
      }
    }
    
    /*def into[U:Manifest](b: Stream[U]): Stream[U] = new Stream[U] { // not in general: b may have wrong input type
      type XI[I] = o.XI[T]
      type XS[S] = o.XS[S]
      def into[S:Manifest,O:Manifest](k: Stepper2[U,S,O]) = {
        val k1 = b.into(k)
        o.into(k)
      }
    }*/

    /*def zip[U](b: Stream[U]) = new Stream[(T,U)] { // not in general: one stream may skip elements (or create more)
      type FE[S] = (o.FE[S],b.FE[S])
      def into[S:Manifest](x: Foreach[(T,U),S]) = {
        //o.into(x) zip b.into(x)
      }
    }*/


  }


  // --- stepper ---

  // convert stepper to coroutine / da
  def stepthrough[S:Manifest,O:Manifest](st: Stepper2[Char,S,O]): DIO = {

    val step = st.yld
    def iter: Rep[S => DfaState] = doLambda { (s: Rep[S]) =>
      val o = st.res(s)

      dfa_trans(List(o)) { c => 
        val s1 = step(c)(protect(s,c::Nil))
        iter(s1)
      }
    }
    iter(st.s)
  }
  
}



/*
trait Stream
case class EOF(err: Option[String]) extends Stream
case class Chunk(data: String) extends Stream

trait Iteratee[A]
case class IE_done[A](x: A) extends Iteratee[A,B]
case class IE_cont[A](err: Option[String], k: Stream => (Iteratee[A], Stream))


def flatMap[A,B](in: Iteratee[A])(f: A => Iteratee[B]) = in match {
  case IE_done(x) => f(x)
  case IE_cont(err,k) = IE_cont(err, s => docase(k(s)))
    def docase(it: Iteratee[A], s: Stream) = it match {
      case IE_done(x) => f(x) match {
        case IE_cont(None, k) => k(s)
        case i => (i, s)
      }
      case _ => (flatMap(it,f), s)
    }
}

f: a -> Iteratee b
instance Monad Iteratee where 
return = IE_done
IE_done a >>= f = f a 
IE_cont e k >>= f = IE_cont e (docase . k)
where docase 
  (IE_done a, stream) = case f a of
  IE_cont Nothing k -> k stream
  i -> (i,stream) 
docase (i, s) = (i >>= f, s)
*/