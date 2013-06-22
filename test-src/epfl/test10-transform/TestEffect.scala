package scala.virtualization.lms
package epfl
package test10

import common._
import internal.{NestedBlockTraversal}
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}
import test8._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext


// investigate modified mutation tracking similar to SSA form.
// 
//    val x = vzeros(100)
//    val as = vliteral(List(x))
//    vupdate(x,5,7)
//    val y = as(0)
//    println(y)
// 
// becomes:
// 
//    val x0 = vzeros(100)
//    val as0 = vliteral(List(x))
//    val m0 = vupdate(x0,5,7)
//    val x1 = x0 // mutate m0
//    val as1 = as0 // mutate m0
// 
// current questions: 
//    what do we gain?
//      improved DCE: no Mutate node -> vupdate stm can go, too
//    can we do without .mutable annotations?
//      probably not. would need to be pessimistic about everything, i.e. no CSE at all
//    can we do cse for mutable data?
//      one idea was to introduce copy on write, i.e. undo cse when mutation is detected.
//      this seems hard: original data may be used as part of complex structures that 
//      would need to be recreated and the previous version is not guaranteed to be DCEd.
//      another idea was to recognize common subexpressions but insert copy nodes instead
//      of eliminating them (similar to mzeros(100) = zeros(100).mutable)
//
// implementation (DONE):
//    ensure anti-deps: mutated symbols must be dead after the VectorUpdate stm
//    add fattening: fuse VectorUpdate and those Mutate nodes that aren't DCEd
//      is this really necessary? it seems like 



trait Lib extends VectorOps


trait LibExp extends Lib with VectorExp with BaseFatExp with EffectExp {
  
  case class Mutate[T](a: Rep[T], b: Rep[Any]) extends Def[T]

  case class Copy[T](a: Rep[T]) extends Def[T]
  
  case class ReflectSoft[T](a: Def[T], es: List[Exp[Any]]) extends Def[T]
  
  case class Multi(as: List[Def[Any]]) extends FatDef

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Mutate(a,b) => toAtom(Mutate(f(a),f(b)))
    case Copy(a) => toAtom(Copy(f(a)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??


  def writeSyms(d: Any): List[Sym[Any]] = d match {
    case VectorUpdate(a,x,y) => syms(a)
    case _ => List()
  }

  override def syms(d: Any): List[Sym[Any]] = d match {
    case d@ReflectSoft(a,es) => syms(a)
    case _ => super.syms(d)
  }
  override def symsFreq(d: Any) = d match {
    case d@ReflectSoft(a,es) => symsFreq(a)
    case _ => super.symsFreq(d)
  }
  override def softSyms(d: Any): List[Sym[Any]] = d match {
    case d@ReflectSoft(a,es) => syms(es)
    case _ => super.softSyms(d)
  }


  var subst: Map[Sym[_],Exp[_]] = Map() // need to hook this up with reifyEffects? does it ever need to be reset?

  // speculative nature: we must be able to back off if speculation fails.
  // ergo, need to save original Defs (or have a way to reconstruct them).

  override implicit def toAtom[A:Manifest](d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    val in = syms(d)
    val actual = in map (s => subst.getOrElse(s,s))
    
    if (in != actual) {
      val t = new SubstTransformer
      t.subst ++= subst
      mirror(d,t)
    } else {
      
      val kill = writeSyms(d)
      if (kill.nonEmpty) {
        
        val transitive = globalDefs collect { case e@TP(s,rhs) if (s::allAliases(rhs) intersect kill).nonEmpty => e }
        
        println("killing: " + kill + "/" + transitive.map(_.sym) + " by " + d)
        
        val sym = fresh[A]
        
        for (TP(s,rhs) <- transitive) {
          subst += (s -> toAtom(Mutate(s, sym)))
        }
        
        // add soft dependencies on transitive to ensure ordering!
        
        // at first sight it should be enough that for each s in transitive 
        // there is a Mutate node with a dependency.
        // however the Mutate node might be DCE'd, but not the original
        // mutable sym.
        
        /* scenario:
          
          val v = vrand(100)
          val w = v
          println(w)
          v(5) = 100               <--- must still come after w
          val v1 = v // mutated
        [ val w1 = w // mutated ]  <--- dce
          println(v)
          
        */
        
        createDefinition(sym, ReflectSoft(d, transitive.flatMap(_.lhs)))
        sym
      } else {
        // right now we add copy statements whenever we'd do CSE.
        // TODO: re-introduce a split between mutable and immutable data.
        val o = findDefinition(d)
        
        val sym = fresh[A]
        if (o.nonEmpty) { //d available as s1          
          val s1 = o.get.lhs.head
          println("cse: " + sym + " -> " + s1)
          //subst += (sym -> s1)
          createDefinition(sym, Copy(s1)) // no CS
        } else {
          createDefinition(sym, d) // no CSE!
        }
        // if (d available as s1) subst += (sym -> s1)
        sym
      }
    }
  }

  
  override def findDefinition[A](d: Sym[A]): Option[Stm] = {
    super.findDefinition(d)
  }

}



class TestEffects extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test10-"
  
  trait DSL extends Lib with ArrayMutation with Arith with OrderingOps with BooleanOps with LiftVariables with IfThenElse with While with RangeOps with Print {

    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

    def test(x: Rep[Int]): Rep[Unit]
  }
  trait Impl extends DSL with ArrayMutationExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
      with EqualExpOpt //with VariablesExpOpt 
      with IfThenElseExpOpt with WhileExpOptSpeculative with RangeOpsExp with PrintExp 
      with Lib with LibExp { self => 
    override val verbosity = 2
    val codegen = new ScalaGenFat with ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhileOptSpeculative with ScalaGenRangeOps 
      with ScalaGenPrint /*with LivenessOpt*/ { val IR: self.type = self 
        override def fattenAll(e: List[Stm]): List[Stm] = {
          println("**fatten "+e)
          // group all Mutate helper nodes together with the mutation
          // TBD: is this necessary (if not, desirable) ?
          val m = e collect { case e@TP(s, Mutate(a,b)) => e } 
          val mg = m groupBy { case e@TP(s, Mutate(a,b)) => b } 
          
          val e2 = e map {
            case e@TP(s, rhs) if mg.contains(s) => 
              val vs = mg(s)
              val llhs = vs map (_.sym)
              val rrhs = vs map (_.rhs)
              TTP(s::llhs, rhs::rrhs, Multi(rhs::rrhs))
            case e => e
          }
          val e3 = e2 diff m
          
          super.fattenAll(getSchedule(e3)(e3.flatMap(_.lhs)))
        }
        override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
          case Multi(as) => 
            stream.println("// begin multi")
            (symList zip as) foreach { case (s,e) => emitNode(s,e) }
            stream.println("// end multi")
          case _ => super.emitFatNode(symList, rhs)
        }
        override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
          case _ if rhs.toString.startsWith("Vector") => 
            emitValDef(sym, rhs.toString)
          case ReflectSoft(x,es) =>
            stream.println("// soft deps: "+es.map(quote).mkString(","))
            emitNode(sym,x)
          case Mutate(a,b) =>
            emitValDef(sym, quote(a) + " // mutated by "+ quote(b, true))
          case Copy(a) =>
            emitValDef(sym, quote(a) + ".clone")
          case _ =>
            super.emitNode(sym,rhs)
        }
      }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }
  
  def testEffects1 = withOutFileChecked(prefix+"effects1") { // test ordering
    trait Prog extends DSL {
      def test(x: Rep[Int]) = {
        val x = vzeros(100)
        val as = vliteral(List(x))
        vupdate(x,5,7) // later reads of as must come after this
        val y = vapply(as,0)
        print(y)
      }
    }
    new Prog with Impl
  }

  def testEffects2 = withOutFileChecked(prefix+"effects2") { // test cse
    trait Prog extends DSL {
      def test(x: Rep[Int]) = {
        val x = vzeros(100)
        val y = vzeros(100) // this will do speculative cse (or call copy)
        val as = vliteral(List(x))
        val bs = vliteral(List(y))
        vupdate(x,5,7) // must undo cse: now x and y are different. also, as and bs are different now
        val u = vapply(as,0)
        val v = vapply(bs,0)
        print(u)
        print(v)
      }
    }
    new Prog with Impl
  }

  def testEffects3 = withOutFileChecked(prefix+"effects3") { // test cse
    trait Prog extends DSL {
      def test(x: Rep[Int]) = {
        val x = vzeros(100)
        val y = vzeros(100) // this will do speculative cse  (or call copy)
        val e = vliteral(List(y)) // assume that this operation is expensive (don't want to do it twice)
        print(e)
        vupdate(x,5,7) // must undo cse: now x and y are different. also, as and bs are different now
        print(e)
      }
    }
    new Prog with Impl
  }

  def testEffects4 = withOutFileChecked(prefix+"effects4") { // test mutable dce
    trait Prog extends DSL {
      def test(x: Rep[Int]) = {
        val x = vzeros(100)
        val y = x
        val as = vliteral(List(x))
        val bs = vliteral(List(y)) // this one should be dce'd because it is never used
        vupdate(x,5,7) // this will invalidate bs (anti-dep) but should not give rise to a hard dependency
        val u = vapply(as,0)
        val v = vapply(bs,0)
        print(u)
      }
    }
    new Prog with Impl
  }
}