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

// investigate transform phases that are separate from optimization.
// in particular notation like this:
//
//    VectorPlus(a,b).atPhase(xform) {
//      val data = array(vlength(a)) { i => vapply(a,i) +  vapply(b,i) }
//      vfromarray(data)
//    }


/*
  unrelated question: can we separate loop fusion and code motion?
  
    a1 = SimpleLoop(s, i1, ArrayElem(Block(e1)))

    a2 = SimpleLoop(s, i2, ArrayElem(Block(e2)))
  
  becomes:
  
    a1,a2 = SimpleFatLoop(s, i12, ArrayElem(Block(e1)), ArrayElem(Block(e2)))

  we still have separate blocks for the ArrayElems.
  
  currently, calling emitFatBlock on the list of all embedded blocks List(e1,e2)
  will effectively combine the blocks during the descent, triggering
  fusion of inner loops and other optimizations.
  
  if we descend separately we miss opportunities.

  an isolated fusion pass will need to physically merge the blocks. a yield
  based version may achieve this:

    a1,a2 = SimpleFatLoop(s, i12, y1=ArrayElem, y2=ArrayElem, Block(e12)))
  
  where e12 invokes y1 and y2 internally.
  
  RULE: isolated passes need to merge blocks
*/


/*
  another idea: make block results staging time values
  
    case class Block[T](res: T)
    
  and use syms(res) to get the block result expressions.
  
  this would allow moving arbitrary stuff inside blocks:
  
    a1,a2 = SimpleFatLoop(s, i12, Block((ArrayElem(e1), ArrayElem(e2))))

*/


/*
  scheduling/dce/code motion: really a few separate things
  
    1) available: get all upward dependencies from result (backwards,dce,available)
    2) from that set, get all downward dependencies on bound variables, up to the node in which the var is bound (forward,must-below)
  
*/



/*
  another unrelated question: is mirroring always behaving correctly?
  what are the side conditions?
  
  RULE: mirror should not create new syms (apart from final result via toAtom)
        we must not create a new sym for one that is to be replaced
  RULE: mirror should not not call reifyEffects 
        why? it will likely call reflectEffect inside, which will create new syms. 
        maybe we already have a replacement for one of the effectful nodes.
  
  if mirror has a replacement for a node, apply it. if inputs of replacement
  are in domain of substitution, mirror replacement.
  
  mirroring is context-free in general. (for Reflect nodes we do setup a 
  dummy context with the tranformed inputs)
  
  
  thinking about a test case where mirroring breaks:
  
    val x = mzeros(100)

    val p = any()

    val a = foobar(x,i,p)

  then go ahead and replace p -> Const(0)

    def foobar(x,i,p) = p match {
      case Const(0) => 
        
        // something effectful...
        vapply(x,i)
        
      case _ => FooBar(x,i,p)
    }

    val a = vapply(x,i)   // becomes Reflect(VectorApply(x,i), Read(x))

  not sure...
  
*/



trait FWTransform extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>
  
  trait MyWorklistTransformer extends WorklistTransformer { val IR: self.type = self }
  
  def xform: MyWorklistTransformer
  
  def continue[A:Manifest](s: Block[A]): Block[A] = {
    xform.runOnce(s)
  }
  
  
  // ---------- Exp api
  
  implicit def toAfter[A:Manifest](x: Def[A]) = new { def atPhase(t: MyWorklistTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }
  implicit def toAfter[A](x: Exp[A]) = new { def atPhase(t: MyWorklistTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }

  // transform x to y at the *next* iteration of t. 
  
  def transformAtPhase[A](x: Exp[A])(t: MyWorklistTransformer)(y: => Exp[A]): Exp[A] = {
    t.register(x)(y)
    x
  }
  
  // note: if t is currently active, it will continue the current pass with x = x.
  // do we need a variant that replaces x -> y immediately if t is active?
  
  
  // ----------
  
  // we need to apply the current substitution to each Def we create:
  // Foo(x) atPhase(t) { bar(x) }   <--- x in bar(x)  will refer to a sym that may have been replaced itself
  
  def subst: Map[Sym[_], Exp[_]] = xform.subst

  override implicit def toAtom[A:Manifest](d: Def[A]): Exp[A] = { // override createDefinition instead?
    val in = syms(d)
    val actual = in map (s => subst.getOrElse(s,s))
    
    if (in != actual) {
      println("toAtom transform "+d+" " + in + " -> " + actual)
      val t = new SubstTransformer
      t.subst ++= subst
      mirror(d,t)
    } else {
      super.toAtom(d)
    }
  }

}

trait VectorExpTrans extends FWTransform with VectorExp with ArrayLoopsExp with ArrayMutationExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
    with EqualExpOpt //with VariablesExpOpt 
    with IfThenElseExpOpt with WhileExpOptSpeculative with RangeOpsExp with PrintExp {
  
  case class VectorFromArray[T](a: Rep[Array[T]]) extends Def[Vector[T]]
  case class VectorToArray[T](a: Rep[Vector[T]]) extends Def[Array[T]]

  def vfromarray[A:Manifest](x: Exp[Array[A]]): Exp[Vector[A]] = VectorFromArray(x)
  def vtoarray[A:Manifest](x: Exp[Vector[A]]): Exp[Array[A]] = x match {
    case Def(VectorFromArray(z)) => z
    case _ => VectorToArray(x)
  }
  
  // TODO: this is not very modular. should it be more modular?
  // can we (do we even want to?) add the atPhase transforms in a
  // separate trait? the argument against it is that we could just
  // implement the transform by pattern matching against Defs

  override def vzeros(n: Rep[Int]) =  VectorZeros(n).atPhase(xform) { vfromarray(array(n) { i => 0 }) }

  override def vapply[T:Manifest](a: Rep[Vector[T]], x: Rep[Int]) =  VectorApply(a,x).atPhase(xform) { vtoarray(a).at(x) }
  

  override def vlength[T:Manifest](a: Rep[Vector[T]]) = a match {
    case Def(VectorFromArray(b)) => b.length
    case _ => super.vlength(a)
  }


  // how does this interact with CSE? 
  // Foo(x) atPhase(t) { bar(x) }   <--- Foo(x) may resolve to existing sym z, which is then scheduled to be replaced

  override def vplus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = (a,b) match {
    //case (Def(VectorZeros(n)), b) => b
    //case (a, Def(VectorZeros(n))) => a
    case _ => 
      VectorPlus(a,b).atPhase(xform) {
        val data = array(vlength(a)) { i => vapply(a,i) +  vapply(b,i) }
        vfromarray(data)
      }
  }
  
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case VectorFromArray(a) => vfromarray(f(a))
    case VectorToArray(a) => vtoarray(f(a))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // gadt fail


  val xform = new MyWorklistTransformer {
    override def mmmirror[A:Manifest](e: Def[A]): Exp[A] = (e match {
      case IfThenElse(c,a,b) => __ifThenElse(mirrorExp(c),mirrorBlock(a),mirrorBlock(b))
      case SimpleLoop(s,v,ArrayElem(body)) => 
        array(mirrorExp(s)) { i => 
          subst += (v -> i) // TODO: should make sure we're in a subst environment
          mirrorBlock(body) 
        }
      case _ => 
        //println("* super mmirror " + e)
        super.mmmirror(e)
    }).asInstanceOf[Exp[A]] // gadt fail
  }
  
  
}



class TestForward extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test10-"
  
  trait DSL extends VectorOps with Arith with OrderingOps with BooleanOps with LiftVariables with IfThenElse with While with RangeOps with Print {

    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]


    def test(x: Rep[Int]): Rep[Any]
  }
  trait Impl extends DSL with VectorExpTrans with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
      with EqualExpOpt //with VariablesExpOpt 
      with IfThenElseExpOpt with WhileExpOptSpeculative with RangeOpsExp with PrintExp 
      with test7.TransformingStuff with FWTransform { self => 
    override val verbosity = 2
    val codegen = new ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhileOptSpeculative with ScalaGenRangeOps 
      with ScalaGenPrint /*with LivenessOpt*/ { val IR: self.type = self 
        override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
          if (rhs.toString.startsWith("Vector"))
            emitValDef(sym, rhs.toString)
          else
            super.emitNode(sym,rhs)
        }
      }
    codegen.withStream(new PrintWriter(System.out)) {
      println("### first")
      val b1 = reifyEffects(test(fresh))
      codegen.emitBlock(b1)
      codegen.stream.flush
      def iter(n: Int, b1: Block[Any]): Unit = if (n > 0) {
        println()
        println("### next")
        val b2 = continue(b1)
        codegen.emitBlock(b2)
        codegen.stream.flush
        iter(n-1,b2)
      }
      iter(10,b1) // fixed num of iterations for now
    }
  }
  
  def testForward1 = withOutFileChecked(prefix+"forward1") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val z = vzeros(100)
        val y = vzeros(100)
        val a = vplus(z,y)
        val b = vplus(z,a)
        print(b)
      }
    }
    new Prog with Impl
  }

  def testForward2 = withOutFileChecked(prefix+"forward2") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val z = vzeros(100)
        val y = vliteral(List(z))
        val a = vapply(y,0)
        print(a)
      }
    }
    new Prog with Impl
  }

}