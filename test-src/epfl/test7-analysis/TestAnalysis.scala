package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

trait Print extends Base {
  implicit def unit(s: String): Rep[String]
  def print(s: Rep[Any]): Rep[Unit]
}

trait PrintExp extends Print with EffectExp {
  implicit def unit(s: String): Rep[String] = Const(s)
  case class Print(s: Rep[Any]) extends Def[Unit]
  def print(s: Rep[Any]) = reflectEffect(Print(s))
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case Print(s) => Print(f(s))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(Print(s), u, es) => reflectMirrored(Reflect(Print(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenPrint extends ScalaGenEffect {
  val IR: PrintExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Print(s) =>  emitValDef(sym, "println(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}



/*

  TODO interesting cases:

    val y = if (c) {
      val x = read()
      write()
      x
    } else {
      0
    }

    ------
    
    val a = Array.make(100) { i =>
      i * i
    }

    val b = Array.make(100) { j =>
      val y = j * j
      y * j
    }

    val c = Array.reduce(100)(+) { k =>
      val z = a(k) 
      z * 3
    }
    
    translate to: (caveat: alpha convert j and k <- howto generalize?)
    
    step 0) flat schedule (strip dead code)
    step 1) find things worthwhile to fuse (CAREFUL ABOUT NESTING!!)
    step 2) alpha-convert (does algebraic opts + cse)
    step 3) actual codegen
    
    val i = Reflect(InRange(100))
    val x1 = i * i
    val a = Reify(Array.makeElem(x1), i)
    val y1 = j * j
    val y2 = y1 * j
    val b = Array.make(100)(j,y2)
    val z1 = a(k) // or: k*k
    val z2 = z1 * 3
    val c = Array.reduce(100)(+)(k,z2)

    
    
    LoopFused(100) { i =>
      val x1 = i * i
      val x2 = x1 * i
      val x3 = x1 * 3
      export val a = Array.makeElem(x1)
      export val b = Array.makeElem(x2)
      export val c = Array.reduceElem(+,x3)
    }
    
    val a,b,c = Array.fused { i =>
      val x1 = i * i
      val x2 = x1 * i
      val x3 = x1 * 3
      yieldMake(x1)
      yieldMake(x2)
      yieldReduce(+)(x1*3)
    }

    var a = new Array(100) // might stack alloc, or reuse mem
    var b = new Array(100)
    var c = 0
    for (i <- 0 until 100) {
      val x1 = i * i
      val x2 = x1 * i
      val x3 = x1 * 3
      a(i) = x1
      b(i) = x2
      c = c + x3
    }

    ------

    Folding vs. Fusion:
    
    val a = Array(100) { ia =>  ia * 3 }

    val b = Array(100) { ib => a(ib) + 3 } // can fold or fuse

    val c = Array(100) { ic => a(ic + 3) } // can fold but not fuse! a(i+3) not available to c(i)

      val ac = Array(100) { iac => iac*3, (iac+3)*3 } // can fold but not fuse! a(i+3) not available to c(i)
      val ac = Array(100) { iac => t = iac*3; t,t+9 } // will not be done!!!



    ------

    Folding decisions are similar to inlining decisions. Might duplicate computations
    (instead of reusing stored results). If the computations are small this is worthwile (see above), 
    otherwise not.

    We should be specific about inlining -> should we be specific about folding as well?

    After all, arrays are just cached function results (of partial functions, that is).
    More exactly: partitioning of the domain into multiple function results (<- concat).

    --
    
    idea: not everything should be hoisted out of loops. an array construction L1 might be
    better folded into the loop L2 (removing the need for the intermediate array). however,
    if L1 is very costly, it might be better to construct it explicitly. it might even
    be worth to extract scalar subexpressions into arrays!
    
      val a = Array(100) { ia => val xa = gold(ia+1); xa * 4 }
      val b = Array(100) { ib => val xb = gold(ib+2); xb * 3 }

    gold(ia) cannot be moved out. assume gold is expensive, so it might be worth doing
    
      val g = Array(100) { ig => gold(ig) }                 // g = Array(100)(gold)
      val a = Array(100) { ia => val xa = g(ia+1); xa * 4 } // a = g.rotate(-1) *. scatter(100)(4)
      val b = Array(100) { ib => val xb = g(ib+2); xb * 3 } // b = g.rotate(-2) *. scatter(100)(3)  <- DEFINITELY inline scatter!
    
    (although we'd really want to fuse all three into one, the +1 and +2 don't allow parallel loop)
    
    currently we can do CSE only for scalar values.

    --
    
    another option is to always array-ify things that depend on i: eta-expand, basically.
    need to think some more about this.
    
    --
    
    SAC:
      folding: forward subsitute L1 into L2 if that is the only use of L1
      fusion: if data dependency hinders fusion of L1 and L2, forward substitute anyways.
        other uses of L1 will be redirected to fused L1', so L1 goes away.
    
    Question: does that enable fusion of ALL loops with same generators?
    "sets of generators .. sufficiently simple or similar to make fusion feasible"
    
    what about a(i+2) dependency? --> will cause duplicate work, no?? gold(i), gold(i+1)
    what about fold(a) dep? not elem-wise access! would duplicate whole loop?
    
    --> assume for now only a(i) deps trigger forward subst! --> make sure not to pessimize
    --> regular folding can also handle other indices, like a(i+j)
    
    
    "fusion of two with-loops may prevent further fusion with a third with-loop"
    
    --
    
    what about: traverse nodes from root (result) breadth first wise, with no interdep
    at each level. then do maximal munch.
    
    not sure.
    
    --
    
    SAC-style fusion:
    
    1. try to bring loops closer together (by removing interdependent stuff)
    2. can fuse adjacent loops if L1 not in freevars(L2)
    
    --
    
    Kennedy's line of work: it's probably not worth (much) fusing loops that do not share anything
    
    --
    
    to find which loops can be fused: 
      - simple interdependencies are ok (i.e. A(i) where A is an Array and i is a loop var, or affine in the future)
      - complex ones prevent fusion
    need to distinguish the two! temporarily remove the simple ones --> only the others are left!
      - if loops are unrelated (no interdeps) in the modified graph they can be fused!
      - strongly connected components cannot be fused internally (they are sequential), only externally
      - 
*/



class TestAnalysis extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test7-"
  

  
}
