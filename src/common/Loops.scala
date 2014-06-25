package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.{FatBlockTraversal,GenericNestedCodegen,GenericFatCodegen}

trait Loops extends Base { // no surface constructs for now

}

trait LoopsExp extends Loops with BaseExp with EffectExp {

  abstract class AbstractLoop[A] extends Def[A] with CanBeFused {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: Def[A]
  }
  
  case class SimpleLoop[A](val size: Exp[Int], val v: Sym[Int], val body: Def[A]) extends AbstractLoop[A]
  
  def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A])(implicit pos: SourceContext): Exp[A] = SimpleLoop(size, v, body)
  def simpleLoopCopy[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A], oldDef: Def[A])(implicit pos: SourceContext): Exp[A] = SimpleLoop(size, v, body).copyCanBeFused(oldDef)


  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => syms(e.size) ::: syms(e.body) // should add super.syms(e) ?? not without a flag ...
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => readSyms(e.size) ::: readSyms(e.body)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => e.v :: boundSyms(e.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: AbstractLoop[_] => freqNormal(e.size) ::: freqHot(e.body) // should add super.syms(e) ?? not without a flag ...
    case _ => super.symsFreq(e)
  }


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s,v,body: Def[A]) => simpleLoopCopy(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f), e)
    case Reflect(SimpleLoop(s,v,body: Def[A]), u, es) if u == Control() => reflectMirrored(Reflect(SimpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f)).copyCanBeFused(e), mapOver(f,u), f(es)))(mtype(manifest[A]), pos) 
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => aliasSyms(e.body)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => containSyms(e.body)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => extractSyms(e.body)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => copySyms(e.body)
    case _ => super.copySyms(e)
  }
}

trait LoopsFatExp extends LoopsExp with BaseFatExp {

  abstract class AbstractFatLoop extends FatDef with CanBeFused {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: List[Def[Any]]
  }
  
  case class SimpleFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop


  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => syms(e.size) ::: syms(e.body)
    case _ => super.syms(e)
  }
  
  override def readSyms(e: Any): List[Sym[Any]] = e match { 
    case e: AbstractFatLoop => readSyms(e.size) ::: readSyms(e.body)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => e.v :: boundSyms(e.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: AbstractFatLoop => freqNormal(e.size) ::: freqHot(e.body)
    case _ => super.symsFreq(e)
  }

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => aliasSyms(e.body)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => containSyms(e.body)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => extractSyms(e.body)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => copySyms(e.body)
    case _ => super.copySyms(e)
  }
}


trait BaseLoopsTraversalFat extends FatBlockTraversal {
  val IR: LoopsFatExp
  import IR._

  override def fatten(e: Stm): Stm = e match {
    case TP(sym, op: AbstractLoop[_]) =>
      TTP(List(sym), List(op), SimpleFatLoop(op.size, op.v, List(op.body)).copyCanBeFused(op))
    case TP(sym, p @ Reflect(op: AbstractLoop[_], u, es)) =>
      if (shouldFattenEffectfulLoops()) {
        // Fattening effectful loop, middle will preserve effect information
        TTP(List(sym), List(p), SimpleFatLoop(op.size, op.v, List(op.body)).copyCanBeFused(op))
      } else { // old loop fusion legacy mode
        if (!u.maySimple && !u.mayGlobal) {
          // assume body will reflect, too. bring it on...
          printdbg("-- fatten effectful loop " + e)
          TTP(List(sym), List(p), SimpleFatLoop(op.size, op.v, List(op.body)).copyCanBeFused(op))
        } else { 
          super.fatten(e)
        }
      }
    case _ => super.fatten(e)
  }

  override def combineFat(list: List[TTP]): TTP = list(0) match {
    case TTP(_, _, firstLoop: AbstractFatLoop) =>
      val shape = firstLoop.size
      val index = firstLoop.v
      val (lmhs, rhs) = list.map({ 
        case TTP(lhs, mhs, loop: AbstractFatLoop) => 
          if (shape != loop.size) {
            printlog("FAILED: combineFat of two loops with different sizes: " + firstLoop + " and " + loop)
            return super.combineFat(list)
          } else if (index != loop.v) {
            printlog("FAILED: combineFat of two loops with different indices: " + firstLoop + " and " + loop)
            return super.combineFat(list)
          } else if (!firstLoop.isFusedWith(loop)) {
            printlog("FAILED: combineFat of two loops that haven't been fused (call CanBeFused.registerFusion first): " + firstLoop + " and " + loop)
            return super.combineFat(list)
          }
          ((lhs, mhs), loop.body)
        case s => 
          printlog("FAILED: combineFat of a loop with something else: " + firstLoop + " and " + s)
          return super.combineFat(list)
      }).unzip
      val (lhs, mhs) = lmhs.unzip
      // The mhs lists the original statements including their effects
      TTP(lhs.flatten, mhs.flatten, SimpleFatLoop(shape, index, rhs.flatten).copyCanBeFused(firstLoop))

    case _ =>
      super.combineFat(list)
  }

}

trait BaseGenLoops extends GenericNestedCodegen {
  val IR: LoopsExp
  import IR._

}

trait BaseGenLoopsFat extends BaseGenLoops with BaseLoopsTraversalFat with GenericFatCodegen {
  val IR: LoopsFatExp
  import IR._

}

trait ScalaGenLoops extends ScalaGenBase with BaseGenLoops {
  import IR._

  //TODO

}

trait ScalaGenLoopsFat extends ScalaGenLoops with ScalaGenFat with BaseGenLoopsFat {
  import IR._

  //TODO

}

trait CLikeGenLoops extends CLikeGenBase with BaseGenLoops
trait CLikeGenLoopsFat extends CLikeGenLoops with CLikeGenFat with BaseGenLoopsFat

trait CGenLoops extends CGenBase with CLikeGenLoops
trait CGenLoopsFat extends CGenLoops with CGenFat with CLikeGenLoopsFat

trait GPUGenLoops extends GPUGenBase with CLikeGenLoops
trait GPUGenLoopsFat extends GPUGenLoops with GPUGenFat with CLikeGenLoopsFat 

trait CudaGenLoops extends CudaGenBase with GPUGenLoops
trait CudaGenLoopsFat extends CudaGenLoops with CudaGenFat with GPUGenLoopsFat

trait OpenCLGenLoops extends OpenCLGenBase with GPUGenLoops
trait OpenCLGenLoopsFat extends OpenCLGenLoops with OpenCLGenFat with GPUGenLoopsFat
