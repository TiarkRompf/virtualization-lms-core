package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen,GenericFatCodegen}

trait Loops extends Base { // no surface constructs for now

}

trait LoopsExp extends Loops with BaseExp with EffectExp {

  abstract class AbstractLoop[A] extends Def[A] {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: Def[A]
  }

  /**
   * Super trait for all statements that emit results from the loop. For example: Yield and Skip.
   */
  trait Gen[+T]

  /**
   * Yield statement in loops. Indicates that element is being emitted to
   *  @param  g   Represents list of loop vars that in which this yield is nested.
   *  @param  a   Expression for the
   */
  case class Yield[T](g: List[Exp[Int]], a: Exp[T]) extends Def[Gen[T]]

  /**
   * Skip statement is used in loops to indicate that no element is being emitted. For example in filter clauses.
   *  @param  g   Represents list of loop vars that in which this yield is nested.
   */
  case class Skip[T](g: List[Exp[Int]]) extends Def[Gen[T]]

  case class SimpleLoop[A](val size: Exp[Int], val v: Sym[Int], val body: Def[A]) extends AbstractLoop[A]

  // TODO (VJ) Why does this not work? Simple loop and if then else should accept body as the parameter?
//  def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Block[A]): Exp[A] = reflectEffect(SimpleLoop(size, v, body),summarizeEffects(body).star)
  def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = SimpleLoop(size, v, body)


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

  abstract class AbstractFatLoop extends FatDef {
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




trait BaseGenLoops extends GenericNestedCodegen {
  val IR: LoopsExp
  import IR._

  // TODO: multiple gens
  var genStack: Map[Exp[Gen[_]], String => Unit] = Map.empty

  def withGens[A](p: List[(Exp[Gen[_]], String => Unit)])(body: => A): A = {
    val save = genStack
    genStack = genStack ++ p
    //println("--- withGens " + p + " == " + genStack)
    val res = body
    genStack = save
    res
  }

  def withGen[T, A](g: Exp[Gen[T]], f: String => Unit)(body: => A): A = withGens(List((g, f)))(body)

  def topGen[T](g: Exp[Gen[T]]): String => Unit = {
    genStack.getOrElse(g, (s => "UNKNOWN: " + s))
  }

}

trait BaseGenLoopsFat extends BaseGenLoops with GenericFatCodegen {
  val IR: LoopsFatExp
  import IR._

  override def fatten(e: TP[Any]): TTP = e.rhs match {
    case op: AbstractLoop[_] => 
      TTP(List(e.sym), SimpleFatLoop(op.size, op.v, List(op.body)))
    case Reflect(op: AbstractLoop[_], u, es) if !u.maySimple && !u.mayGlobal => // assume body will reflect, too. bring it on...
      printdbg("-- fatten effectful loop " + e)
      TTP(List(e.sym), SimpleFatLoop(op.size, op.v, List(op.body)))
    case _ => super.fatten(e)
  }

}


trait ScalaGenLoops extends ScalaGenBase with BaseGenLoops {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Yield(g, a) =>
      if (genStack.nonEmpty) {
        topGen(sym.asInstanceOf[Sym[Gen[Any]]])(quote(a))
      } else emitValDef(sym, "yield " + quote(a) + " // context is messed up!")
    case Skip(g) =>
      emitValDef(sym, "() // skip")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenLoopsFat extends ScalaGenLoops with ScalaGenFat with BaseGenLoopsFat {
  import IR._



}

trait CudaGenLoops extends CudaGenBase with BaseGenLoops {
  import IR._

  //TODO

}

trait CudaGenLoopsFat extends CudaGenLoops with CudaGenFat with BaseGenLoopsFat {
  import IR._

  //TODO

}

trait OpenCLGenLoops extends OpenCLGenBase with BaseGenLoops {
  import IR._

  //TODO

}

trait OpenCLGenLoopsFat extends OpenCLGenLoops with OpenCLGenFat with BaseGenLoopsFat {
  import IR._

  //TODO

}
