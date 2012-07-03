package scala.virtualization.lms
package common

import java.io.PrintWriter
import collection.immutable.Stack
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.{GenericNestedCodegen,GenericFatCodegen}

trait Loops extends Base { // no surface constructs for now

}

trait LoopsExp extends Loops with BaseExp with EffectExp {

  abstract class AbstractLoop[A] extends Def[A] {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: Def[A]
  }

  object Yield {
    
    def apply(lvs: List[Exp[Int]], exps: List[Exp[Any]]): Def[Gen[Any]] = exps match {
      case x :: Nil => YieldSingle(lvs, x)
      case x :: y :: Nil => YieldTuple(lvs, (x, y))
      case Nil => throw new RuntimeException("Empty Yield not allowed!!!")
      case _ => ???
    }
    
    def unapply(g: Def[Gen[Any]]): Option[(List[Exp[Int]], List[Exp[Any]])] = g match {
      case YieldSingle(a, b) => Some((a, List(b)))
      case YieldTuple(a, b) => Some((a, b.productIterator.toList.asInstanceOf[List[Exp[Any]]]))
      case _ => None      
    } 
  }
  
  /**     
   * $yieldstmt
   * 
   *  @param  g   Represents list of loop vars in which this yield is nested.
   *  @param  a   Expression for the value that is being yielded.
   */
  case class YieldSingle[T](g: List[Exp[Int]], a: Exp[T]) extends Def[Gen[T]] {
    var concatSym: Option[Sym[T]] = None
  }

  /**
   * $yieldstmt
   * 
   * This is a special case of the Yield statement that is used for optimizing processing of tuples. 
   * It is used during code generation to avoid generation of excess tuples during aggregation.
   * 
   *  @param  g   Represents list of loop vars in which this yield is nested.
   *  @param  a   Expression for the value that is being yielded.
   */
  case class YieldTuple[A, B](g: List[Exp[Int]], a: (Exp[A], Exp[B])) extends Def[Gen[(A, B)]]
  
  /**
   * Skip statement is used in loops to indicate that no element is being emitted. For example in filter clauses, else branch will contain a Skip.
   * @param  g   Represents list of loop vars in which this skip is nested.
   */
  case class Skip[T](g: List[Exp[Int]]) extends Def[Gen[T]]
  
  def skip[T : Manifest](s: Exp[_], g: List[Exp[Int]]) =
    reflectWrite(s)(Skip[T](g))
  
  // used for convenient creation of yield statements
  var yieldStack: Stack[Exp[Gen[_]]] = Stack.empty
   
  def yields[T : Manifest](s: Exp[_], g: List[Exp[Int]], a: Exp[T]) = {
    val y = reflectWrite(s)(YieldSingle(g, a))
    yieldStack = yieldStack.push(y)
    y
  }
  
  def yields[A : Manifest, B : Manifest](s: Sym[_], g: List[Exp[Int]], a: (Exp[A], Exp[B])) = {
    val y = reflectWrite(s)(YieldTuple(g, a))
    yieldStack = yieldStack.push(y)
    y
  }
  
  /**
   * For now single type parameter.
   */
  def collectYields[T](e: => Block[Gen[T]]): (Exp[Gen[T]], Block[Gen[T]]) = {
    val block = e
    val res = (yieldStack.top.asInstanceOf[Exp[Gen[T]]], block)
    yieldStack = yieldStack.pop
    res
  }
  
  case class SimpleLoop[A](val size: Exp[Int], val v: Sym[Int], val body: Def[A]) extends AbstractLoop[A]
  
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


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(SimpleLoop(s,v,body), u, es) => 
      reflectMirrored(Reflect(SimpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case SimpleLoop(s,v,body) =>
      simpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f))(mtype(manifest[A]))
    case Reflect(YieldSingle(i, y), u, es) => 
      reflectMirrored(Reflect(YieldSingle(f(i), f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(YieldTuple(i, y), u, es) => 
      reflectMirrored(Reflect(YieldTuple(f(i),(f(y._1), f(y._2))), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(Skip(i), u, es) => 
      reflectMirrored(Reflect(Skip(f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case s @ Skip(i) => toAtom(Skip(f(i)))(mtype(manifest[A]), pos)
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

  abstract class AbstractFatLoop extends FatDef {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: List[Def[Any]]
  }
  
  case class SimpleFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop {
    var concatSym: Option[Sym[Any]] = None 
  }


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

  var genStack: Map[Exp[Gen[_]], List[String] => Unit] = Map.empty

  def withGens[A](p: List[(Exp[Gen[_]], List[String] => Unit)])(body: => A): A = {
    val save = genStack
    genStack = genStack ++ p

    val res = body
    genStack = save
    res
  }

  def withGen[T, A](g: Exp[Gen[T]], f: List[String] => Unit)(body: => A): A = withGens(List((g, f)))(body)

  def topGen[T](g: Exp[Gen[T]]): List[String] => Unit = {
    genStack.getOrElse(g, (s => "UNKNOWN: " + s))
  }

}

trait BaseGenLoopsFat extends BaseGenLoops with GenericFatCodegen {
  val IR: LoopsFatExp
  import IR._

  override def fatten(e: Stm): Stm = e match {
    case TP(sym, op: AbstractLoop[_]) =>
      TTP(List(sym), List(op), SimpleFatLoop(op.size, op.v, List(op.body)))
    case TP(sym, p @ Reflect(op: AbstractLoop[_], u, es)) if !u.maySimple && !u.mayGlobal => // assume body will reflect, too. bring it on...
      printdbg("-- fatten effectful loop " + e)
      TTP(List(sym), List(p), SimpleFatLoop(op.size, op.v, List(op.body)))
    case _ => super.fatten(e)
  }

}



trait ScalaGenLoops extends ScalaGenBase with BaseGenLoops {
  import IR._

   override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Yield(g, a) =>
      if (genStack.nonEmpty) {
        topGen(sym.asInstanceOf[Sym[Gen[Any]]])(a.map(quote))
      } else emitValDef(sym, "yield " + a.map(quote) + " // context is messed up!")
    case Skip(g) =>
      emitValDef(sym, "() // skip")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenLoopsFat extends ScalaGenLoops with ScalaGenFat with BaseGenLoopsFat {
  import IR._

  //TODO

}

trait CLikeGenLoops extends CLikeGenBase with BaseGenLoops
trait CLikeGenLoopsFat extends CLikeGenLoops with CLikeGenFat with BaseGenLoopsFat

trait GPUGenLoops extends GPUGenBase with CLikeGenLoops
trait GPUGenLoopsFat extends GPUGenLoops with GPUGenFat with CLikeGenLoopsFat 

trait CudaGenLoops extends CudaGenBase with GPUGenLoops
trait CudaGenLoopsFat extends CudaGenLoops with CudaGenFat with GPUGenLoopsFat

trait OpenCLGenLoops extends OpenCLGenBase with GPUGenLoops
trait OpenCLGenLoopsFat extends OpenCLGenLoops with OpenCLGenFat with GPUGenLoopsFat