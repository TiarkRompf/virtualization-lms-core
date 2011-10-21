package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal._

trait ArrayOps extends Variables {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToArrayOps[A:Manifest](x: Var[Array[A]]) = new ArrayOpsCls(readVar(x))
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]) = new ArrayOpsCls(a)
  implicit def arrayToArrayOps[T:Manifest](a: Array[T]) = new ArrayOpsCls(unit(a))

  class ArrayOpsCls[T:Manifest](a: Rep[Array[T]]){
    def apply(n: Rep[Int]) = array_apply(a, n)
    def update(n: Rep[Int], y: Rep[T]) = array_update(a,n,y)
    def length = array_length(a)
    def foreach(block: Rep[T] => Rep[Unit]) = array_foreach(a, block)
  }

  def array_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Int]): Rep[T]
  def array_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T]): Rep[Unit]
  def array_length[T:Manifest](x: Rep[Array[T]]) : Rep[Int]
  def array_foreach[T:Manifest](x: Rep[Array[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
}

trait ArrayOpsExp extends ArrayOps with EffectExp with VariablesExp {

  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], n: Exp[Int], y: Exp[T]) extends Def[Unit]  
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayForeach[T](a: Exp[Array[T]], x: Sym[T], block: Exp[Unit]) extends Def[Unit]

  def array_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Int]): Exp[T] = ArrayApply(x, n)
  def array_update[T:Manifest](x: Exp[Array[T]], n: Exp[Int], y: Exp[T]) = ArrayUpdate(x,n,y)
  def array_length[T:Manifest](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)
  def array_foreach[T:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Unit]): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayForeach(a, x, b), summarizeEffects(b).star)
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = {
    (e match {
      case ArrayApply(a,x) => array_apply(f(a),f(x))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }
    
}

trait BaseGenArrayOps extends GenericNestedCodegen {
  val IR: ArrayOpsExp
  import IR._

}

trait ScalaGenArrayOps extends BaseGenArrayOps with ScalaGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case ArrayUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case ArrayForeach(a,x,block) => stream.println("val " + quote(sym) + "=" + quote(a) + ".foreach{")
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayOps extends BaseGenArrayOps with CLikeGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case ArrayLength(a) =>
          emitValDef(sym, " sizeof(" + quote(a) + ")")
        case arr@ArrayApply(a,n) =>
          emitValDef(sym, "" + quote(a) + "[" + quote(n) + "]")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayOps extends CudaGenBase with CLikeGenArrayOps
trait OpenCLGenArrayOps extends OpenCLGenBase with CLikeGenArrayOps
trait CGenArrayOps extends CGenBase with CLikeGenArrayOps

