package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal._

trait ArrayOps extends Variables {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToRepArrayOps[A:Manifest](x: Var[Array[A]]) = new RepArrayOpsCls(readVar(x))
  implicit def repArrayToRepArrayOps[T:Manifest](a: Rep[Array[T]]) = new RepArrayOpsCls(a)
  implicit def arrayToRepArrayOps[T:Manifest](a: Array[T]) = new RepArrayOpsCls(a)

  class RepArrayOpsCls[T:Manifest](a: Rep[Array[T]]){
    def apply(n: Rep[Int]) = array_apply(a, n)
    def length = array_length(a)
    def foreach(block: Rep[T] => Rep[Unit]) = array_foreach(a, block)
  }

  def array_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Int]): Rep[T]
  def array_length[T:Manifest](x: Rep[Array[T]]) : Rep[Int]
  def array_foreach[T:Manifest](x: Rep[Array[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
}

trait ArrayOpsExp extends ArrayOps with VariablesExp {

  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayApply[T](a: Exp[Array[T]], n: Exp[Int])(implicit val mT:Manifest[T]) extends Def[T]
  case class ArrayForeach[T](a: Exp[Array[T]], x: Exp[T], block: Exp[Unit]) extends Def[Unit]

  def array_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Int]): Rep[T] = ArrayApply(x, n)
  def array_length[T:Manifest](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)
  def array_foreach[T:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Unit]): Exp[Unit] = {
    val x = fresh[T]
    reflectEffect(ArrayForeach(a, x, reifyEffects(block(x))))
  }
}

trait BaseGenArrayOps extends GenericNestedCodegen {
  val IR: ArrayOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a,x,block) if shallow => syms(a)
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case ArrayForeach(a,x,block) => getFreeVarBlock(block,List(x.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }
}

trait ScalaGenArrayOps extends ScalaGenBase with BaseGenArrayOps {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case ArrayForeach(a,x,block) => stream.println("val " + quote(sym) + "=" + quote(a) + ".foreach{")
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayOps extends CLikeCodegen with BaseGenArrayOps {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
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
trait CGenArrayOps extends CGenBase with CLikeGenArrayOps

