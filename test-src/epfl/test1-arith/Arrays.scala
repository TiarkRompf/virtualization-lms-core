package scala.lms
package epfl
package test1

import common._

import java.io.PrintWriter


trait Arrays extends Base {

  class ArrayOps[T:Typ](x: Rep[Array[T]]) {
    def apply(i: Int) = arrayApply(x, i)
  }
  implicit def array2arrayOps[T:Typ](x: Rep[Array[T]]) = new ArrayOps(x)

  def arrayApply[T:Typ](x: Rep[Array[T]], i:Int): Rep[T]
  //def arrayUpdate(x: Rep[Double]): Rep[Unit]
  def makeArray[T:Typ](x: List[Rep[T]]): Rep[Array[T]]
}

trait ArraysExp extends Arrays with BaseExp {
  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp

  case class ArrayApply[T:Typ](x:Rep[Array[T]], i:Int) extends Def[T]
  //case class ArrayUpdate[T](x:Rep[Array[T]], i:Int) extends Def[T]
  case class MakeArray[T:Typ](x:List[Rep[T]]) extends Def[Array[T]]

  def arrayApply[T:Typ](x: Rep[Array[T]], i:Int) = ArrayApply(x, i)
  //def arrayUpdate(x: Rep[Double]) = ArrayUpdate(x)
  def makeArray[T:Typ](x: List[Rep[T]]) = MakeArray(x)
}

trait ScalaGenArrays extends ScalaGenBase {
  val IR: ArraysExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,i) =>  emitValDef(sym, "" + quote(x) + ".apply(" + i + ")")
    case MakeArray(x) =>  emitValDef(sym, "Array(" + x.map(quote).mkString(",") + ")")
    case _ => super.emitNode(sym, rhs)
  }
}