package scala.virtualization.lms
package epfl
package test6

import common._
import test1._

import java.io.PrintWriter
import java.io.FileOutputStream


trait UtilExp extends BaseExp {

  case class Tup[A,B](a: Exp[A],b: Exp[B]) extends Def[(A,B)]
  
  case class External[A](s: String) extends Exp[A]
  
}

trait ScalaGenUtil extends ScalaGenBase with UtilExp {

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Tup(a,b) =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[_]) = x match {
    case External(s) => s
    case _ => super.quote(x)
  }


}

trait Vectors extends Base {

  implicit def unit(x:Int): Rep[Int]

  type Vector

  def ZeroVector(n: Rep[Int]): Rep[Vector]
  def RandomVector(n: Rep[Int]): Rep[Vector]
  def __ext__+(a: Rep[Vector], b: Rep[Vector]): Rep[Vector]
}

trait VectorsExp extends Vectors with BaseExp { this: VectorsImpl =>

  implicit def unit(x:Int) = Const(x)

  // use Apply directly (instead of doApply) to signal that operations are pure

  def ZeroVector(n: Exp[Int]) = Apply(vectorZero, n)
  def RandomVector(n: Exp[Int]) = doApply(vectorRandom, n) // random vectors are different...

  def __ext__+(a: Exp[Vector], b: Exp[Vector]) = (a,b) match {
    case (Def(ZeroVector(_)), b) => b
    case (a, Def(ZeroVector(_))) => a
    case _ => Apply(vectorPlus, Tup(a, b))
  }
  
  object ZeroVector {
    def unapply(e: Def[Vector]): Option[Exp[Int]] = e match {
      case Apply(`vectorZero`, n: Exp[Int]) => Some(n)
      case _ => 
        None
    }
  }
  
  
}


trait VectorsImpl extends Vectors with FunctionsExp with UtilExp {

  val vectorZero: Exp[Int => Vector]
  val vectorRandom: Exp[Int => Vector]
  val vectorPlus: Exp[((Vector,Vector)) => Vector]

}


trait VectorsImplExternal extends VectorsImpl {

  type Vector = Array[Double]

  val base = "scala.virtualization.lms.epfl.test6.VectorOps.%s"
  
  // FIXME: using base + "zero" crashes the compiler!
  
  val vectorZero = External(base format "zero")
  val vectorRandom = External(base format "random")
  val vectorPlus = External(base format "plus")

}

object VectorOps {
  
  def zero(n: Int) = new Array[Double](n)
  def random(n: Int) = new Array[Double](n)
  def plus(p: (Array[Double], Array[Double])) = p._1
  
}



// possible future alternatives:

trait VectorsImplConst extends VectorsImpl {

  type Vector = Array[Double]

  // kernels implementations as function-type constants

  val vectorZero = Const((n:Int) => new Array[Double](n))
  val vectorRandom = Const((n:Int) => new Array[Double](n)) // fill with random data...
  val vectorPlus = Const((p:(Vector,Vector)) => p._1) // actually do the addition

}


/*

trait VectorImplInternal extends VectorImpl {

  trait Vector

  // kernels implemented as staged coded within lambdas

  val vectorZero = doLambda { x:Rep[Int] => ... }
  val vectorRandom = doLambda { x:Rep[Int] => ... }
  val vectorPlus = doLambda { x:Rep[(Vector,Vector)] => ... }

}

*/


trait TestVectors extends Vectors {
  
  def test(x: Rep[Unit]) = {
    RandomVector(7) + (ZeroVector(7) + RandomVector(7))
  }
  
}


object TestTestVectors {
  
  def main(args: Array[String]) = {

    println("-- begin")

    new TestVectors with VectorsExp with VectorsImplExternal
    with CompileScala 
    with ScalaGenFunctions with ScalaGenUtil
    {
      emitScalaSource(test, "Test", new PrintWriter(System.out))
      val g = compile(test)
      println(g())
    }
    
/*
    new TestConditional with ArithExpOpt with EqualExp with PrintExp
    with JSGenIfThenElse
    with JSGenArith with JSGenEqual with JSGenPrint
    {
      val f = (x: Rep[Double]) => test(x)
      emitJSSource(f, "main", new PrintWriter(System.out))
      emitHTMLPage(() => f(7), new PrintWriter(new FileOutputStream("test5.html")))
    }

*/
    println("-- end")
  }
}