package scala.virtualization.lms
package epfl
package test6

import common._
import test1._

import util.OverloadHack

import java.io.PrintWriter
import java.io.FileOutputStream

import org.scala_lang.virtualized.virtualize
import org.scala_lang.virtualized.SourceContext

trait Utils extends Base with OverloadHack {
  
  def infix_+(a: Rep[String], b: Rep[Any])(implicit x: Overloaded1): Rep[String]
  def infix_+(a: Rep[Any], b: Rep[String])(implicit x: Overloaded2): Rep[String]
  def infix_+(a: String, b: Rep[Any])(implicit x: Overloaded4): Rep[String]
  def infix_+(a: Rep[Any], b: String)(implicit x: Overloaded5): Rep[String]

  // TODO(trans): we do not have a solution for "string" + exp -- right now we use "string" ^ exp as workaround here

  implicit class strOps(a:Rep[Any]) {
    def +(b: Rep[String]) = infix_+(a,b)
    def ^(b: Rep[String]) = infix_+(a,b)
  }
  implicit class strOps1(a:String) {
    def +(b: Rep[Any]) = infix_+(a,b)
    def ^(b: Rep[Any]) = infix_+(a,b)
    def ^(b: String) = a + b
  }
  implicit class strOps2(a:Rep[String]) {
    def +(b: Rep[Any]) = infix_+(a,b)
    def ^(b: Rep[Any]) = infix_+(a,b)
  }

  
  implicit def unit(x:String): Rep[String]
  implicit def unit(x:Int): Rep[Int]
  
}


trait UtilExp extends BaseExp with Utils {

  implicit def unit(x:Int): Rep[Int] = Const(x)
  implicit def unit(x:String): Rep[String] = Const(x)
  
  def infix_+(a: Rep[String], b: Rep[Any])(implicit x: Overloaded1): Rep[String] = StrCat(a,b)
  def infix_+(a: Rep[Any], b: Rep[String])(implicit x: Overloaded2): Rep[String] = StrCat(a,b)
  def infix_+(a: String, b: Rep[Any])(implicit x: Overloaded4): Rep[String] = StrCat(Const(a),b)
  def infix_+(a: Rep[Any], b: String)(implicit x: Overloaded5): Rep[String] = StrCat(a,Const(b))

  case class StrCat(a: Exp[Any],b: Exp[Any]) extends Def[String]

  case class Tup[A,B](a: Exp[A],b: Exp[B]) extends Def[(A,B)]
  
  case class External[A:Manifest](s: String, fmt_args: List[Exp[Any]] = List()) extends Exp[A]
  
}

trait ScalaGenUtil extends ScalaGenBase {
  val IR: UtilExp
  import IR._
  
  // case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StrCat(a,b) =>
      emitValDef(sym, quote(a) + ".toString + " + quote(b) + ".toString")
    case Tup(a,b) =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) = x match {
    case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
    case _ => super.quote(x)
  }


}

trait Vectors extends Utils {

  type Vector
  implicit def mV: Manifest[Vector]

  def ZeroVector(n: Rep[Int]): Rep[Vector]
  def RandomVector(n: Rep[Int]): Rep[Vector]
  def infix_+(a: Rep[Vector], b: Rep[Vector])(implicit x: Overloaded3): Rep[Vector]

  implicit class VectorOps(a: Rep[Vector]) {
    def +(b: Rep[Vector]) = infix_+(a,b)
  }
}

trait VectorsExp extends Vectors with BaseExp { this: VectorsImpl =>

  // use Apply directly (instead of doApply) to signal that operations are pure

  def ZeroVector(n: Exp[Int]) = Apply(vectorZero, n)
  def RandomVector(n: Exp[Int]) = doApply(vectorRandom, n) // random vectors are different...

  def infix_+(a: Exp[Vector], b: Exp[Vector])(implicit x: Overloaded3) = (a,b) match {
    case (Def(ZeroVector(_)), b) => b
    case (a, Def(ZeroVector(_))) => a
    case _ => Apply(vectorPlus, toAtom(Tup(a, b)))
  }
  
  class ApplyExtractor[A:Manifest,B:Manifest](f: Exp[A => B]) {
    def apply(x: Exp[A]): Exp[B] = Apply(f,x)
    def unapply(e: Def[B]): Option[Exp[A]] = e match {
      case Apply(`f`, x: Exp[A]) => Some(x)
      case _ => None
    }
  }

  object ZeroVector extends ApplyExtractor[Int,Vector](vectorZero)  
  
/*  
  object ZeroVector {
    def unapply(e: Def[Vector]): Option[Exp[Int]] = e match {
      case Apply(`vectorZero`, n: Exp[Int]) => Some(n)
      case _ => 
        None
    }
  }
*/  
  
}


trait VectorsImpl extends Vectors with FunctionsExp with UtilExp {

  val vectorZero: Exp[Int => Vector]
  val vectorRandom: Exp[Int => Vector]
  val vectorPlus: Exp[((Vector,Vector)) => Vector]

}


trait VectorsImplExternal extends VectorsImpl {

  type Vector = Array[Double]
  def mV = manifest[Array[Double]]

  val base = "scala.virtualization.lms.epfl.test6.VectorOps.%s"
  
  // FIXME: using base + "zero" crashes the compiler!
  
  val vectorZero = External[Int => Vector](base format "zero")
  val vectorRandom = External[Int => Vector](base format "random")
  val vectorPlus = External[((Vector,Vector)) => Vector](base format "plus")

}

object VectorOps {
  
  def zero(n: Int) = new Array[Double](n)
  def random(n: Int) = new Array[Double](n)
  def plus(p: (Array[Double], Array[Double])) = p._1
  
}



// possible future alternatives:

trait VectorsImplConst extends VectorsImpl {

  type Vector = Array[Double]
  def mV = manifest[Array[Double]]

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


@virtualize
trait VectorsProg extends Vectors {
  
  def test(x: Rep[Unit]): Rep[Vector] = {
    RandomVector(7) + (ZeroVector(7) + RandomVector(7))
  }
  
}

@virtualize
trait StringsProg extends Vectors {
  
  def test(x: Rep[Any]) = {
    val s: Rep[Any] = "hi " ^ "yo " ^ x ^ " done"
    s
  }
  
}



class TestVectors extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test6-"
  
  def `testVectors ` = {
    withOutFile(prefix+"vectors") {
    
      println("-- begin")

      new VectorsProg with VectorsExp with VectorsImplExternal
      with CompileScala { self =>
        val codegen = new ScalaGenFunctions with ScalaGenUtil { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        val g = compile(test)
        println(g().mkString(","))
      }
    
      new StringsProg with VectorsExp with VectorsImplExternal
      with CompileScala { self =>
        val codegen = new ScalaGenFunctions with ScalaGenUtil { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        val g = compile(test)
        println(g(0))
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
    assertFileEqualsCheck(prefix+"vectors")
    
  }
}
