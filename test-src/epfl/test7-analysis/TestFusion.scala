package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait TransformingStuff extends internal.Transforming with ArrayLoopsExp with ArithExp with PrintExp {

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    //case Copy(a) => f(a)
    case ArrayElem(y) => toAtom(ArrayElem(f(y)))
    case ReduceElem(y) => toAtom(ReduceElem(f(y)))
    case ArrayIndex(a,i) => toAtom(ArrayIndex(f(a), f(i)))
    case Plus(x,y) => infix_+(f(x), f(y))
    case Minus(x,y) => infix_-(f(x), f(y))
    case Times(x,y) => infix_*(f(x), f(y))
    case Div(x,y) => infix_/(f(x), f(y))
    case Reflect(Print(x), es) => toAtom(Reflect(Print(f(x)), es map (e => f(e))))
    case Reify(x, es) => toAtom(Reify(f(x), es map (e => f(e))))
  }).asInstanceOf[Exp[A]]
    
}



trait ScalaGenFatArrayLoopsFusionOpt extends ScalaGenArrayLoopsFat with LoopFusionOpt {
  val IR: ArrayLoopsFatExp
  import IR._  
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(a) => Some(a)
    case _ => super.unapplySimpleCollect(e)
  }

}


// trait NestLambdaProg extends Arith with Functions with Print 
// --> from TestCodeMotion.scala

trait FusionProg extends Arith with ArrayLoops with Print {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {
    
    val constant = array(100) { i => 1 }

    val linear = array(100) { i => 2*i }

    val affine = array(100) { i => constant.at(i) + linear.at(i) }
    
    def square(x: Rep[Double]) = x*x
    def mean(x: Rep[Array[Double]]) = sum(x.length) { i => x.at(i) } / x.length
    def variance(x: Rep[Array[Double]]) = sum(x.length) { i => square(x.at(i)) } / x.length - square(mean(x))
    
    val data = affine
    
    val m = mean(data)
    val v = variance(data)

    print(m)
    print(v)
  }
  
}


class TestFusion extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testFusion1 = {
    withOutFile(prefix+"fusion1") {
      new FusionProg with ArithExp with ArrayLoopsExp with PrintExp { self =>
        val codegen = new ScalaGenArrayLoops with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion1")
  }

  def testFusion2 = {
    withOutFile(prefix+"fusion2") {
      // LoopsExp2 with ArithExp with PrintExp with BaseFatExp
      new FusionProg with ArithExp with ArrayLoopsFatExp with PrintExp with TransformingStuff { self =>
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion2")
  }
 
}