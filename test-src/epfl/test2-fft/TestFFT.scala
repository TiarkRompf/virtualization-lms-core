package scala.lms
package epfl
package test2

import common._
import test1._
import reflect.SourceContext

import java.io.PrintWriter

import org.scalatest._
import internal.ScalaCompile

trait FFT { this: Arith with Trig =>
  
  def omega(k: Int, N: Int): Complex = {
    val kth = -2.0 * k * math.Pi / N
    Complex(cos(kth), sin(kth))
  }

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)
    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)
    def *(that: Complex) = Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  }

  def splitEvenOdd[T](xs: List[T]): (List[T], List[T]) = (xs: @unchecked) match {
    case e :: o :: xt =>
      val (es, os) = splitEvenOdd(xt)
      ((e :: es), (o :: os))
    case Nil => (Nil, Nil)
    // cases?
  }

  def mergeEvenOdd[T](even: List[T], odd: List[T]): List[T] = ((even, odd): @unchecked) match {
    case (Nil, Nil) =>
      Nil
    case ((e :: es), (o :: os)) =>
      e :: (o :: mergeEvenOdd(es, os))
    // cases?
  }

  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

}





trait ArithExpOptFFT extends ArithExpOpt {

  override def infix_+(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (x, Def(Minus(Const(0.0) | Const(-0.0), y))) => infix_-(x, y)
    case _ => super.infix_+(x, y)
  }

  override def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (x, Def(Minus(Const(0.0) | Const(-0.0), y))) => infix_+(x, y)
    case _ => super.infix_-(x, y)
  }

  override def infix_*(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (x, Const(-1.0)) => infix_-(0.0, x)
    case (Const(-1.0), y) => infix_-(0.0, y)
    case _ => super.infix_*(x, y)
  }
}



trait TrigExpOptFFT extends TrigExpOpt {
  override def cos(x: Exp[Double]) = x match {
    case Const(x) if { val z = x / math.Pi / 0.5; z != 0 && z == z.toInt } => Const(0.0)
    case _ => super.cos(x)
  }
}


trait FlatResult extends BaseExp { // just to make dot output nicer

  case class Result(x: Any) extends Def[Any]
  
  def result(x: Any): Exp[Any] = toAtom(Result(x))
  
}

trait ScalaGenFlat extends ScalaGenBase {
   import IR._
   type Block[+T] = Exp[T]
   def getBlockResultFull[T](x: Block[T]): Exp[T] = x
   def reifyBlock[T:Manifest](x: =>Exp[T]): Block[T] = x
   def traverseBlock[A](block: Block[A]): Unit = {
     buildScheduleForResult(block) foreach traverseStm
   }
}



class TestFFT extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test2-"
  ScalaCompile.dumpGeneratedCode = false
  
  def testFFT1 = {
    withOutFile(prefix+"fft1") {
      val o = new FFT with ArithExp with TrigExpOpt with FlatResult with DisableCSE //with DisableDCE
      import o._

      val r = fft(List.tabulate(4)(_ => Complex(fresh, fresh)))
      println(globalDefs.mkString("\n"))
      println(r)
      
      val p = new ExportGraph with DisableDCE { val IR: o.type = o }
      p.emitDepGraph(result(r), prefix+"fft1-dot", true)
    }
    assertFileEqualsCheck(prefix+"fft1")
    assertFileEqualsCheck(prefix+"fft1-dot")
  }

  def testFFT2 = {
    withOutFile(prefix+"fft2") {
      val o = new FFT with ArithExpOptFFT with TrigExpOptFFT with FlatResult
      import o._

      case class Result(x: Any) extends Exp[Any]
      
      val r = fft(List.tabulate(4)(_ => Complex(fresh, fresh)))
      println(globalDefs.mkString("\n"))
      println(r)

      val p = new ExportGraph { val IR: o.type = o }
      p.emitDepGraph(result(r), prefix+"fft2-dot", true)
    }
    assertFileEqualsCheck(prefix+"fft2")
    assertFileEqualsCheck(prefix+"fft2-dot")
  }

  def testFFT3 = {
    withOutFile(prefix+"fft3") {
      class FooBar extends FFT
        with ArithExpOptFFT with TrigExpOptFFT with ArraysExp
        with CompileScala {

        def ffts(input: Rep[Array[Double]], size: Int) = {
          val list = List.tabulate(size)(i => Complex(input(2*i), input(2*i+1)))
          val r = fft(list)
          // make a new array for now - doing in-place update would be better
          makeArray(r.flatMap { case Complex(re,im) => List(re,im) })
        }
        
        val codegen = new ScalaGenFlat with ScalaGenArith with ScalaGenArrays { val IR: FooBar.this.type = FooBar.this } // TODO: find a better way...
      }
      val o = new FooBar
      import o._
    
      val fft4 = (input: Rep[Array[Double]]) => ffts(input, 4)
      codegen.emitSource1(fft4, "FFT4", new PrintWriter(System.out))
      val fft4c = compile1(fft4)
      println(fft4c(Array(1.0,0.0, 1.0,0.0, 2.0,0.0, 2.0,0.0, 1.0,0.0, 1.0,0.0, 0.0,0.0, 0.0,0.0)).mkString(","))
    }
    assertFileEqualsCheck(prefix+"fft3")
  }
  
}
