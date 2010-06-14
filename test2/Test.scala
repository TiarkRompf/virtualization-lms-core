package test2

import test1._

import java.io.PrintWriter


trait TestPower1 { this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] = 
    if (x == 0) 1.0 else b * power(b, x - 1)
}

trait TestPower2 { this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] = 
    if (x == 0) 1.0
    else if ((x&1) == 0) { val y = power(b, x/2); y * y }
    else b * power(b, x - 1)
}


trait BaseStr extends Base {
  type Rep[+T] = String
}

trait ArithStr extends Arith with BaseStr {
  implicit def unit(x: Double) = x.toString

  def __ext__+(x: Rep[Double], y: Rep[Double]) = "(%s+%s)".format(x,y)
  def __ext__-(x: Rep[Double], y: Rep[Double]) = "(%s-%s)".format(x,y)
  def __ext__*(x: Rep[Double], y: Rep[Double]) = "(%s*%s)".format(x,y)
  def __ext__/(x: Rep[Double], y: Rep[Double]) = "(%s/%s)".format(x,y)
}



trait TestFFT { this: Arith with Trig =>
  
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





/*
trait TestSort { this: Relat =>

  def splitOddEven[T](xs: List[T]): (List[T], List[T]) = (xs: @unchecked) match {
    case o :: e :: xt =>
      val (os, es) = splitOddEven(xt)
      ((o :: os), (e :: es))
    case Nil => (xs, xs)
    // cases?
  }

  def mergeOddEven[T](odd: List[T], even: List[T]): List[T] = ((odd, even): @unchecked) match {
    case (Nil, Nil) =>
      Nil
    case ((o :: os), (e :: es)) =>
      o :: (e :: mergeOddEven(os, es))
    // cases?
  }
  
  def merge(xs: List[Rep[Double]]): List[Rep[Double]] = (xs: @unchecked) match {
    case o :: e :: Nil =>
      min(o, e) :: max(o, e) :: Nil
    case _ =>
      val (odd0, even0) = splitOddEven(xs)
      val (odd1, even1) = (merge(odd0), merge(even0))
      val (odd2, even2) = odd1 zip even1 map {
        case (x, y) => 
          (min(x,y), max(x,y))
      } unzip;
      mergeOddEven(odd2, even2)
  }

  def sort(xs: List[Rep[Double]]): List[Rep[Double]] = xs match {
    case (x :: Nil) =>
      xs
    case _ =>
      val N = xs.length // should assert it's power of two

      val (left0, right0) = xs.splitAt(N/2)
      
      val (left1, right1) = (sort(left0), sort(right0))
      
      merge(left1 ::: right1)
  }

}
*/

object Test {
  
  def main(args: Array[String]) = {
/*    
    println {
      val o = new TestPower with ArithRepDirect
      import o._
      power(2,4)
    }

    println {
      val o = new TestPower with ArithRepString
      import o._
      power(2,4)
    }
    
    println {
      val o = new TestPower with ArithRepString
      import o._
      power("x",4)
    }

    println {
      val o = new TestPower with ArithRepString
      import o._
      power("(x + y)",4)
    }
*/
    {
      val o = new TestPower1 with ArithStr
      import o._

      val r = power(__ext__+("x0","x1"),4)
      println(r)
    }
    {
      val o = new TestPower2 with ArithStr
      import o._

      val r = power(__ext__+("x0","x1"),4)
      println(r)
    }
    {
      val o = new TestPower1 with ArithExp with GraphVizExport
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test2-power1-dot")
    }

    {
      val o = new TestPower1 with ArithExpOpt with GraphVizExport
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test2-power2-dot")
    }
    {
      val o = new TestPower1 with ArithExpOpt with ScalaCompile with ScalaCodegenArith
      import o._
      val f = (x: Rep[Double]) => power(x + x, 4)
      emitScalaSource(f, "Power2", new PrintWriter(System.out))
    }

    {
      val o = new TestPower2 with ArithExpOpt with GraphVizExport
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test2-power3-dot")
    }
    {
      val o = new TestPower2 with ArithExpOpt with ScalaCompile with ScalaCodegenArith
      import o._
      val f = (x: Rep[Double]) => power(x + x, 4)
      emitScalaSource(f, "Power3", new PrintWriter(System.out))
    }

    {
      val o = new TestFFT with ArithExp with TrigExpOpt with GraphVizExport with DisableCSE with DisableDCE
      import o._

      case class Result(x:Any) extends Def[Any]

      val r = fft(List.tabulate(4)(_ => Complex(fresh, fresh)))
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(toAtom(Result(r)), "test2-fft1-dot", true)
    }

    {
      val o = new TestFFT with ArithExpOpt2 with TrigExpOpt2 with GraphVizExport
      import o._

      case class Result(x:Any) extends Def[Any]

      val r = fft(List.tabulate(4)(_ => Complex(fresh, fresh)))
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(toAtom(Result(r)), "test2-fft2-dot", true)
    }

/*
    {
      val o = new TestSort with RelatExpOpt with GraphVizExport
      import o._

      case class Result(x:Any) extends Def[Any]

      val r = sort(List.tabulate(8)(_ => fresh))
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(toAtom(Result(r)), "test2-sort1-dot", true)
    }
*/

    {
      val o = new TestPower1 with ArithExpOpt with ScalaCompile with ScalaCodegenArith
      import o._

      val power4 = (x:Rep[Double]) => power(x,4)
      emitScalaSource(power4, "Power4", new PrintWriter(System.out))
      val power4c = compile(power4)
      println(power4c(2))
    }

    {
      class FooBar extends TestFFT
        with ArithExpOpt2 with TrigExpOpt2 with ArrayExp
        with ScalaCompile with ScalaCodegenArith with ScalaCodegenArrays {

        def ffts(input: Rep[Array[Double]], size: Int) = {
          val list = List.tabulate(size)(i => Complex(input(2*i), input(2*i+1)))
          val r = fft(list)
          // make a new array for now - doing in-place update would be better
          makeArray(r.flatMap { case Complex(re,im) => List(re,im) })
        }
      }
      val o = new FooBar
      import o._


      val fft4 = (input: Rep[Array[Double]]) => ffts(input, 4)
      emitScalaSource(fft4, "FFT4", new PrintWriter(System.out))
      val fft4c = compile(fft4)
      println(fft4c(Array(1.0,0.0, 1.0,0.0, 2.0,0.0, 2.0,0.0, 1.0,0.0, 1.0,0.0, 0.0,0.0, 0.0,0.0)).mkString(","))
  
    }
    
  }
  
}