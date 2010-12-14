package lms
import lms.Conversions._
import lms.With._
import lms.Operations._
import util.Random

object PDE1Benchmark {
  
  def main(args: Array[String]): Unit = {
    val size: Int = 10
    val iter: Int = 1
    val eps: Double = 1E-14
    val arr: Array[Double] = new Array[Double](size * size * size)
    val rnd: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr.indices)
      arr(i) = rnd.nextDouble()

    val matrix: MDArray[Double] = reshape(size :: size :: size :: Nil, arr)

    val result1: MDArray[Double] = Benchmark("Relax1", PDE1impl(matrix, Relax1, iter))
    val result2: MDArray[Double] = Benchmark("Relax2", PDE1impl(matrix, Relax2, iter))
    val result3: MDArray[Double] = Benchmark("Relax3", PDE1impl(matrix, Relax3, iter))
    val result4: MDArray[Double] = Benchmark("Relax4", PDE1impl(matrix, Relax4, iter))
    val result5: MDArray[Double] = Benchmark("Relax5", PDE1impl(matrix, Relax5, iter))
    println("Results from Relax1 and Relax2 are comparable: " + all((result1-result2) < eps))
    println("Results from Relax1 and Relax3 are comparable: " + all((result1-result3) < eps))
    println("Results from Relax1 and Relax4 are comparable: " + all((result1-result4) < eps))
    println("Results from Relax1 and Relax5 are comparable: " + all((result1-result5) < eps))
  }

  def Benchmark[A: ClassManifest](name: String, func: => A): A = {
    val time1 = System.currentTimeMillis
    val a: A = func
    val time2 = System.currentTimeMillis
    println(name+" took "+(time2-time1)+"ms")
    a
  }

  def PDE1impl(matrix: MDArray[Double],
               Relax: (MDArray[Double], MDArray[Double], Double) => MDArray[Double],
               iter: Int): MDArray[Double] = {

    def Where(mask: MDArray[Boolean], a: MDArray[Double], b: MDArray[Double]): MDArray[Double] = {
      With().GenArray(shape(a), iv => {
        if (mask(iv))
          a(iv)
        else
          b(iv)
      })
    }

    val red: MDArray[Boolean] = With(_lb = List(1, 0, 0), _step = List(2,1,1)).
      GenArray(shape(matrix), iv => true)

    var u = matrix
    val f = matrix
    for (i <- Stream.range(0, iter)) {
      u = Where(red, Relax(u, f, 1d/10d), u)
      u = Where(!red, Relax(u, f, 1d/10d), u)
    }
    u
  }

  def Relax1(u: MDArray[Double], f: MDArray[Double], hsq: Double): MDArray[Double] = {

    val factor:Double = 1d/6d

    With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      val local_sum = u(iv + List(1, 0, 0)) + u(iv - List(1, 0, 0)) +
                      u(iv + List(0, 1, 0)) + u(iv - List(0, 1, 0)) +
                      u(iv + List(0, 0, 1)) + u(iv - List(0, 0, 1))
      factor * (hsq * f(iv) + local_sum)
    })
  }

  def Relax2(u: MDArray[Double], f: MDArray[Double], hsq: Double): MDArray[Double] = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      val block = tile(shape(W), iv-1, u)
      val local_sum = sum(W * block)
      factor * (hsq * f(iv) + local_sum)
    })
  }

  def CombineInnerOuter(inner: MDArray[Double], outer: MDArray[Double]) =
    With(_lbStrict=true, _ubStrict=true).ModArray(outer, iv => inner(iv))

  def Relax3(u: MDArray[Double], f: MDArray[Double], hsq: Double): MDArray[Double] = {

    val factor:Double = 1d/6d
    val u1 = f * hsq
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    val u2 = u1 + With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      sum(W * tile(shape(W), iv-1, u))
    })
    CombineInnerOuter(u2 * factor, u)
  }

  def Relax4(u: MDArray[Double], f: MDArray[Double], hsq: Double): MDArray[Double] = {

    val factor:Double = 1d/6d
    var u1 = f * hsq

    def justOne(size: Int, dim: Int, v: Int): IndexVector = {
      val array = new Array[Int](size)
      array(dim) = v
      new SDArray[Int](array)
    }

    for (i <- List.range(0, dim(u))) {
      u1 = u1 + shift(justOne(u.dim, i, 1), 0d, u)
      u1 = u1 + shift(justOne(u.dim, i, -1), 0d, u)
    }

    CombineInnerOuter(u1 * factor, u)
  }

  def Relax5(u: MDArray[Double], f: MDArray[Double], hsq: Double): MDArray[Double] = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    def justOne(size: Int, dim: Int, v: Int): IndexVector = {
      val array = new Array[Int](size)
      array(dim) = v
      new SDArray[Int](array)
    }

    val u1 = With(_lb=shape(W) * 0, _ub=shape(W)-1).Fold((a:MDArray[Double], b:MDArray[Double]) => a+b, f * hsq, iv => shift(-iv + 1, 0d, u))

    CombineInnerOuter(u1 * factor, u)
  }
}