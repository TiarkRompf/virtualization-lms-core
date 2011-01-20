package scala.virtualization.lms
package epfl
package test7

import original.MDArray
import original.Conversions._
import original.Operations._
import original.With

import common._
import test1.Arith

class PDE1BenchmarkOriginal {
  type MDArrayBool = MDArray[Boolean]
  type MDArrayDbl = MDArray[Double]
  type MDArrayInt = MDArray[Int]
  type Dbl = Double

  def range1(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax1, iterations)
  def range2(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax2, iterations)
  def range3(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax3, iterations)
  def range4(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax4, iterations)
  def range5(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax5, iterations)

  /**
   * This is used to test the == correctness
   */
  def vectorTest() = {
    var arrays: List[MDArray[_]] = Nil

    arrays = (1::0::0::Nil) :: arrays
    arrays = (1::0::0::Nil) :: arrays
    arrays = (0::1::0::Nil) :: arrays
    arrays = (0::0::1::Nil) :: arrays
    arrays = (1.0::0.0::0.0::Nil) :: arrays
    arrays = (true::true::Nil) :: arrays
    arrays = (true::true::Nil) :: arrays
    arrays = reshape ((1::2::Nil), (1::0::Nil)) :: arrays
    arrays = reshape ((1::2::Nil), (1::0::Nil)) :: arrays
    arrays = reshape ((1::2::Nil), (1.0::0.0::Nil)) :: arrays
    arrays = reshape ((2::2::Nil), (1::0::1::0::Nil)) :: arrays
    arrays = reshape ((1::2::Nil), (true::false::Nil)) :: arrays
    arrays = 7 :: arrays
    arrays = 7 :: arrays
    arrays = 7.0 :: arrays
    arrays = 7.1 :: arrays
    arrays = true :: arrays
    arrays = arrays.reverse

    for(i <- Stream.range(0, arrays.length - 1))
      println("v" + i + "= " + arrays(i))

    for(i <- Stream.range(0, arrays.length - 1)) {
      for (j <- Stream.range(i+1, arrays.length - 1))
        println("test v" + i + " vs v" + j + ": " + (arrays(j) == arrays(i)) + (if ((arrays(i) == arrays(j)) != (arrays(j) == arrays(i))) "ASYMETRY DETECTED!" else ""))
      println("---")
    }
  }

  // The PDE1BenchmarkStaged implementation
  def PDE1impl(matrix: MDArrayDbl,
               Relax: (MDArrayDbl, MDArrayDbl, Dbl) => MDArrayDbl,
               iterations: Int): MDArrayDbl = {

    val startTime: Long = System.currentTimeMillis

    val red: MDArrayBool = With(_lb = List(1, 0, 0), _step = List(2,1,1)).
      GenArray(shape(matrix), iv => true)

    var u = matrix
    val f = matrix
    // Luckily "iterations" is not staged :)
    for (i <- Stream.range(0, iterations)) {
      u = where(red, Relax(u, f, 1d/10d), u)
      u = where(!red, Relax(u, f, 1d/10d), u)
    }
    val finishTime: Long = System.currentTimeMillis
    println("Time: " + (finishTime-startTime).toString + "ms")

    u
  }

  // The 5 'relax' methods
  def Relax1(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d

    With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      // TODO: Fix this forced conversion
      val local_sum = u(iv + List(1, 0, 0)) + u(iv - List(1, 0, 0)) +
                      u(iv + List(0, 1, 0)) + u(iv - List(0, 1, 0)) +
                      u(iv + List(0, 0, 1)) + u(iv - List(0, 0, 1))
      factor * (hsq * f(iv) + local_sum)
    })
  }

  def Relax2(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      val block = tile(shape(W), iv-1, u)
      val local_sum = sum(W * block)
      factor * (hsq * f(iv) + local_sum)
    })
  }

  def CombineInnerOuter(inner: MDArrayDbl, outer: MDArrayDbl) =
    With(_lbStrict=true, _ubStrict=true).ModArray(outer, iv => inner(iv))

  def Relax3(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val u1 = f * hsq
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    val u2 = u1 + With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      sum(W * tile(shape(W), iv-1, u))
    })
    CombineInnerOuter(u2 * factor, u)
  }

  def Relax4(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    var u1 = f * hsq

    def justOne(size: Int, dim: Int, v: Int): MDArrayInt = {
      val array = new Array[Int](size)
      array(dim) = v
      array
    }

    for (i <- List.range(0, dim(u))) {
      u1 = u1 + shift(justOne(u.dim, i, 1), 0d, u)
      u1 = u1 + shift(justOne(u.dim, i, -1), 0d, u)
    }

    CombineInnerOuter(u1 * factor, u)
  }

  def Relax5(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    def justOne(size: Int, dim: Int, v: Int): MDArrayInt = {
      val array = new Array[Int](size)
      array(dim) = v
      array
    }

    val u1 = With(_lb=shape(W) * 0, _ub=shape(W)-1).Fold((a:MDArrayDbl, b:MDArrayDbl) => a+b, f * hsq, iv => shift(-iv + 1, 0d, u))

    CombineInnerOuter(u1 * factor, u)
  }
}