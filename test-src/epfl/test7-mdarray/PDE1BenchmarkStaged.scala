package scala.virtualization.lms
package epfl
package test7

import _root_.scala.util.Random
import original.MDArray
import common._
import test1.Arith

trait PDE1BenchmarkStaged { this: MDArrayBase with IfThenElse =>
  type MDArrayBool = Rep[MDArray[Boolean]]
  type MDArrayDbl  = Rep[MDArray[Double]]
  type MDArrayInt  = Rep[MDArray[Int]]
  type Dbl         = Rep[Double]

  def range1(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax1, iterations)
  def range2(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax2, iterations)
  def range3(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax3, iterations)
  //def range4(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax4, iterations)
  def range5(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax5, iterations)

  def vectorTest() = {
    val v1: Rep[MDArray[Int]] = 1::0::0::Nil
    val v2: Rep[MDArray[Int]] = 0::1::0::Nil
    val v3: Rep[MDArray[Int]] = 0::0::1::Nil

    reshape(1::1::3::Nil, v1 + v2 + v3)
  }

  // The PDE1BenchmarkStaged implementation
  def PDE1impl(matrix: MDArrayDbl,
               Relax: (MDArrayDbl, MDArrayDbl, Dbl) => MDArrayDbl,
               iterations: Int): MDArrayDbl = {
    val red: MDArrayBool = With(_lb = List(1, 0, 0), _step = List(2,1,1)).
      GenArray(shape(matrix), iv => true)

    var u = matrix
    val f = matrix
    // Luckily "iterations" is not staged :)
    for (i <- Stream.range(0, iterations)) {
      u = where(red, Relax(u, f, 1d/10d), u)
      u = where(!red, Relax(u, f, 1d/10d), u)
    }
    u
  }

  // The 5 'relax' methods
  def Relax1(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d

    With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      // TODO: Fix this forced conversion
      val local_sum = u(iv + convertFromListRep(List(1, 0, 0))) + u(iv - convertFromListRep(List(1, 0, 0))) +
                      u(iv + convertFromListRep(List(0, 1, 0))) + u(iv - convertFromListRep(List(0, 1, 0))) +
                      u(iv + convertFromListRep(List(0, 0, 1))) + u(iv - convertFromListRep(List(0, 0, 1)))
      // TODO: Fix this reordering
      // now factor gets converted to Rep[MDArray] scalar, instead of converting
      // the parenthesis to a Rep[Double]
      (f(iv) * hsq + local_sum) * factor
    })
  }

  def Relax2(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    With(_lbStrict=true, _ubStrict=true).ModArray(u, iv => {
      val block = tile(shape(W), iv-1, u)
      val local_sum = sum(W * block)
      // TODO: Fix this reordering
      (f(iv) * hsq + local_sum) * factor
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

// To stage this we need support for iterators
// TODO: Check if this can be easily staged
//
//      def Relax4(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {
//
//        val factor:Double = 1d/6d
//        var u1 = f * hsq
//
//        def justOne(size: Int, dim: Int, v: Int): MDArrayInt = {
//          val array = new Array[Int](size)
//          array(dim) = v
//          array
//        }
//
//        for (i <- List.range(0, dim(u))) {
//          u1 = u1 + shift(justOne(u.dim, i, 1), 0d, u)
//          u1 = u1 + shift(justOne(u.dim, i, -1), 0d, u)
//        }
//
//        CombineInnerOuter(u1 * factor, u)
//      }

  def Relax5(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    def justOne(size: Int, dim: Int, v: Int): MDArrayInt = {
      val array = new Array[Int](size)
      array(dim) = v
      array
    }

    val u1: MDArrayDbl = With(_lb=shape(W) * 0, _ub=shape(W)-1).Fold((a:MDArrayDbl, b:MDArrayDbl) => a+b, f * hsq, iv => shift(-iv + 1, 0.0, u))

    CombineInnerOuter(u1 * factor, u)
  }
}