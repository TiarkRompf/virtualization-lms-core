package scala.virtualization.lms
package epfl
package test2

import common._
import test1._

trait MergeSort { this: Relat =>

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


class TestSort extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test2-"

  def `testSort ` = {
    withOutFile(prefix+"sort1") {
      val o = new MergeSort with RelatExpOpt with FlatResult
      import o._

      val r = sort(List.tabulate(8)(_ => fresh))
      println(globalDefs.mkString("\n"))
      println(r)

      val p = new ExportGraph { val IR: o.type = o }
      p.emitDepGraph(result(r), prefix+"sort1-dot", true)
    }
    assertFileEqualsCheck(prefix+"sort1")
    assertFileEqualsCheck(prefix+"sort1-dot")
  }
}
