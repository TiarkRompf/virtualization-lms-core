package scala.virtualization.lms
package epfl
package test7

import test7.original.MDArray
import test7.original.Conversions._
import test7.original.Operations._
import scala.util.Random

/*
To run only this test use:
sbt 'test-only scala.virtualization.lms.epfl.test7.TestOriginalPDE1Benchmark'
*/
class TestOriginalPDE1Benchmark extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-original-"

  def testPower = {
    // Perform actual tests:
    val pde1 = new OriginalPDE1Benchmark

    // Create the matrix
    val size: Int = 10
    val arr: Array[Double] = new Array[Double](size * size * size)
    val rnd: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr.indices)
      arr(i) = rnd.nextDouble()
    val matrix: MDArray[Double] = reshape(size :: size :: size :: Nil, arr)

    withOutFile(prefix+"vector-test")(pde1.vectorTest)
//    withOutFile(prefix+"range1")(println(pde1.range1(matrix, 1).toString))
//    withOutFile(prefix+"range2")(println(pde1.range2(matrix, 1).toString))
//    withOutFile(prefix+"range3")(println(pde1.range3(matrix, 1).toString))
//    withOutFile(prefix+"range4")(println(pde1.range4(matrix, 1).toString))
//    withOutFile(prefix+"range5")(println(pde1.range5(matrix, 1).toString))
  }
}