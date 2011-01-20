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
    val pde1 = new PDE1BenchmarkOriginal
    val gol = new GameOfLifeOriginal

    // Create the matrix
    val size: Int = 10
    val arr: Array[Double] = new Array[Double](size * size * size)
    val rnd: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr.indices)
      arr(i) = rnd.nextDouble()
    val matrix: MDArray[Double] = reshape(size :: size :: size :: Nil, arr)

    val arr2: Array[Int] = new Array[Int](size * size)
    val rnd2: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr2.indices)
      arr2(i) = rnd2.nextInt(2)
    val gameOfLifeMatrix: MDArray[Int] = reshape(size :: size :: Nil, arr2)

    // PDE1 experiments
    withOutFile(prefix+"range1")(println(pde1.range1(matrix, 1).toString)); System.gc
    withOutFile(prefix+"range2")(println(pde1.range2(matrix, 1).toString)); System.gc
    withOutFile(prefix+"range3")(println(pde1.range3(matrix, 1).toString)); System.gc
    withOutFile(prefix+"range4")(println(pde1.range4(matrix, 1).toString)); System.gc
    withOutFile(prefix+"range5")(println(pde1.range5(matrix, 1).toString)); System.gc

    // Some old test attached to PDE1 TODO: Remove at some point
    withOutFile(prefix+"vector-test")(pde1.vectorTest)

    // Game of Life experiments
    withOutFile(prefix+"game-of-life-begin")(println(gameOfLifeMatrix))
    withOutFile(prefix+"game-of-life")(gol.testGameOfLife(1000,gameOfLifeMatrix))
  }
}