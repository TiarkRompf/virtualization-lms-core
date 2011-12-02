package scala.virtualization.lms
package epfl
package test10

import test10.original.MDArray
import test10.original.Conversions._
import test10.original.Operations._
import scala.util.Random

/*
 * To run only this test use:
 * sbt 'test-only scala.virtualization.lms.epfl.test10.TestOriginalPDE1Benchmark'
 */
class TestOriginal extends FileDiffSuite {

  val prefix = "test-out/epfl/test10-original-"

  def testPDE = {
    val pde1 = new PDE1BenchmarkOriginal

    // We want deterministic random
    Random.setSeed(0);

    val size: Int = 10
    val arr: Array[Double] = new Array[Double](size * size * size)
    val rnd: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr.indices)
      arr(i) = rnd.nextDouble()
    val matrix: MDArray[Double] = reshape(size :: size :: size :: Nil, arr)

    // PDE1 experiments
    System.err.println("PDE1 Benchmark")
    withOutFile(prefix+"pde-begin")(println(matrix))
    timed("range1", withOutFile(prefix+"pde-range1")(println(pde1.range1(matrix, 1).toString))); System.gc
    timed("range2", withOutFile(prefix+"pde-range2")(println(pde1.range2(matrix, 1).toString))); System.gc
    timed("range3", withOutFile(prefix+"pde-range3")(println(pde1.range3(matrix, 1).toString))); System.gc
    timed("range4", withOutFile(prefix+"pde-range4")(println(pde1.range4(matrix, 1).toString))); System.gc
    timed("range5", withOutFile(prefix+"pde-range5")(println(pde1.range5(matrix, 1).toString))); System.gc 
    assertFileEqualsCheck(prefix+"pde-begin")
    assertFileEqualsCheck(prefix+"pde-range1")
    assertFileEqualsCheck(prefix+"pde-range2")
    assertFileEqualsCheck(prefix+"pde-range3")
    assertFileEqualsCheck(prefix+"pde-range4")
    assertFileEqualsCheck(prefix+"pde-range5")
  }

  def testGameOfLife = {
    val gol = new GameOfLifeOriginal

    // We want deterministic random
    Random.setSeed(0);

    // Create the matrix
    val size: Int = 10
    val arr2: Array[Int] = new Array[Int](size * size)
    // Draw a "glider" in the upper left corner ans see it flying :)
    // http://en.wikipedia.org/wiki/Conway's_Game_of_Life#Examples_of_patterns
    arr2(12) = 1
    arr2(23) = 1
    arr2(31) = 1
    arr2(32) = 1
    arr2(33) = 1
    val gameOfLifeMatrix: MDArray[Int] = reshape(size :: size :: Nil, arr2)

    // Game of Life experiments
    System.err.println("Game of life")
    withOutFile(prefix+"game-of-life-begin")(println(gameOfLifeMatrix)); System.gc
    timed("gol-glider", withOutFile(prefix+"game-of-life")(gol.testGameOfLife(20,gameOfLifeMatrix))); System.gc
    assertFileEqualsCheck(prefix+"game-of-life-begin")
    assertFileEqualsCheck(prefix+"game-of-life")
  }
  
  def timed(name: String, code: => Unit) = {
    
    val start = System.currentTimeMillis()
    code
    val stop = System.currentTimeMillis()
    System.err.println("  time for " + name + ": " + (stop - start) + "ms")
  }
}
