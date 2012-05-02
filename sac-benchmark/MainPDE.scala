package scala.virtualization.lms
package epfl
package test7
package original

import scala.util.Random
import Operations._
import Conversions._

object Main {

    def main(args: Array[String]) = {
        // Create the matrix
        val size: Int = 10
        val arr: Array[Double] = new Array[Double](size * size * size)
        val rnd: Random = new Random(1) // We need to have a fixed seed
        for (i <- arr.indices)
            arr(i) = rnd.nextDouble()
        val matrix: MDArray[Double] = reshape(size :: size :: size :: Nil, arr)
        val func = new Experiment

        val start = System.currentTimeMillis
        println(func(matrix))
        val stop = System.currentTimeMillis
        println("Loop execution time: " + (stop - start) + "ms")
      }
}
