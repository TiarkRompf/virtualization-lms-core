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
        val iter: Int = 1000
        val arr2: Array[Int] = new Array[Int](size * size)
        val rnd2: Random = new Random(1) // We need to have a fixed seed
        for (i <- arr2.indices)
            arr2(i) = rnd2.nextInt(2)
        val gameOfLifeMatrix: MDArray[Int] = reshape(size :: size :: Nil, arr2)
        val func = new Experiment

        
        val start = System.currentTimeMillis
        
        var alive: MDArray[Int] = gameOfLifeMatrix
        for (i <- List.range(0, iter))
              alive = func(alive)
        println(alive)
        
        val stop = System.currentTimeMillis
        println("Loop execution time: " + (stop - start) + "ms")
    }
}
