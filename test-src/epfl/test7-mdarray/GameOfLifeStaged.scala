package scala.virtualization.lms
package epfl
package test7

import _root_.scala.util.Random
import original.MDArray
import common._
import test1.Arith

trait GameOfLifeStaged { this: MDArrayBase with IfThenElse =>
  def testGameOfLife(iter: Int, start: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {
    var alive: Rep[MDArray[Int]] = start
    for (i <- List.range(0, iter))
      alive = gameOfLife(alive)
    alive
  }

  def computeIfDead(neigh: Rep[MDArray[Int]], alive: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {

    if (alive === 1) {
      if ((neigh - alive) < 2)
        1 // Rule1: Any live cell with fewer than two live neighbours dies, as if caused by under-population.
      else if ((neigh - alive) < 4)
        0 // Rule2: Any live cell with two or three live neighbours lives on to the next generation.
      else
        1 // Rule3: Any live cell with more than three live neighbours dies, as if by overcrowding.
    }
    else
      0
  }

  def computeIfReborn(neigh: Rep[MDArray[Int]], alive: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {

    if (alive === 0) {
      if (neigh === 3)
        1 // Rule 4: Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
      else
        0
    } else
      0
  }

  def gameOfLife(alive: Rep[MDArray[Int]]) = {

    val dead = With(lbStrict = true, ubStrict = true, function =
      iv => computeIfDead(sum(tile(values(dim(alive), 3), iv-1, alive)), alive(iv))).GenArray(shape(alive))

    val reborn = With(lbStrict = true, ubStrict = true, function =
      iv => computeIfReborn(sum(tile(values(dim(alive), 3), iv-1, alive)), alive(iv))).GenArray(shape(alive))

    val result = alive - dead + reborn
    result
  }
}