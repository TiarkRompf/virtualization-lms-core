package lms

import scala.collection.immutable._


object MainObject {
  def main(args: Array[String]): Unit = {
    testIndexVectors()
    testArrayReshapeAndSel()
    testGenArrayAndTake()
  }

  def testIndexVectors() {
    val iv1:IndexVector = new IndexVector(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: List[Int]())
    val iv2:IndexVector = new IndexVector(1 :: 2 :: 3 :: List[Int]())
    println("================================================================================")
    println("Testing Index Vectors")
    println("================================================================================")
    println("iv1 = " + iv1)
    println("iv2 = " + iv2)
    println("iv1 + 3 = " + (iv1 + 3))
    println("iv1 + iv2 = " + (iv1+iv2))
    println("iv1 - iv2 = " + (iv1-iv2))
    println("(iv1 - iv2).iterate:")
    for (t <- (iv1-iv2).iterate)
      print(t.toString+ "  ")
    println
    println
  }

  def testArrayReshapeAndSel() {
    val arr = new Array[Int](24)
    for (i <- List.range(0, 24))
      arr(i) = i + 10

    println("================================================================================")
    println("Testing Reshape and Sel")
    println("================================================================================")

    println("a(2,3,4):")
    val a1 = new MDArray(new IndexVector(List(2,3,4)), arr)
    println(a1.toString)

    println("reshape(a, (6,4)):")
    val a2 = MDArray.reshape(a1, new IndexVector(List(6,4)))
    println(a2.toString)

    println("sel(a, (0,2)):")
    val a3 = MDArray.sel(a1, new IndexVector(List(0, 2)))
    println(a3.toString)
  }

  def testGenArrayAndTake() {
    val arr = new Array[Int](3)
    for (i <- List.range(0, 3))
      arr(i) = i + 10

    println("================================================================================")
    println("Testing Genarray and Take")
    println("================================================================================")

    println("a(3):")
    val a1 = new MDArray(new IndexVector(List(3)), arr)
    println(a1.toString)

    println("genarray(a, (2,4)):")
    val a2 = MDArray.genArray(a1, new IndexVector(List(2,4)))
    println(a2.toString)

    println("take(a, (2,4,2)):")
    val a3 = MDArray.take(a2, new IndexVector(List(2, 4, 2)))
    println(a3.toString)
  }
}

