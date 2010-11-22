package lms

import scala.collection.immutable._


object MainObject {
  def main(args: Array[String]): Unit = {
    testIndexVectors()
    testArrayReshapeAndSel()
    testModarray()
    testGenArrayAndTake()
    testCat()
  }

  def testIndexVectors() {
    val iv1:IndexVector = new IndexVector(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: List[Int]())
    val iv2:IndexVector = new IndexVector(0 :: 1 :: 1 :: List[Int]())
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

    println("a1(2,3,4):")
    val a1 = new MDArray(new IndexVector(List(2,3,4)), arr)
    println(a1.toString)

    println("a2 = reshape(a1, (6,4)):")
    val a2 = MDArray.reshape(a1, new IndexVector(List(6,4)))
    println(a2.toString)

    println("a3 = sel(a1, (0,2)):")
    val a3 = MDArray.sel(a1, new IndexVector(List(0, 2)))
    println(a3.toString)
  }


  def testModarray() {
    val arr = new Array[Int](24)
    for (i <- List.range(0, 24))
      arr(i) = i + 10

    val arr2 = new Array[Int](4)
    for (i <- List.range(0, 4))
      arr2(i) = 42

    println("================================================================================")
    println("Testing ModArray")
    println("================================================================================")

    println("a1(2,3,4):")
    val a1 = new MDArray(new IndexVector(List(2,3,4)), arr)
    println(a1.toString)

    println("a2(4):")
    val a2 = new MDArray(new IndexVector(List(4)), arr2)
    println(a2.toString)

    println("a3 = modarray(a1, (0,1), a2):")
    val a3 = MDArray.modarray(a1, new IndexVector(List(0,1)), a2)
    println(a3.toString)
  }


  def testGenArrayAndTake() {
    val arr = new Array[Int](3)
    for (i <- List.range(0, 3))
      arr(i) = i + 10

    println("================================================================================")
    println("Testing Genarray, Take and Drop")
    println("================================================================================")

    println("a1(3):")
    val a1 = new MDArray(new IndexVector(List(3)), arr)
    println(a1.toString)

    println("a2 = genarray(a1, (2,4)):")
    val a2 = MDArray.genArray(a1, new IndexVector(List(2,4)))
    println(a2.toString)

    println("a3 = take(a2, (2,4,2)):")
    val a3 = MDArray.take(a2, new IndexVector(List(2, 4, 2)))
    println(a3.toString)

    println("a4 = drop(a2, (1,3,1)):")
    val a4 = MDArray.drop(a2, new IndexVector(List(1, 3, 1)))
    println(a4.toString)
  }


  def testCat() {
    val arr = new Array[Int](24)
    for (i <- List.range(0, 24))
      arr(i) = i + 10

    val arr2 = new Array[Int](12)
    for (i <- List.range(0, 12))
      arr2(i) = i + 50

    println("================================================================================")
    println("Testing Cat (concatenation)")
    println("================================================================================")

    println("a1(3,8):")
    val a1 = new MDArray(new IndexVector(List(3,8)), arr)
    println(a1.toString)

    println("a2(3,4):")
    val a2 = new MDArray(new IndexVector(List(3,4)), arr2)
    println(a2.toString)

    println("a3 = cat(1, a1, a2):")
    val a3 = MDArray.cat(1, a1, a2)
    println(a3.toString)
  }

}

