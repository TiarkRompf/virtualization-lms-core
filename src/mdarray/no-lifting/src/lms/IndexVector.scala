package lms

import scala.collection.immutable._

/**
 * IndexVector object
 */
class IndexVector(ivList:List[Int]) extends Ordered[IndexVector] {

  /*
   * TODO: Make this a MDArray and implement the operations in a separate object
   * VLAD: The IndexArray is indeed an instance of a 1-dimensional MDArray[Int]. Still, there are two reasons for not
   *       making the IndexArray an instance of a MDArray
   *       * the IndexVector contains a set of specific operations, that assume the 1-dimensional property and the
   *         Int data type
   *       * creating a MDArray that takes an MDArray as a constructor parameter creates a closed dependency cycle,
   *         which we can only break by creating another separate class, extending MDArray, which we should represent
   *         a 1-dimensional version of the MDArray... that's basically going back to square 1, with more complex data
   *         structures :(
   *      At this point I see no serious reason to implement the IndexVector as an MDArray
   *
   * Feel free to comment if you think there is a good reason to implement the IndexVector as a MDArray 
   */

  def contentSize = ivList.foldLeft(1)((x:Int, y:Int) => x * y )
  def apply(i: Int) = ivList(i)
  def dim = ivList.length
  def shape = ivList

  // adds an entire IndexVector
  def +(that:IndexVector) = new IndexVector(this.shape ::: that.shape)
  // appends a single dimension, to the back
  def +(that:Int) = new IndexVector(this.shape ::: List[Int](that))

  // Simple minus operation with checks
  def -(that:IndexVector): IndexVector = {
    // compatibility checking
    if ((that.dim > dim) || (List.range(0, that.dim).filter(i => that(i) > this(i)).length != 0))
      throw new Exception("Invalid IndexVectors in difference operation: " + this + " - " + that)

    new IndexVector(shape.drop(that.dim))
  }
  
  def flatten(iv:IndexVector): Int = {
    // compatibility checking
    if ((iv.dim != dim) || (List.range(0, iv.dim).filter(i => iv(i) >= this(i)).length != 0))
      throw new Exception("Invalid IndexVectors in flatten operation: " + this + " flatten " + iv)
   
    flat(shape, iv.shape, 0)
  }

  private def flat(shape: List[Int], vector: List[Int], value: Int): Int = {
    if (shape.isEmpty)
      value
    else
      flat(shape.tail, vector.tail, (value * shape.head) + vector.head)
  }

  // makes the difference between two IVs
  def iterate(): Stream[IndexVector] = {

    val first = List.fill(dim)(0)
    val last = shape.map(x => x-1)

    IndexVector.iterate(new IndexVector(first), new IndexVector(last))
  }

  /**
   * Comparison: dirty hack
   */
  def compare(that: IndexVector): Int = {
    if (this.dim == that.dim) {
      var result:Int = 0
      for (i <- List.range(0, that.dim)) {
        if (result == 0) {
          if (that(i) > this(i)) result = -1
          if (that(i) < this(i)) result = 1
        }
      }
      result
    }
    else
      this.dim.compare(that.dim)
  }


  override def toString(): String =
    ivList.toString
}

/**
 * IndexVector companion object
 */
object IndexVector {

  def iterate(lb: IndexVector, ub: IndexVector) : Stream[IndexVector] = {
    Stream.cons(lb, nextOp(lb.shape, lb.shape, ub.shape))
  }

  private def nextOp(list: List[Int], lb: List[Int], ub: List[Int]) : Stream[IndexVector] = {
    try {
      val nextList = add(list.reverse, lb.reverse, ub.reverse).reverse
      Stream.cons(new IndexVector(nextList), nextOp(nextList, lb, ub))
    } catch {
      case error => Stream.empty
    }
  }

  private def add(list: List[Int], lb: List[Int], ub: List[Int]) : List[Int] = {
    if (list.head < ub.head)
      (list.head + 1) :: list.tail
    else
      lb.head :: add(list.tail, lb.tail, ub.tail)
  }
}