package lms

import scala.collection.immutable._

/** IndexVector object */
class IndexVector(ivList:List[Int]) {

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


  /**
   * Flatten returns the index in a 1-dimensional array corresponding to the multidimensional indexVector this
   * The shape of the multidimensional array must be given.
   */
  def flatten(iv:IndexVector): Int = {
    // size compatibility checking
    if (!(this isElementWiseGreaterThen iv))
      throw new Exception("Invalid IndexVectors in flatten operation: " + this + " flatten " + iv)

    def flat(shape: List[Int], vector: List[Int], value: Int): Int = {
      if (shape.isEmpty)
        value
      else
        flat(shape.tail, vector.tail, (value * shape.head) + vector.head)
    }

    flat(shape, iv.shape, 0)
  }


  /** concatenates two IndexVectors */
  def +(that:IndexVector) = new IndexVector(this.shape ::: that.shape)


  /** concatenates another dimension to the IndexVector */
  def +(that:Int) = new IndexVector(this.shape ::: List[Int](that))


  /** subtracts the given prefix from the IndexVector */
  def -(that:IndexVector): IndexVector = {
    // size compatibility checking
    if (!(this isPrefixWiseGreaterThan that))
      throw new Exception("Invalid IndexVectors in difference operation: " + this + " - " + that)

    new IndexVector(shape.drop(that.dim))
  }


  /** concatenate along an axis */
  def axisConcatenation(that:IndexVector, axis: Int):IndexVector = {
    // size compatibility checks
    if ((that.dim != this.dim) || (List.range(0, that.dim).filter(i => ((that(i) != this(i)) && (i != axis))).length != 0))
      throw new Exception("Impossible to concatenate " + this + " and " + that + " on axis " + axis)

    val newShape = List.range(0, this.dim).map(i => if (i==axis) this(i)+that(i) else this(i))
    new IndexVector(newShape)
  }


  /** element by element difference */
  def elementWiseDifference(that:IndexVector):IndexVector = {
    // size compatibility checks
    if (!(this isElementWiseGreaterThen that))
      throw new Exception("Impossible to compute element-wise difference between " + this + " and " + that)

    val newShape = List.range(0, this.dim).map(i => this(i) - that(i))
    new IndexVector(newShape)
  }

    /** element by element difference */
  def elementWiseAdd(that:IndexVector):IndexVector = {
    // size compatibility checks
    if (!(this.dim == that.dim))
      throw new Exception("Impossible to compute element-wise sum between " + this + " and " + that)

    val newShape = List.range(0, this.dim).map(i => this(i) + that(i))
    new IndexVector(newShape)
  }

  /** Starts an element-by-element iteration in [0..0, this) */
  def iterate(): Stream[IndexVector] = {

    val first = List.fill(dim)(0)
    val last = shape.map(x => x-1)

    IndexVector.iterate(new IndexVector(first), new IndexVector(last))
  }


  /** compares the prefix of two IndexVectors */
  def isPrefixWiseGreaterThan(that: IndexVector): Boolean =
    ((that.dim < this.dim) && (List.range(0, that.dim).filter(i => that(i) > this(i)).length == 0))

  /** compares the prefix of two IndexVectors */
  def isPrefixWiseEqualTo(that: IndexVector): Boolean =
    ((that.dim < this.dim) && (List.range(0, that.dim).filter(i => that(i) != this(i)).length == 0))

  /** compares two IndexVectors element by element */
  def isElementWiseGreaterThen(that: IndexVector): Boolean =
    ((that.dim == this.dim) && (List.range(0, that.dim).filter(i => that(i) > this(i)).length == 0))

  /** compares two IndexVectors element by element */
  def isElementWiseGreaterOrEqualTo(that: IndexVector): Boolean =
    ((that.dim == this.dim) && (List.range(0, that.dim).filter(i => that(i) >= this(i)).length == 0))

  /** compares two IndexVectors element by element */
  def isElementWiseEqualTo(that: IndexVector): Boolean =
    ((that.dim == this.dim) && (List.range(0, that.dim).filter(i => that(i) != this(i)).length == 0))

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