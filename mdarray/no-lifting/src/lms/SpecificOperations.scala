package lms
import lms.Conversions._
import lms.Operations._

object SpecificOperations {

  /** Reshape implementation */
  def internalReshape[A: ClassManifest](iv: MDArray[Int], a: Array[A], opName: String): MDArray[A] = {

    if (iv.dim != 1)
      throw new Exception(opName + ": The shape vector (" + iv + ") must be one-dimensional")

    if (prod(iv) != a.length)
      throw new Exception(opName + ": Incorrect shape vector (" + iv + ") for the array given (size=" + a.length + ")")

    new MDArray(iv, a)
  }

  /**
   * Simple, hardcoded comparison for SDArray[Int]
   * the programmer is responsible for checking 1-dimensionality of the arrays passed
   */
  def shapeEqual(a: MDArray[Int], b: MDArray[Int]): Boolean =
    (a.content.length == b.content.length) &&
    (List.range(0, a.content.length).filter(i => a.content()(i) != b.content()(i)).length == 0)

  /**
   * Prefix operation lt
   * the programmer is responsible for checking 1-dimensionality of the arrays passed
   */
  def prefixLt(iv: MDArray[Int], shape: MDArray[Int], opName: String): Boolean =
    (shape.content.length >= iv.content.length) &&
    ((shape zip (iv ::: zeros(shape.content.length - iv.content.length))).filter((p) => (p._1 <= p._2)).length == 0)

  /**
   * Prefix operation minus
   * the programmer is responsible for checking 1-dimensionality of the arrays passed
   */
  def prefixMinus(iv: MDArray[Int], shape: MDArray[Int], opName: String): MDArray[Int] =
    shape.toList.drop(iv.content.length).toArray

  /** Flatten */
  def flatten(shape: MDArray[Int], iv: MDArray[Int], opName: String) = {

    if ((shape.dim != 1) || (iv.dim != 1))
      throw new Exception(opName + ": The shape vector and/or the index vector are not 1-dimensional (shape=" + shape +
              " iv=" + iv + ")")      
    if ((shape.shape zip iv.shape).filter((p) => p._1 != p._2).length != 0)
      throw new Exception(opName + ": Internal error, trying to flatten iv: " + iv + " with shape: " + shape)
    (shape zip iv).foldLeft(0)((v, p) => v * p._1 + p._2)
  }

  /** where implementation */
  def internalWhere[A: ClassManifest](p: MDArray[Boolean], a: MDArray[A], b: MDArray[A], opName: String): MDArray[A] = {
    if (!shapeEqual(p.shape, a.shape) ||
        !shapeEqual(p.shape, b.shape))
      throw new Exception(opName + ": matrices of different shapes: p:" + p.shape + " a:" + a.shape + " b:" + b.shape)

    val result: Array[A] = new Array[A](a.content.length)
    for (i:Int <- List.range(0, a.content.length))
      result(i) = if (p.content()(i)) a.content()(i) else b.content()(i)
    new MDArray(a.shape, result)
  }

  /** Iterating through array sizes
   * the programmer is responsible for checking 1-dimensionality of the arrays passed
   */
  def iterateShape(maxBound: MDArray[Int], opName: String): Stream[MDArray[Int]] = {
    if (any((maxBound - 1) < 0))
      throw new Exception(opName + ": Incorrect size of iterating array: " + maxBound)
    iterate(zeros(maxBound.content.length), (maxBound - 1), opName)
  }

  /** Iterating through index vectors */
  def iterate(lb: MDArray[Int], ub: MDArray[Int], opName: String) : Stream[MDArray[Int]] = {

    def nextOp(crt: MDArray[Int]): Stream[MDArray[Int]] = {
      // XXX: Do not turn efficiency on until the lb deep copy is functional!
      val efficient = false
      var result: Array[Int] = null
      var response: MDArray[Int] = null
      var carry  = 1

      if (efficient)
        result = crt.content()
      else
        result = new Array[Int](crt.content.length)

//      // Check if lb is affected 
//      println("lb = " + lb)

      for (i <- List.range(crt.content.length-1, -1, -1))
        if (crt.content()(i) + carry > ub.content()(i))
          result(i) = lb.content()(i)
        else {
          result(i) = crt.content()(i) + carry
          carry = 0
        }

      if (efficient)
        response = crt
      else
        response = result

      if (carry == 1)
        Stream.empty
      else
        Stream.cons(response, nextOp(response))
    }

    // ops equivalent: any(lb > ub)
    if (lb.zip(ub).foldLeft(false)((a, p) => a || (p._1 > p._2))) {
      // DONE: Everyone must guard against empty streams
      // DONE: Decide if it is okay to throw an exception here... maybe it's better to just return an empty stream
      //throw new Exception(opName + ": Lower bound components are greater than their counterparts in ub: lb:" +
      //        lb + " ub:" + ub)
      Stream.empty
    } else {
      //TODO: Do the correct deep copying, so that lb is not affected by the optimization (efficient = true)
      //def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
      //  util.Marshal.load[A](util.Marshal.dump(a))
      //Stream.cons(new IndexVector(deepCopy(lb.content)), nextOp(lb))
      Stream.cons(lb.content, nextOp(lb))
    }
  }

  /** iteration with step and width */
  def iterateWithStep(_lb: MDArray[Int], lbStrict: Boolean, _ub: MDArray[Int], ubStrict: Boolean, step: MDArray[Int], width: MDArray[Int], opName: String) : Stream[MDArray[Int]] = {

    if (_lb.dim != 1)
      throw new Exception(opName + ": The lower bound vector (" + _lb + ") must be one-dimensional")
    if (_ub.dim != 1)
      throw new Exception(opName + ": The lower bound vector (" + _ub + ") must be one-dimensional")
    if (step.dim != 1)
      throw new Exception(opName + ": The step vector (" + step + ") must be one-dimensional")
    if (width.dim != 1)
      throw new Exception(opName + ": The width vector (" + width + ") must be one-dimensional")

    // ops equivalent: any(step - 1 > width)
    val useStep = (step.zip(width).foldLeft(false)((a, p) => a || ((p._1 - 1) > p._2)))
    val lb: MDArray[Int] = if (lbStrict) _lb.map(x => x + 1) else _lb
    val ub: MDArray[Int] = if (ubStrict) _ub.map(x => x - 1) else _ub

    // Correctness checks
    if ((lb.content.length != ub.content.length) ||
        (lb.content.length != step.content.length) ||
        (lb.content.length != width.content.length))
      throw new Exception(opName + ": Incorrect sizes for vectors: lb:" + lb + " ub:" + ub + " step:" + step + " width:" + width)

    val baseIterator: Stream[MDArray[Int]] = iterate(lb, ub, opName)
    if (!useStep)
      baseIterator
    else {
      def filterStepWidth(baseIterator: Stream[MDArray[Int]]): Stream[MDArray[Int]] = {
        var iterator = baseIterator
        while ((iterator != Stream.empty) && !(all(((iterator.head - _lb) rem step) <= width)))
          iterator = iterator.tail
        if (iterator == Stream.empty)
          Stream.empty
        else
          Stream.cons(iterator.head, filterStepWidth(iterator.tail))
      }
      filterStepWidth(baseIterator)
    }
  }
}