package lms
import lms.Conversions._
import lms.Operations._

object SpecificOperations {

  /** Reshape implementation */
  def internalReshape[A: ClassManifest](iv: IndexVector, a: Array[A], opName: String): MDArray[A] = iv.content.length match {
    case 0 =>
      if (a.length == 1)
        new Scalar(a(0))
      else
        throw new Exception(opName + ": Can only convert elements of size one to scalars (given size: " + a.length + ")")
    case x: Int =>
      if (a.length == prod(iv))
        if (x==1)
          new SDArray(a)
        else
          new MDArray(iv, a)
      else
        throw new Exception(opName + ": Different sizes: iv:" + iv + " size of array:" + a.length)
  }

  /** Simple, hardcoded comparison for SDArray[Int] */
  def shapeEqual(a: SDArray[Int], b: SDArray[Int]): Boolean =
    (a.content.length == b.content.length) &&
    (List.range(0, a.content.length).filter(i => a.content()(i) != b.content()(i)).length == 0)

  /** Prefix operation lt */
  def prefixLt(iv: SDArray[Int], shape: SDArray[Int], opName: String): Boolean =
    (shape.content.length >= iv.content.length) &&
    all(shape > (iv ::: zeros(shape.content.length - iv.content.length)))

  /** Prefix operation minus */
  def prefixMinus(iv: SDArray[Int], shape: SDArray[Int], opName: String): SDArray[Int] =
    new SDArray[Int](shape.toList.drop(iv.content.length).toArray)

  /** Flatten */
  def flatten(shape: SDArray[Int], iv: SDArray[Int], opName: String) = {
    if (shape.dim != iv.dim) throw new Exception(opName + ": Internal error, trying to flatten iv: " + iv +
            " with shape: " + shape)
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

  /** Iterating through array sizes */
  def iterateShape(maxBound: IndexVector, opName: String): Stream[IndexVector] = {
    if (any((maxBound - 1) < 0))
      throw new Exception(opName + ": Incorrect size of iterating array: " + maxBound)
    iterate(zeros(maxBound.content.length), (maxBound - 1), opName)
  }

  /** Iterating through index vectors */
  def iterate(lb: IndexVector, ub: IndexVector, opName: String) : Stream[IndexVector] = {

    def nextOp(crt: IndexVector): Stream[IndexVector] = {
      // XXX: Do not turn efficiency on until the lb deep copy is functional!
      val efficient = false
      var result: Array[Int] = null
      var response: IndexVector = null
      var carry  = 1

      if (efficient)
        result = crt.content()
      else
        result = new Array[Int](crt.content.length)

//      // Check if lb is affected 
//      println("lb = " + lb)

      for (i <- List.range(crt.content.length-1, -1, -1))
        if (crt(i) + carry > ub(i))
          result(i) = lb(i)
        else {
          result(i) = crt(i) + carry
          carry = 0
        }

      if (efficient)
        response = crt
      else
        response = new IndexVector(result)
      
      if (carry == 1)
        Stream.empty
      else
        Stream.cons(response, nextOp(response))
    }

    if (any(lb > ub))
      // TODO: Everyone must guard against empty streams
      // DONE: Decide if it is okay to throw an exception here... maybe it's better to just return an empty stream
      //throw new Exception(opName + ": Lower bound components are greater than their counterparts in ub: lb:" +
      //        lb + " ub:" + ub)
      Stream.empty
    else {
//      TODO: Do the correct deep copying, so that lb is not affected by the optimization (efficient = true)
//      def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
//        util.Marshal.load[A](util.Marshal.dump(a))
//      Stream.cons(new IndexVector(deepCopy(lb.content)), nextOp(lb))
      Stream.cons(new IndexVector(lb.content), nextOp(lb))
    }
  }

  /** iteration with step and width */
  def iterateWithStep(_lb: IndexVector, lbStrict: Boolean, _ub: IndexVector, ubStrict: Boolean, step: IndexVector, width: IndexVector, opName: String) : Stream[IndexVector] = {

    val useStep = any((step - 1) > width)
    val lb: IndexVector = if (lbStrict) _lb + 1 else _lb
    val ub: IndexVector = if (ubStrict) _ub - 1 else _ub

    // Correctness checks
    if ((lb.content.length != ub.content.length) ||
        (lb.content.length != step.content.length) ||
        (lb.content.length != width.content.length))
      throw new Exception(opName + ": Incorrect sizes for vectors: lb:" + lb + " ub:" + ub + " step:" + step + " width:" + width)

    val baseIterator: Stream[IndexVector] = iterate(lb, ub, opName)
    if (!useStep)
      baseIterator
    else {
      def filterStepWidth(baseIterator: Stream[IndexVector]): Stream[IndexVector] = {
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