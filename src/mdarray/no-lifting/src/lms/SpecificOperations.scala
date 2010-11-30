package lms
import lms.Conversions._
import lms.Operations._

object SpecificOperations {

  /** Reshape implementation */
  def internalReshape[A: ClassManifest](iv: IndexVector, a: MDArray[A]): MDArray[A] = iv.content.length match {
    case 0 =>
      if (a.isInstanceOf[Scalar[A]])
        a
      else
        throw new Exception("reshape: Can only convert scalars to scalars")
    case x: Int =>
      if (a.content.length == prod(iv))
        if (x==1)
          new SDArray(a.content)
        else
          new MDArray(iv, a.content)
      else
        throw new Exception("reshape: Different sizes: iv:" + iv + " shape:" + a.shape)
  }

  /** Cat implementation */
  def internalCat[A](d: Int, one: MDArray[A], two: MDArray[A])(implicit ev: ClassManifest[A]): MDArray[A] = (one, two) match {
    case (a: SDArray[A], b: SDArray[A]) =>
      if (d != 0)
        throw new Exception("cat: can only concatenate two vectors on axis 0.")
      else
        new SDArray[A](one.content ++ two.content)

    case (a: MDArray[A], b: MDArray[A]) => {
      // TODO: Implement
      throw new Exception("cat: not implemnted")
    }
    
    case _ =>
      throw new Exception("cat: the two arrays cannot be concatenated because of their size difference: " + one.shape +
              " vs " + two.shape)
  }

  /** Iterating through index vectors */
  def iterate(lb: IndexVector, ub: IndexVector, lt: (Int, Int) => Boolean = (a, b) => a < b) : Stream[IndexVector] = {
    def nextOp(crt: IndexVector, lb: IndexVector, ub: IndexVector): Stream[IndexVector] = {
      val result = new Array[Int](crt.content.length)
      var carry  = 1

      for (i <- List.range(crt.content.length-1, -1, -1))
        if (!lt(crt(i) + carry, ub(i)))
          result(i) = lb(i)
        else {
          result(i) = crt(i) + carry
          carry = 0
        }

      val response = new IndexVector(result)
      if (carry == 1)
        Stream.empty
      else
        Stream.cons(response, nextOp(response, lb, ub))
    }

    Stream.cons(lb, nextOp(lb, lb, ub))
  }

  /** Simple, hardcoded comparison for SDArray[Int] */
  def shapeEqual(a: SDArray[Int], b: SDArray[Int]): Boolean =
    (a.content.length == b.content.length) &&
    (List.range(0, a.content.length).filter(i => a.content()(i) != b.content()(i)).length == 0)

  /** Prefix operation lt */
  def prefixLt(iv: SDArray[Int], shape: SDArray[Int]): Boolean =
    (shape.content.length >= iv.content.length) &&
    all(shape > (iv +++ zeros(shape.content.length - iv.content.length)))

  /** Prefix operation minus */
  def prefixMinus(iv: SDArray[Int], shape: SDArray[Int]): SDArray[Int] =
    new SDArray[Int](shape.toList.drop(iv.content.length).toArray)

  /** Flatten */
  def flatten(shape: SDArray[Int], iv: SDArray[Int]) = {
    if (shape.dim != iv.dim) throw new Exception("flatten: Internal error, trying to flatten iv: " + iv +
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
    new MDArray(this.shape, result)
  }

  /** internal With iterator */
  def internalWith(lb: IndexVector,
                   lb_strict: Boolean,
                   ub: IndexVector,
                   ub_strict: Boolean,
                   step: IndexVector,
                   width: IndexVector) : Stream[IndexVector] = {

    // Correctness checks
    if ((lb.content.length != ub.content.length) ||
        (lb.content.length != step.content.length) ||
        (lb.content.length != width.content.length))
      throw new Exception("with: Incorrect sizes for vectors: lb:" + lb + " ub:" + ub + " step:" + step + " width:" + width)
    if (!semanticLt(lb, ub))
      throw new Exception("with: Lower bound is greater than ub: lb:" + lb + " ub:" + ub + " step:" + step + " width:" + width)
    if (!semanticLt(width, step))
      throw new Exception("with: Width is greater than step: lb:" + lb + " ub:" + ub + " step:" + step + " width:" + width)

    // 1. Start iterating until the first value s.t. value%step = 0
    val zeroVector: IndexVector = zeros(lb.content.length)
    var crt: Stream[IndexVector] = iterate(lb, ub)
    if (ub_strict) crt = crt.tail // don't look at the first one
    while ((crt != Stream.empty) && (!all((crt.head remwz step) == zeroVector)))
      crt = crt.tail

    // 2. Start the actual execution
    if (crt == Stream.empty)
      Stream.empty
    else
      nextStep(crt.head, ub, ub_strict, step, width)
  }

  private def nextStep(crt: IndexVector,
             ub: IndexVector,
             ub_strict: Boolean,
             step: IndexVector,
             width: IndexVector): Stream[IndexVector] = {

    var continue: Stream[IndexVector] = Stream.empty

    if ((ub_strict && all((crt + step) < ub)) ||
        (!ub_strict && all((crt + step) <= ub)))
      continue = nextStep((crt + step).asInstanceOf[IndexVector], ub, ub_strict, step, width)

    var current: Stream[IndexVector] =  iterate(crt, (crt + width).asInstanceOf[IndexVector])

    if (ub_strict) {
      if (!all((crt + width) < ub))
          current = iterate(crt, ub)
    } else {
      if (!all((crt + width) <= ub))
          current = iterate(crt, ub, (a, b) => a <=b)
    }

    current ++ continue
  }

  /** Semantic greater than */
  def semanticLt(lb: SDArray[Int], ub: SDArray[Int], res: Boolean = false): Boolean = {
    for (i <- List.range(lb.content.length-1, -1, -1))
      if (lb.content()(i) < ub.content()(i))
        return true // it's easier than doing a 3-state :)            
      else if (lb.content()(i) > ub.content()(i))
        return false
    return false
  }
}