package lms
import lms.Conversions._
import lms.Operations._
import lms.SpecificOperations._
import lms.MDArray._

class MDArray[A: ClassManifest](_shape: SDArray[Int], _content: Array[A]) {

  def dim(): Int = _shape.content.length
  def shape(): SDArray[Int] = _shape
  def content(): Array[A] = _content
  def sel(iv: SDArray[Int]): MDArray[A] = {
    // Check the selection size
    if (!prefixLt(iv, shape))
      throw new Exception("MDArray.sel("+iv+") the index vector components are greater or equal to the shape: " + shape)

    // Compute the new array and iterating
    val suffix: IndexVector = prefixMinus(iv, this.shape)
    val arraySize: Int = prod(suffix)
    val array: Array[A] = new Array[A](arraySize)

    // Compute values in the new array
    for (i <- iterate(zeros(suffix.content.length), suffix))
      array(flatten(suffix, i)) = this.content()(flatten(shape, (iv +++ i).asInstanceOf[SDArray[Int]]))

    suffix.content.length match {
      case 0 =>
        new Scalar[A](array(0))
      case 1 =>
        new SDArray(array)
      case _ =>
        new MDArray(suffix, array)
    }
  }
  def apply(iv: SDArray[Int]) = sel(iv)

  // Element-wise operations
  def +(that: MDArray[A])(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.plus, "+")
  def -(that: MDArray[A])(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.minus, "-")
  def *(that: MDArray[A])(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.times, "*")
  def -()(implicit numeric: Numeric[A]): MDArray[A] = uop(numeric.negate, "-[unary]")
  def /(that: MDArray[A])(implicit fractional: Fractional[A]): MDArray[A] = op(that)(fractional.div, "/[div]")
  def div(that: MDArray[A])(implicit integral: Integral[A]): MDArray[A] = op(that)(integral.quot, "/[quot]")
  def rem(that: MDArray[A])(implicit integral: Integral[A]): MDArray[A] = op(that)(integral.rem, "%[rem]")
  def <(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.lt, "<")
  def <=(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.lteq, "<=")
  def >(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.gt, ">")
  def >=(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.gteq, ">=")
  def ==(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.equiv, "==")
  def !=(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(
                                                                                  (a, b) => !ordering.equiv(a, b), "!=")
  def &&(that: MDArray[A])(implicit ev: A =:= Boolean): MDArray[Boolean] = op(that)((a, b) => ev(a) && ev(b), "&&")
  def ||(that: MDArray[A])(implicit ev: A =:= Boolean): MDArray[Boolean] = op(that)((a, b) => ev(a) || ev(b), "||")
  def ! ()(implicit ev: A =:= Boolean): MDArray[Boolean] = uop((a) => !(ev(a)), "!")

  // Special purpose definitions
  /** Greater than definition for min and max */
  def >(that: MDArray[A], opName: String)(implicit ordering: Ordering[A]): MDArray[Boolean] =
    op(that)(ordering.gt, opName)
  /** Reminder with zero */
  def remwz(that: MDArray[A])(implicit integral: Integral[A]): MDArray[A] = op(that)(
    (a, b) => if (b == integral.zero) integral.zero else integral.rem(a, b), "%[remwz]")

  // Reducing all array elements, in order, left or right
  def reduceLeft[B](z: B)(f: (B, A)=>B): B = content.foldLeft(z)(f)
  def reduceRight[B](z: B)(f: (A, B)=>B): B = content.foldRight(z)(f)

  // Concatenation shortcut (note: it's three pluses instead of two)
  def +++(that: MDArray[A]): MDArray[A] = cat(0, this, that)


  /** Binary element-wise operation */
  private def op[B: ClassManifest](that:MDArray[A])(op: (A, A) => B, opName: String): MDArray[B] = {
    if (!shapeEqual(this.shape, that.shape))
      throw new Exception(opName + ": matrices of different shapes: " + this.shape + " vs " + that.shape)

    val result: Array[B] = new Array[B](this.content.length)
    for (i:Int <- List.range(0, this.content.length))
      result(i)= op(this.content()(i), that.content()(i))
    new MDArray(this.shape, result)
  }

  /** Unary element-wise operation */
  private def uop[B: ClassManifest](op: A => B, opName: String): MDArray[B] = {
    val result: Array[B] = new Array[B](this.content.length)
    for (i:Int <- List.range(0, this.content.length))
      result(i)= op(this.content()(i))
    new MDArray(this.shape, result)
  }

  override def toString(): String = {
    val sb: StringBuffer = new StringBuffer()
    sb.append("Array(")
    sb.append(shape.toString)
    sb.append("):\n")

    for (i <- List.range(0, _shape(0))) {
      for (j <- List.range(0, _shape(1))) {
        sb.append("<")
        val bp = i * (_content.length / _shape(0)) + j * (_content.length / _shape(0) / _shape(1))
        for (k <- List.range(0, (_content.length / _shape(0) / _shape(1)))) {
          if (k!=0) sb.append(" ")
          sb.append(_content(bp + k).toString)
        }
        sb.append(">  ")
      }
      sb.append("\n")
    }

    sb.toString()
  }
}

object MDArray{
}
