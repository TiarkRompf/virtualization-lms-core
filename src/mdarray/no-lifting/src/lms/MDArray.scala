package lms

import scala.collection.immutable._
import java.lang.StringBuffer

/*

class MDArray[A](shape: Array[Rep[Int]], content: Rep[Array[T]]) {
  
  def dim = shape.length

  def apply(iv: MDArray[Int]): MDArray[A] = {
    assert(iv.dim == 1)
    ...
  }


}

  0 0 0 0 0 
  0 0 0 0 0

  Apply(1)
  
  0 0 0 0 0




*/


class MDArray[A](shapeIV:IndexVector, contentArray:Array[A]) {

  // Built-in functions
  def dim = shape.dim
  def shape = shapeIV
  def content = contentArray

  def apply(iv: IndexVector): A =
    contentArray(shape flatten iv)

  def update(iv: IndexVector, a: A): A = {
    val index:Int = shape flatten iv
    contentArray(index) = a
    a
  }

  override def toString: String = {
    val sb: StringBuffer = new StringBuffer()
    sb.append("Array of dim ")
    sb.append(dim.toString)
    sb.append(" with shape vector ")
    sb.append(shape.toString)
    sb.append(":\n")

    if (dim==1) {
      for (a <- contentArray)
        sb.append("[" + a.toString + "]  ")
      sb.append("\n")
    } else
      for (i <- List.range(0, shape.shape(0))) {
        for (j <- List.range(0, shape.shape(1))) {
          sb.append("[")
          val bp = i * (shape.contentSize/shape.shape(0)) + j * (shape.contentSize/shape.shape(0)/shape.shape(1)) 
          for (k <- List.range(0, (shape.contentSize / shape.shape(0) / shape.shape(1)))) {
            if (k!=0) sb.append(" ")
            sb.append(contentArray(bp + k).toString)
          }
          sb.append("]  ")
        }
        sb.append("\n")
      }

    
    sb.toString()
  }

  /*
    Common array operations go here ... :)
   */
}


/**
 * MDArray companion object for the operations
 */
object MDArray {

  def dim[A](a: MDArray[A]): Int = a.dim
  def shape[A](a: MDArray[A]): IndexVector = a.shape

  private def createArray[A: ClassManifest](iv: IndexVector): MDArray[A] =
    new MDArray(iv, new Array[A](iv.contentSize)) 

  def reshape[A](a: MDArray[A], newShape:IndexVector): MDArray[A] = {
    if (newShape.contentSize != a.shape.contentSize)
      throw new Exception("Incorrect size in reshape.")

    // since the arrays are immutable, we can share content between them
    new MDArray[A](newShape, a.content)
  }


  def genArray[A: ClassManifest](a: MDArray[A], iv: IndexVector): MDArray[A] = {

    val newSize = iv + a.shape
    val newArray = createArray[A](newSize)

    for (i <- iv.iterate)
      for (j <- a.shape.iterate)
        newArray(i+j) = a(j)

    newArray
  }


  def sel[A: ClassManifest](a: MDArray[A], iv: IndexVector): MDArray[A] = {

    val newSize = a.shape - iv
    val newArray = createArray[A](newSize)

    for (i <- newSize.iterate)
      newArray(i) = a(iv + i)

    newArray
  }


  def modarray[A: ClassManifest](a: MDArray[A], iv: IndexVector, value: MDArray[A]): MDArray[A] = {

    if (!((a.shape - iv) isElementWiseEqualTo  value.shape))
      throw new Exception("modarray: Array sizes do not match.")

    val newArray = createArray[A](a.shape)

    for (i <- a.shape.iterate)
      newArray(i) = a(i)

    for (i <- value.shape.iterate)
      newArray(iv + i) = value(i)

    newArray
  }


  def take[A: ClassManifest](a: MDArray[A], iv: IndexVector): MDArray[A] = {

    if (a.shape isElementWiseGreaterOrEqualTo iv)
      throw new Exception("take: Take vector does not match given matrix.")

    val newArray = createArray[A](iv)

    for (i <- iv.iterate)
      newArray(i) = a(i)

    newArray
  }


  def drop[A: ClassManifest](a: MDArray[A], iv: IndexVector): MDArray[A] = {

    if (!(a.shape isElementWiseGreaterThen iv))
      throw new Exception("drop: Unable to drop more than the actual size of the matrix.")

    val newSize = a.shape elementWiseDifference iv
    val newArray = createArray[A](newSize)

    for (i <- newSize.iterate)
      newArray(i) = a(i elementWiseAdd iv)

    newArray
  }


  def cat[A: ClassManifest](d: Int, a: MDArray[A], b: MDArray[A]): MDArray[A] = {

//    try {
      val newSize = a.shape.axisConcatenation(b.shape, d)
      val newArray = createArray[A](newSize)
      val offset = newSize elementWiseDifference b.shape

      for (i <- a.shape.iterate)
        newArray(i) = a(i)

      for (i <- b.shape.iterate)
        newArray(i  elementWiseAdd offset) = b(i)

      newArray
//    } catch {
//      case _ => throw new Exception("cat: Incorrect shapes in "+d+"-concatenation.")
//    }
  }
}