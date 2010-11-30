package lms
import lms.Conversions._
import lms.SpecificOperations._

object Operations {
  type IndexVector = SDArray[Int]
  
  // Element-Wise operations on matrices
  def max[A: Ordering : ClassManifest](a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(a.>(b, "max"), a, b, "max")
  def min[A: Ordering : ClassManifest](a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(a.>(b, "min"), b, a, "min")
  def where[A: ClassManifest](p: MDArray[Boolean], a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(p, a, b, "where")
  // note: the rest are defined in the MDArray class!

  // Basic operations
  def dim[A: ClassManifest](a: MDArray[A]): Int = a.dim
  def shape[A: ClassManifest](a: MDArray[A]): SDArray[Int] = a.shape
  def sel[A: ClassManifest](iv: IndexVector, a: MDArray[A]): MDArray[A] = a.sel(iv)
  def reshape[A: ClassManifest](iv: IndexVector, a: MDArray[A]): MDArray[A] = internalReshape(iv, a)
  def cat[A](d: Int, one: MDArray[A], two: MDArray[A])(implicit ev: ClassManifest[A]): MDArray[A] = internalCat(d, one, two)

  // Reduction operations on matrices
  def sum[A](a: MDArray[A])(implicit ev: Numeric[A]): A = a.reduceLeft(ev.zero)((a: A, b: A) => ev.plus(a, b))
  def prod[A](a: MDArray[A])(implicit ev: Numeric[A]): A = a.reduceLeft(ev.one)((a: A, b: A) => ev.times(a, b))
  def all(a: MDArray[Boolean]): Boolean = a.reduceLeft(true)((a, b) => a && b)
  def any(a: MDArray[Boolean]): Boolean = a.reduceLeft(false)((a, b) => a || b)
  def maxVal[A](a: MDArray[A])(implicit ev: Ordering[A]): A = a.reduceLeft(a.content()(0))((a, b) => if (ev.gt(a, b)) a else b)
  def minVal[A](a: MDArray[A])(implicit ev: Ordering[A]): A = a.reduceLeft(a.content()(0))((a, b) => if (ev.lt(a, b)) a else b)

  // Some additional functions
  def zeros[A: ClassManifest](size: Int): SDArray[Int] = new SDArray[Int](new Array(size))

  // With iterator
  def mdWith(lb: IndexVector,
             lb_strict: Boolean,
             ub: IndexVector,
             ub_strict: Boolean,
             step: IndexVector,
             width: IndexVector): Stream[IndexVector] = internalWith(lb, lb_strict, ub, ub_strict, step, width)
}