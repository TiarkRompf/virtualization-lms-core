package lms
import lms.Conversions._

class Scalar[A: ClassManifest](_value: A) extends MDArray[A](null, Array(_value)) {
  
  override def dim(): Int = 0
  override def shape(): SDArray[Int] = new SDArray[Int](new Array(0))
  override def sel(iv: SDArray[Int]) : Scalar[A] = {
    if (iv.content.length != 0)
      throw new Exception("Scalar.sel("+iv+") is impossible")
    else
      return this
  }
  override def apply(iv: SDArray[Int]) = sel(iv)
  def value(): A = _value

  override def toString(): String = "Scalar: " + _value  
}

object Scalar {

}