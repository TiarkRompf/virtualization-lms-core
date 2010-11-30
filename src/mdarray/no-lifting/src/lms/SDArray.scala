package lms
import lms.Conversions._

class SDArray[A: ClassManifest](_content: Array[A]) extends MDArray[A](null, _content){

  override def dim(): Int = 1
  override def shape(): SDArray[Int] = new SDArray[Int](Array(_content.length))
  override def content() = _content
  override def sel(iv: SDArray[Int]) : Scalar[A] = {
    if ((iv.dim != 1) || (_content.length >= iv(List(0))))
      throw new Exception("SDArray.sel("+iv+") is impossible")
    else
      sel(0)
  }
  override def apply(iv: SDArray[Int]) = sel(iv)

  // simpler versions of the overrides
  def sel(index: Int) : A = _content(index)
  def apply(index: Int) = sel(index)

  override def toString(): String =
    "Vector(" + _content.length + "):" + _content.foldLeft("")((b: String, a: A) => b + " " + a.toString())
}

object SDArray {
}
