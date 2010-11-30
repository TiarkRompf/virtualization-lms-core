package lms
import lms.Conversions._

object TestObject{

  def main(args: Array[String]): Unit = {
    val a = new MDArray[Int](List(1,2), Array(1,2))
    val b = new MDArray[Int](List(1,2), Array(2,4))
    val c = a + b
    println(c)
    println(a(List(0)))
  }

}