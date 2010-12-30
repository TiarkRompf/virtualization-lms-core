package lms
import lms.Conversions._
import lms.With._
import lms.Operations._
import lms.SpecificOperations._
import util.Random

object TestObject{

  def main(args: Array[String]): Unit = {
    val a = reshape(2::2::2::Nil, 11::12::13::14::15::16::17::18::Nil)
    val b = reshape(2::2::2::Nil, 22::24::26::28::30::32::34::36::Nil)
    val c = a + b
    println(c)
    println(a(1::1::Nil))

    val it = iterateWithStep(3::3::3::Nil, false, 3::3::3::Nil, false, 1::2::2::Nil, 0::1::1::Nil, opName="test")
    for (i <- it)
      println(i)

    val d = With(0::0::0::0::Nil, true, 9::9::9::9::Nil, true).GenArray(10::10::10::10::Nil, (iv: IndexVector) => sum(iv))
    println(d)

    val e = With(4::Nil, false, 6::Nil, false).ModArray(d, (iv: IndexVector) => d(iv) + 1)
    println(e)

    val small: MDArray[Int] = 1::1::1::1::1::Nil
    val large: MDArray[Int] = 1::1::1::1::1::1::1::1::1::1::Nil
    val offset: IndexVector = 3::Nil

    println(With(offset, false, offset + shape(small), true).
            ModArray(large, iv => large(iv) + small(iv - offset)))

    val a1 = reshape(2::2::2::Nil, 11::12::13::14::15::16::17::18::Nil)
    val b1 = reshape(2::2::1::Nil, 22::24::26::28::Nil)

    print(cat(2, a1, b1))
  }
}