package scala.virtualization.lms
package common

/**
 * Generate a "zero" value for a given type.
 *
 * The apply method of this object can be used to generate a constant
 * "zero" value for a given type `T` like so:
 *
 * {{{
 *   val zero = ZeroVal[T]
 * }}}
 *
 * The return value can be used e.g. to initialize variables.  This is
 * useful when one needs to generate constants symbols in a generic
 * way, e.g. when initializing variables or fields of structs.  The
 * special treatment of value types ensures the correct behavior
 * e.g. when generating a string literal from a constant symbol.
 */
object ZeroVal {

  val BooleanC = classOf[Boolean]
  val ByteC = classOf[Byte]
  val CharC = classOf[Char]
  val IntC = classOf[Int]
  val LongC = classOf[Long]
  val ShortC = classOf[Short]
  val DoubleC = classOf[Double]
  val FloatC = classOf[Float]
  val UnitC = classOf[Unit]

  def apply[A: Manifest]: A = {
    val z: Any = implicitly[Manifest[A]].runtimeClass match {
      case ByteC    => 0.toByte
      case CharC    => 0.toChar
      case IntC     => 0
      case LongC    => 0L
      case ShortC   => 0.toShort
      case DoubleC  => 0.0
      case FloatC   => (0.0).toFloat
      case BooleanC => false
      case UnitC    => ()
      case _        => null
    }
    z.asInstanceOf[A]
  }
}
