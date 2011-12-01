package scala.virtualization.lms
package util

// hack to appease erasure

trait OverloadHack {
  class Overloaded1
  class Overloaded2
  class Overloaded3
  class Overloaded4
  class Overloaded5
  class Overloaded6
  class Overloaded7
  class Overloaded8
  class Overloaded9
  class Overloaded10

  implicit val overloaded1 = new Overloaded1
  implicit val overloaded2 = new Overloaded2
  implicit val overloaded3 = new Overloaded3
  implicit val overloaded4 = new Overloaded4
  implicit val overloaded5 = new Overloaded5
  implicit val overloaded6 = new Overloaded6
  implicit val overloaded7 = new Overloaded7
  implicit val overloaded8 = new Overloaded8
  implicit val overloaded9 = new Overloaded9
  implicit val overloaded10 = new Overloaded10
}