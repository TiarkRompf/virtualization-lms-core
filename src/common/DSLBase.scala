package scala.virtualization.lms
package common

trait DSLBase extends BaseExp with UncheckedOps {
  // keep track of top level functions
  abstract class TopLevel(n: String) {
    val name = n;
  }
  case class TopLevel1 [A, B](n: String, mA: Manifest[A], mB: Manifest[B], f: Rep[A] => Rep[B]) extends TopLevel(n)
  case class TopLevel2 [A1, A2, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B], f: (Rep[A1], Rep[A2]) => Rep[B]) extends TopLevel(n)
  case class TopLevel3 [A1, A2, A3, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3]) => Rep[B]) extends TopLevel(n)
  case class TopLevel4 [A1, A2, A3, A4, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4]) => Rep[B]) extends TopLevel(n)
  case class TopLevel5 [A1, A2, A3, A4, A5, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5]) => Rep[B]) extends TopLevel(n)
  case class TopLevel6 [A1, A2, A3, A4, A5, A6, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6]) => Rep[B]) extends TopLevel(n)
  case class TopLevel7 [A1, A2, A3, A4, A5, A6, A7, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7]) => Rep[B]) extends TopLevel(n)
  case class TopLevel8 [A1, A2, A3, A4, A5, A6, A7, A8, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8]) => Rep[B]) extends TopLevel(n)
  case class TopLevel9 [A1, A2, A3, A4, A5, A6, A7, A8, A9, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mA9: Manifest[A9], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9]) => Rep[B]) extends TopLevel(n)
  case class TopLevel10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mA9: Manifest[A9], mA10: Manifest[A10], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10]) => Rep[B]) extends TopLevel(n)
  case class TopLevel11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mA9: Manifest[A9], mA10: Manifest[A10], mA11: Manifest[A11], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11]) => Rep[B]) extends TopLevel(n)
  case class TopLevel12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mA9: Manifest[A9], mA10: Manifest[A10], mA11: Manifest[A11], mA12: Manifest[A12], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12]) => Rep[B]) extends TopLevel(n)
  case class TopLevel13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mA9: Manifest[A9], mA10: Manifest[A10], mA11: Manifest[A11], mA12: Manifest[A12], mA13: Manifest[A13], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13]) => Rep[B]) extends TopLevel(n)
  case class TopLevel14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mA9: Manifest[A9], mA10: Manifest[A10], mA11: Manifest[A11], mA12: Manifest[A12], mA13: Manifest[A13], mA14: Manifest[A14], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13], Rep[A14]) => Rep[B]) extends TopLevel(n)
  case class TopLevel15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](n: String, mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mA6: Manifest[A6], mA7: Manifest[A7], mA8: Manifest[A8], mA9: Manifest[A9], mA10: Manifest[A10], mA11: Manifest[A11], mA12: Manifest[A12], mA13: Manifest[A13], mA14: Manifest[A14], mA15: Manifest[A15], mB: Manifest[B], f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13], Rep[A14], Rep[A15]) => Rep[B]) extends TopLevel(n)

  val rec = new scala.collection.mutable.HashMap[String, TopLevel]
  def toplevel1[A: Manifest, B: Manifest](name: String)(f: Rep[A] => Rep[B]): Rep[A] => Rep[B] = {
    val g = (x: Rep[A]) => unchecked[B](name, "(", x, ")")
    rec.getOrElseUpdate(name, TopLevel1(name, manifest[A], manifest[B], f))
    g
  }
  def toplevel2[A1: Manifest, A2: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2]) => Rep[B]): (Rep[A1], Rep[A2]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2]) => unchecked[B](name, "(", x1, ",", x2, ")")
    rec.getOrElseUpdate(name, TopLevel2(name, manifest[A1], manifest[A2], manifest[B], f))
    g
  }
  def toplevel3[A1: Manifest, A2: Manifest, A3: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ")")
    rec.getOrElseUpdate(name, TopLevel3(name, manifest[A1], manifest[A2], manifest[A3], manifest[B], f))
    g
  }
  def toplevel4[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ")")
    rec.getOrElseUpdate(name, TopLevel4(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[B], f))
    g
  }
  def toplevel5[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ")")
    rec.getOrElseUpdate(name, TopLevel5(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[B], f))
    g
  }
  def toplevel6[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ")")
    rec.getOrElseUpdate(name, TopLevel6(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[B], f))
    g
  }
  def toplevel7[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ")")
    rec.getOrElseUpdate(name, TopLevel7(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[B], f))
    g
  }
  def toplevel8[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ")")
    rec.getOrElseUpdate(name, TopLevel8(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[B], f))
    g
  }
  def toplevel9[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8], x9: Rep[A9]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ",", x9, ")")
    rec.getOrElseUpdate(name, TopLevel9(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[A9], manifest[B], f))
    g
  }
  def toplevel10[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8], x9: Rep[A9], x10: Rep[A10]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ",", x9, ",", x10, ")")
    rec.getOrElseUpdate(name, TopLevel10(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[A9], manifest[A10], manifest[B], f))
    g
  }
  def toplevel11[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8], x9: Rep[A9], x10: Rep[A10], x11: Rep[A11]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ",", x9, ",", x10, ",", x11, ")")
    rec.getOrElseUpdate(name, TopLevel11(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[A9], manifest[A10], manifest[A11], manifest[B], f))
    g
  }
  def toplevel12[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8], x9: Rep[A9], x10: Rep[A10], x11: Rep[A11], x12: Rep[A12]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ",", x9, ",", x10, ",", x11, ",", x12, ")")
    rec.getOrElseUpdate(name, TopLevel12(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[A9], manifest[A10], manifest[A11], manifest[A12], manifest[B], f))
    g
  }
  def toplevel13[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, A13: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8], x9: Rep[A9], x10: Rep[A10], x11: Rep[A11], x12: Rep[A12], x13: Rep[A13]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ",", x9, ",", x10, ",", x11, ",", x12, ",", x13, ")")
    rec.getOrElseUpdate(name, TopLevel13(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[A9], manifest[A10], manifest[A11], manifest[A12], manifest[A13], manifest[B], f))
    g
  }
  def toplevel14[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, A13: Manifest, A14: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13], Rep[A14]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13], Rep[A14]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8], x9: Rep[A9], x10: Rep[A10], x11: Rep[A11], x12: Rep[A12], x13: Rep[A13], x14: Rep[A14]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ",", x9, ",", x10, ",", x11, ",", x12, ",", x13, ",", x14, ")")
    rec.getOrElseUpdate(name, TopLevel14(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[A9], manifest[A10], manifest[A11], manifest[A12], manifest[A13], manifest[A14], manifest[B], f))
    g
  }
  def toplevel15[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, A13: Manifest, A14: Manifest, A15: Manifest, B: Manifest](name: String)(f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13], Rep[A14], Rep[A15]) => Rep[B]): (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13], Rep[A14], Rep[A15]) => Rep[B] = {
    val g = (x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5], x6: Rep[A6], x7: Rep[A7], x8: Rep[A8], x9: Rep[A9], x10: Rep[A10], x11: Rep[A11], x12: Rep[A12], x13: Rep[A13], x14: Rep[A14], x15: Rep[A15]) => unchecked[B](name, "(", x1, ",", x2, ",", x3, ",", x4, ",", x5, ",", x6, ",", x7, ",", x8, ",", x9, ",", x10, ",", x11, ",", x12, ",", x13, ",", x14, ",", x15, ")")
    rec.getOrElseUpdate(name, TopLevel15(name, manifest[A1], manifest[A2], manifest[A3], manifest[A4], manifest[A5], manifest[A6], manifest[A7], manifest[A8], manifest[A9], manifest[A10], manifest[A11], manifest[A12], manifest[A13], manifest[A14], manifest[A15], manifest[B], f))
    g
  }


}