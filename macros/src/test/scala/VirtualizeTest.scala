package org.scala_lang.virtualized
package annotation

import org.scala_lang.virtualized.SourceContext
import org.scalatest.{ FlatSpec, ShouldMatchers }


class VirtualizeSpec extends FlatSpec with ShouldMatchers with EmbeddedControls {

  def __ifThenElse[T](cs: Seq[Boolean], tb: => T, eb: => T): T = {
    if (cs forall (_ == true)) tb else eb
  }

  def infix_+[T](x1: List[T], x2: Any): String = {
    x1.toString+"+"+x2.toString
  }

  def infix_==[T](x1: List[T], x2: List[T]): Boolean = {
    (x1 zip x2) forall (p => p._1 == p._2)
  }

  "virtualizeSourceContext" should "be virtualized" in {
    implicit class OpsCls(lhs: Boolean){
      def op(rhs: Boolean)(implicit pos: SourceContext) = pos.toString + " " + pos.methodName + " " + pos.assignedVariable
    }

    def virtualizeContext() = {
      val foo = true op false
      foo
    }

    //Careful, these tests depend on the line numbers they are written on!!
    virtualizeContext() should be("VirtualizeTest.scala:28:22 op Some(foo)")
  }

  "virtualizeSourceContextNested" should "be virtualized" in {

    def a()(implicit pos: SourceContext) = b()
    def b()(implicit pos: SourceContext) = c()
    def c()(implicit pos: SourceContext) = pos.toString + " " + pos.methodName + " " + pos.assignedVariable

    //SourceContext macro should only be applied at the highest level
    //Afterwards the implicit parameter should be passed down the forwarding calls
    def virtualizeContext() = a()
    virtualizeContext() should be("VirtualizeTest.scala:44:32 a None")
  }
  implicit class OpsCls(lhs: String) {
    def ++++(rhs: String)(implicit pos: SourceContext) = lhs + " " + pos.toString + " " + pos.methodName + " " + pos.assignedVariable + " " + rhs
    def ----(rhs: String)(implicit pos: SourceContext) = lhs + " " + pos.toString + " " + pos.methodName + " " + pos.assignedVariable + " " + rhs
    def ?(implicit pos: SourceContext) = pos.toString + " " + pos.methodName + " " + pos.assignedVariable
    def infix(implicit pos: SourceContext): String = pos.toString + " " + pos.methodName + " " + pos.assignedVariable
    def unary_!(implicit pos: SourceContext): String = pos.toString + " " + pos.methodName + " " + pos.assignedVariable
    def method(x: String, y: String)(implicit pos: SourceContext) = pos.toString + " " + pos.methodName + " " + pos.assignedVariable
  }

  "virtualizeSourceContextSequential" should "be virtualized" in {
    val x = "X" ++++ "Y" ++++ "Z"
    x should be("X VirtualizeTest.scala:57:17 ++++ Some(x) Y VirtualizeTest.scala:57:26 ++++ Some(x) Z")
  }

  "virtualizeSourceContextMultiDef" should "be virtualized" in {
    val x = "X" ++++ "Y"; val y = "X" ++++ "Y"

    x should be("X VirtualizeTest.scala:62:17 ++++ Some(x) Y")
    y should be("X VirtualizeTest.scala:62:39 ++++ Some(y) Y")
  }

  // TODO: Expected behavior in this case? Should we try to backtrack to the most recent val/var?
  "virtualizeSourceContextMultiLine" should "be virtualized" in {
    val x =
            "X" ++++ "Z"
    x should be("X VirtualizeTest.scala:71:17 ++++ Some(x) Z") // TODO
  }

  "virtualizeSourceContextOthers" should "be virtualized" in {
    var q = "HELLO".infix
    var z = "HELLO".method("WORLD", "!")
    var x = !q
    var m = !x ++++ !z
    var w = "X"++++"Y"
    var y = "32"?

    q should be ("VirtualizeTest.scala:76:21 infix Some(q)")    // Method calls without parentheses - col is at start of term
    z should be ("VirtualizeTest.scala:77:27 method Some(z)")   // Method calls with parentheses - col is at opening paren
    x should be ("VirtualizeTest.scala:78:13 unary_! Some(x)")
    m should be ("VirtualizeTest.scala:79:13 unary_! Some(m) VirtualizeTest.scala:79:16 ++++ Some(m) VirtualizeTest.scala:79:21 unary_! Some(m)")
    w should be ("X VirtualizeTest.scala:80:16 ++++ Some(w) Y")
    y should be ("VirtualizeTest.scala:81:17 ? Some(y)")
  }

  "virtualizeSourceContextMultiOp" should "be virtualized" in {
    val x = "X"----"Y"++++"Z"----"Q"

    x should be ("X VirtualizeTest.scala:92:16 ---- Some(x) Y VirtualizeTest.scala:92:23 ++++ Some(x) Z VirtualizeTest.scala:92:30 ---- Some(x) Q")
  }

  "StagedStringConcat" should "be virtualized" in {
    case class Sym[T](value: T)
    
    def infix_+(x1: String, x2: Any): Sym[String] = x2 match {
      case x2:Sym[_] => Sym(x1 + " " + x2.value)
      case _ => Sym(x1 + x2.toString)
    }
    
    @virtualize
    def virtualizeConcatTest() = "hello" + Sym("world")
    
    virtualizeConcatTest() should be(Sym("hello world"))
  }


  "StagedImplicitStringConcat" should "be virtualized" in {
    case class Sym[T](value: T)
    
    def infix_+[T:Numeric](x1: String, x2: Sym[T]): Sym[String] = {
      Sym(x1 + " has value: " + x2.value.toString)
    }
    @virtualize
    def virtualizeNumericConcatTest() = "this" + Sym(32)

    virtualizeNumericConcatTest() should be (Sym("this has value: 32"))
  }


  def infix_+(x1: String, x2: Boolean): String = "trans"

  "StringConcat" should "be virtualized" in {

    @virtualize
    def virtualizeIfTest() = "wefjbh" + true + "" + 6

    virtualizeIfTest() should be("trans6")
  }

  "StringCaseClassConcat" should "be virtualized" in {

    @virtualize
    def m = {
      case class C(i:Int) {def x = "wefjbh" + true}
      C(6).x
    }
    def virtualizeIfTest() = m

    virtualizeIfTest() should be("trans")
  }

  "method virtualizeIfTest" should "be virtualized" in {
    def m[T:Manifest](x: T) = manifest[T]

    @virtualize
    def virtualizeIfTest(cs: Boolean*) = if (cs) "yep" else "nope"

    virtualizeIfTest(true, false) should be("nope")
    virtualizeIfTest(true, true) should be("yep")
    m(virtualizeIfTest(true, false)) shouldBe manifest[String]

    @virtualize
    def virtualizeSuperTypeIfTest(cs: Boolean*) = if (cs) List(1,2,3) else "nope"

    virtualizeSuperTypeIfTest(true, false) shouldBe "nope"
    virtualizeSuperTypeIfTest(true, true) shouldBe List(1,2,3)
    m(virtualizeSuperTypeIfTest(true, false)) shouldBe manifest[Object with java.io.Serializable]
  }

  "object VirtualizeIfTest" should "be virtualized" in {

    @virtualize
    object VirtualizeIfTest {
      def apply(cs: Boolean*) = if (cs) "yep" else "nope"
    }

    VirtualizeIfTest(true, false) should be("nope")
    VirtualizeIfTest(true, true) should be("yep")
  }

  "VirtualizeIfTraitTest" should "be virtualized" in {

    @virtualize
    trait VirtualizeIfTrait {
      def apply(cs: Boolean*) = if (cs) "yep" else "nope"
    }

    object VirtualizeIfTraitTest extends VirtualizeIfTrait

    VirtualizeIfTraitTest(true, false) should be("nope")
    VirtualizeIfTraitTest(true, true) should be("yep")
  }

  "VirtualizeIfTrait2Test" should "be virtualized" in {

    trait IfListInt {
      def __ifThenElse[T](cs: Seq[Int], tb: => T, eb: => T): T = {
        if (cs forall (_ != 0)) tb else eb
      }
    }

    @virtualize
    trait VirtualizeIfTrait2 { this: IfListInt =>
      def apply(cs: Int*) = if (cs) "yep" else "nope"
    }

    object VirtualizeIfTrait2Test extends VirtualizeIfTrait2 with IfListInt

    VirtualizeIfTrait2Test(1, 0) should be("nope")
    VirtualizeIfTrait2Test(1, 1) should be("yep")
  }

  // Should use default `__ifThenElse` from EmbeddedControls.
  "defaultIfTest" should "be virtualized" in {

    @virtualize
    def defaultIfTest(c: Boolean) = if (c) "yep" else {
      var x = "no"
      x + "pe"
    }

    defaultIfTest(false) should be("nope")
    defaultIfTest(true) should be("yep")

    @virtualize
    def defaultSuperTypeIfTest(c: Boolean) = if (c) List(1,2,3) else "nope"

    defaultSuperTypeIfTest(false) shouldBe "nope"
    defaultSuperTypeIfTest(true) shouldBe List(1,2,3)

  }

  // Should use inner virtualized `__ifThenElse`
  "virtualizeInnerIfTest" should "be virtualized" in {

    // This overrides the `__ifThenElse` in `EmbeddedControls`
    def __ifThenElse[T](c: Boolean, thenBr: => T, elseBr: => T): T =
      if (!c) thenBr else elseBr

    @virtualize
    def virtualizeInnerIfTest(c: Boolean) = if (c) "yep" else "nope"

    virtualizeInnerIfTest(false) should be("yep")
    virtualizeInnerIfTest(true) should be("nope")
  }

  "virtualizeThenOnlyTest" should "be virtualized" in {
    def __ifThenElse[T](c: Boolean, thenBr: => T, elseBr: => T): T = if (!c) thenBr else elseBr

    var x = 3
    def set() { x = 5 }

    @virtualize
    def test() = {
      if (false) { set() }
      x
    }

    test() shouldBe 5
  }

  "virtualizeWhileDo" should "be virtualized" in {
    def __whileDo(cond: Seq[Boolean], body: => String): String = if (cond forall (_ == true)) body else "nope"

    @virtualize
    def virtualizeWhileTest(c: Boolean*) = while (c) { "yep" }

    virtualizeWhileTest(false, true) shouldBe "nope"
    virtualizeWhileTest(true, true) shouldBe "yep"
  }

  case class Var[T](var x: T)
  def __newVar(init: Int): Var[Int] = Var(init + 1)
  def __assign(lhs: Var[Int], rhs: Int): Unit = lhs.x = lhs.x + rhs
  implicit def __readVar(lhs: Var[Int]): Int = lhs.x

  "virtualizeClassFieldTest" should "be virtualized" in {
    def __ifThenElse[T](c: Boolean, thenBr: => T, elseBr: => T): T = if (!c) thenBr else elseBr

    @virtualize
    case class Test(x: Boolean) {
      private var value = 5
      def test(): Int = if (x) 5 else 3
      def evaluate(): Int = { value = 3; value }
    }

    val x = Test(true)
    x.test() shouldBe 3
    x.evaluate() shouldBe 9
    "x.value" shouldNot compile
  }


  "virtualizeVariables" should "be virtualized" in {
    @virtualize
    def virtualizeVariablesTest(): Int = {
      var x = 5 // x = 6
      x = 3 // x = 9
      x      // __readVar injection
    }

    virtualizeVariablesTest() shouldBe 9
  }

  "virtualizePlusEquals" should "be virtualized" in {
    implicit class VarOps(lhs: Var[Int]) {
      def +=(rhs: Int): Unit = lhs.x += rhs + 1
      def -=(rhs: Int): Unit = lhs.x += rhs + 2
      def *=(rhs: Int): Unit = lhs.x += rhs + 3
      def /=(rhs: Int): Unit = lhs.x += rhs + 4
    }

    @virtualize
    var x = 5

    @virtualize
    def test(): Int = {
      x += 3  // 10
      x -= 3  // 15
      x *= 3  // 21
      x /= 3  // 28
      x
    }
    test() shouldBe 28
  }

  "virtualizeImplicitPlusEquals" should "be virtualized" in {
    implicit class VarOps(lhs: Var[Int]) {
      def +=(rhs: Int): String = {
        println("hello!")
        lhs.x + " += " + rhs
      }
    }
    @virtualize
    var x = 3

    @virtualize
    def test() = x += 5

    @virtualize
    def get(): Int = x

    get() shouldBe 4
    test() //shouldBe "4 += 5"
    get() shouldBe 4

    println(x)
  }

  "virtualizeVariables2" should "not be virtualized" in {
    var x = 5

    @virtualize
    def virtualizeVariablesTest(): Int = {
      x = 3
      x
    }
    virtualizeVariablesTest() shouldBe 3
  }

  "virtualizeVariables3" should "be virtualized" in {
    @virtualize
    var x = 5

    @virtualize
    def virtualizeVariablesTest(): Int = {
      x = 3
      x
    }
    virtualizeVariablesTest() shouldBe 9
  }

  // Not supported
  "virtualizeVariables4" should "be virtualized" in {
    var x = 5

    @virtualize
    def virtualizeVariablesTest1(): Int = {
      var x = 5
      x = 3
      x
    }

    @virtualize
    def virtualizeVariablesTest2(): Int = {
      x = 3
      x
    }

    virtualizeVariablesTest1() shouldBe 9
    virtualizeVariablesTest2() shouldBe 3
  }

  "primitiveArithmetic" should "have the right result type" in {
    "val a: Double = infix_+(3, 5.0)" should compile
    "val b: Long = infix_+(3, 5l)" should compile
    "val c: Float = infix_+(3f, 5l)" should compile
    "val d: Double = infix_+(5.0, 5f)" should compile
    "val e: Int = infix_+(3, 5.0)" shouldNot compile
  }

  "virtualizeAnyMethods" should "be virtualized" in {
    def infix_==(x1: List[Int], x2: List[Int]) = Nil
    def infix_!=(x1: List[Int], x2: List[Int]) = Nil
    def infix_##(x: List[Int]) = Nil
    def infix_equals(x1: List[Int], x2: List[Int]) = Nil
    def infix_hashCode(x: List[Int]) = Nil
    def infix_asInstanceOf[T](x: List[Int]) = List(1)
    def infix_isInstanceOf[T](x: List[Int]) = Nil
    def infix_toString(x: List[Int]) = Nil
    def infix_getClass(x: List[Int]) = Nil

    @virtualize
    def virtualizeAnyTest(x: List[Int], y: List[Int]) = {
      (x == y) ++
      (x != y) ++
      x.## ++ x.##() ++
      (x equals y) ++ x.equals(y) ++
      x.hashCode ++ x.hashCode() ++
      x.isInstanceOf[Int] ++
      x.asInstanceOf[Int] ++
      x.toString ++ x.toString() ++
      x.getClass ++ x.getClass()
    }

    virtualizeAnyTest(List(1,2,3), List(4,5,6)) shouldBe List(1)
  }

  // note: you can't define overloaded methods inside the test block below
  def infix_wait(x: List[Int]): List[Int] = Nil
  def infix_wait(x: List[Int], timeout: Long): List[Int] = Nil
  def infix_wait(x: List[Int], timeout: Long, nanos: Int): List[Int] = Nil

  "virtualizeAnyRefMethods" should "be virtualized" in {
    def infix_eq(x1: List[Int], x2: List[Int]) = Nil
    def infix_ne(x1: List[Int], x2: List[Int]) = Nil
    def infix_notify(x: List[Int]) = Nil
    def infix_notifyAll(x: List[Int]) = List(1)
    def infix_synchronized[T](x: List[Int], body: => T) = Nil
    def infix_clone(x: List[Int]) = Nil
    def infix_finalize(x: List[Int]) = Nil

    @virtualize
    def virtualizeAnyRefTest(x: List[Int], y: List[Int]) = {
      (x eq y) ++ x.eq(y) ++
      (x ne y) ++ x.ne(y) ++
      x.notify ++ x.notify() ++
      x.notifyAll ++ x.notifyAll() ++
      x.synchronized("hello") ++
      x.wait ++ x.wait() ++
      x.wait(10) ++
      x.wait(10, 10) ++
      x.clone ++ x.clone() ++
      x.finalize ++ x.finalize()
    }

    virtualizeAnyRefTest(List(1,2,3), List(4,5,6)) shouldBe List(1,1)
  }

  "numericPlusTest" should "not be virtualized" in {
    def numericPlusTest(a: Int, b: Int): Int = a+b
    numericPlusTest(1, 2) should be(3)
  }

  "virtualizePlusTest" should "be virtualized" in {
    def infix_+(a1: Any, a2: Any) = a2.toString + a1 //+ on Any is not virtualized!
    @virtualize
    def virtualizePlusTest(a: String, b: List[Boolean]) = a + b //only "StringLiteral"+b will be virtualized!
    virtualizePlusTest("you", List(false)) should be("youList(false)")
  }

  "virtualizeAnyPlusTest" should "not be virtualized" in {
    @virtualize
    def virtualizePlusTest(a: Any, b: List[Boolean]) = a.toString + b //only "literal"+b will be virtualized!
    virtualizePlusTest("you", List(false)) should be("youList(false)")
  }

  "virtualizePlusTestStringLiteral" should "be virtualized" in {
    def infix_+(s: String, a: Any) = a.toString + s //no need to overwrite?
    @virtualize
    def virtualizePlusTest(a: Any) = "test" + a
    virtualizePlusTest(List(false)) should be("List(false)test") //check that call is actually intercepted
  }

  "method virtualizeEqualsTest" should "be virtualized" in {

    @virtualize
    def virtualizeEqualsTest(a: List[Boolean], b: List[Boolean]) = a == b

    virtualizeEqualsTest(List(true, true), List(true, false)) should be(false)
    virtualizeEqualsTest(List(true, true), List(true, true, false)) should be(true)
    (List(true, true) == List(true, true, false)) should be(false)
  }

  "object VirtualizeEqualsTest" should "be virtualized" in {

    @virtualize
    object VirtualizeEqualsTest {
      def apply(a: List[Boolean], b: List[Boolean]) = a == b
    }

    VirtualizeEqualsTest(List(true, true), List(true, false)) should be(false)
    VirtualizeEqualsTest(List(true, true), List(true, true, false)) should be(true)
    (List(true, true) == List(true, true, false)) should be(false)
  }

  // Should use default `Any.==` method from EmbeddedControls.
  "defaultEqualsTest" should "be virtualized" in {

    @virtualize
    def defaultEqualsTest(a: Boolean, b: Boolean) = a == b

    defaultEqualsTest(false, true) should be(false)
    defaultEqualsTest(true, true) should be(true)
  }

  "guardEqualsTest" should "be virtualized" in {
    def guardEqualsSanityTest(xs: List[Boolean], ys: List[Boolean]) = (xs,ys) match {
      case (x::xs, y::ys) if infix_==(xs,ys) => true
      case _ => false
    }

    @virtualize
    def guardEqualsTest(xs: List[Boolean], ys: List[Boolean]) = (xs,ys) match {
      case (x::xs, y::ys) if xs==ys => true
      case _ => false
    }
    guardEqualsSanityTest(List(false, true, false), List(true, true)) should be(true)
    guardEqualsTest(List(false, true, false), List(true, true)) should be(true)
  }

  "parameter of virtualizeParamTest" should "not be virtualized" in {

    val c = false
    def virtualizeParamTest(
      @virtualize s: String = if (c) "yep" else "nope") = s

    virtualizeParamTest() should be("nope")
  }

  "type parameter of virtualizeTParamTest" should "not be virtualized" in {

    def virtualizeTParamTest[@virtualize T](s: T) = s

    virtualizeTParamTest("nope") should be("nope")
  }

  "try expression in virtualizeTryTest" should "not be virtualized" in {

    @virtualize
    def virtualizeTryTest[T](s: => T) = try s catch { case _:Exception => "hello" }

    virtualizeTryTest("nope") should be("nope")
  }

  "throw expression in virtualizeThrowTest" should "not be virtualized" in {

    case class MyException(msg: String) extends Exception

    @virtualize
    def virtualizeThrowTest(e: String) = throw MyException(e)

    try {
      virtualizeThrowTest("nope")
    } catch {
      case MyException(e) => e should be("nope")
    }
  }

  "isInstanceOf and asInstanceOf" should "not be virtualized" in {
    @virtualize
    def virtualizeInstanceOf(o: Object) = if (o.isInstanceOf[String]) o.asInstanceOf[String] else null
    virtualizeInstanceOf("hello") should be("hello")
    virtualizeInstanceOf(Nil) should be(null)
  }

  "Scopes" should "be generated" in {
    case class MyCls[T](i:T)
    trait Ops {
      def m(i:Int):MyCls[Int]
      def apply: Any
    }
    trait OpsExp[R] extends Ops{
      def m(i:R) = MyCls(i)
//      def apply = ??? //(i:Int):MyCls = m(i)
    }

    //new scope needs to be inside an object for this to work!
    //none should be needed!
    @virtualize
    class OptiQL {
      def MyDSL[R](b: => R) = new Scope[Ops, OpsExp[Int], Int](b)
      val result = MyDSL {
        println("hello")
        m(5).i
      }
      result should be (5)
    }
  }

  // "withTpe" should "crash in this case" in {
  //   //have to make an explicit call to 'execute' side effects
  //    //values could not be annotated...
  //   @virtualize
  //   {
  //     val magic = withTpee(Community){ println("with state") }
  //   }
  // }

}



class VirtualizeVarsSpec extends FlatSpec with ShouldMatchers with EmbeddedControls {
  "virtualizePlusEquals" should "be virtualized" in {
    case class Var[T](var x: T)
    def __newVar(init: Int): Var[Int] = Var(init + 1)
    def __assign(lhs: Var[Int], rhs: Int): Unit = lhs.x = lhs.x + rhs
    implicit def __readVar(lhs: Var[Int]): Int = lhs.x

    implicit class VarOps(lhs: Var[Int]) {
      def +=(rhs: Int): String = s"${lhs.x} += $rhs"
    }

    @virtualize
    def test() = {
      var x = 0
      x += 1
    }
    test() shouldBe "1 += 1"
  }
}

