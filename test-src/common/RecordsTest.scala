package scala.virtualization.lms
package common
import scala.virtualization.lms.epfl._

import scala.virtualization.lms.epfl.test2._
import scala.virtualization.lms.epfl.test3._

import org.scala_lang.virtualized.virtualize
import org.scala_lang.virtualized.Struct
import common._

@virtualize
trait BasicProg extends TestOps {
  def f(s: Rep[String]): Rep[String] = {
    val res = Record(name = s, lastName = s)
    res.lastName
  }
}

@virtualize
trait NestedProg extends TestOps {
  def f(s: Rep[String]): Rep[String] = {
    val res = Record(name = s, lastName = Record(fathers = s, last = s))

    res.lastName.fathers
  }
}

@virtualize
trait AsArgumentsProg extends TestOps {
  def f(s: Rep[String]): Rep[String] =
    (if(unit(true)) Record(name = s) else Record(name = s)).name
}

@virtualize
trait MixedTypesProg extends TestOps {
  implicit def lift[T: Manifest](t: T): Rep[T]= unit(t)
  def f(s: Rep[String]): Rep[String] =
    Record(name = s, lastName = "last").lastName
}


/*
FIXME(trans) this currently doesn't compile:

[error] /Users/me/Desktop/tmpstuff/lms-macro/test-src/common/RecordsTest.scala:49: type mismatch;
[error]  found   : scala.reflect.Manifest[Object]
[error]  required: Manifest[AnyRef{val name: String}]
[error] Note: Object >: AnyRef{val name: String}, but trait Manifest is invariant in type T.
[error] You may wish to investigate a wildcard type such as `_ >: AnyRef{val name: String}`. (SLS 3.2.10)
[error]     val xs1 = xs.flatMap { b => ys }
[error]                          ^

@virtualize
trait FlatMapProg extends TestOps with ListOps {
  implicit def lift[T: Manifest](t: T): Rep[T]= unit(t)
  type Names = List[{ val name: String}]
  def f(s: Rep[String]): Rep[String] = {
    val xs: Rep[List[Int]] = List(1,2,3)
    val ys: Rep[Names] = List(Record(name = "A"),Record(name = "B"))
    val xs1 = xs.flatMap { b => ys }
    "xx"
  }
}
*/

trait TestOps extends Functions with Equal with IfThenElse with RecordOps with StructOps
trait TestExp extends FunctionsExp with EqualExp with IfThenElseExp with StructExp
trait TestGen extends ScalaGenFunctions with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenStruct {val IR: TestExp}

class TestBasic extends FileDiffSuite {

  val prefix = home + "test-out/common/records-"

  def testRecordsBasic = {
    withOutFile(prefix+"basic") {
      object BasicProgExp extends BasicProg with TestExp
      import BasicProgExp._

      val p = new TestGen { val IR: BasicProgExp.type = BasicProgExp }
      val stream = new java.io.PrintWriter(System.out)
      p.emitSource(f, "RecordsBasic", stream)
      p.emitDataStructures(stream)
    }
    assertFileEqualsCheck(prefix+"basic")
  }

  def testRecordsNested = {
    withOutFile(prefix+"nested") {
      object NestedProgExp extends NestedProg with TestExp
      import NestedProgExp._

      val p = new TestGen { val IR: NestedProgExp.type = NestedProgExp }
      val stream = new java.io.PrintWriter(System.out)
      p.emitSource(f, "RecordsNested", stream)
      p.emitDataStructures(stream)
    }
    assertFileEqualsCheck(prefix+"nested")
  }

  def testAsArguments = {
    withOutFile(prefix+"as-arguments") {
      object AsArgumentsProgExp extends AsArgumentsProg with TestExp
      import AsArgumentsProgExp._

      val p = new TestGen { val IR: AsArgumentsProgExp.type = AsArgumentsProgExp }
      val stream = new java.io.PrintWriter(System.out)
      p.emitSource(f, "AsArguments", stream)
      p.emitDataStructures(stream)
    }
    assertFileEqualsCheck(prefix+"as-arguments")
  }

  def testMixedTypes = {
    withOutFile(prefix+"mixed-types") {
      object MixedTypesProgExp extends MixedTypesProg with TestExp
      import MixedTypesProgExp._

      val p = new TestGen { val IR: MixedTypesProgExp.type = MixedTypesProgExp }
      val stream = new java.io.PrintWriter(System.out)
      p.emitSource(f, "MixedTypes", stream)
      p.emitDataStructures(stream)
    }
    assertFileEqualsCheck(prefix+"mixed-types")
  }
}
