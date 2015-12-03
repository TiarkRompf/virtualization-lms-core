package scala.virtualization.lms.common

//import language.experimental.macros
import org.scala_lang.virtualized.{RefinedManifest, SourceContext}

import scala.virtualization.lms.epfl.FileDiffSuite

//import scala.virtualization.lms.common.record
import org.scalatest._

import scala.annotation.StaticAnnotation

class RecordsAnnotationTest extends FlatSpec with FileDiffSuite with ShouldMatchers with TestOps with TestExp with LiftAll {
  //helper function to check wheter the RefinedManifest go created correclty
  def m[T](r: Rep[T])(implicit m:Manifest[T]): List[_] = {
    m match {
      case rf:RefinedManifest[T] => rf.fields
      case _ => sys.error("no refined manifest found")
    }
  }

  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe._

  behavior of "An empty case clase"

  it should "throw an exception" in {
    an [Exception] should be thrownBy {
      @mRecord
      case class RTest()
    }
  }

  behavior of "A case class with one argument"

  it should "work for primitive types" in {
    @mRecord
    case class RTest1(i:Int)
    import O_RTest1._
    val rt:RTest1 = RTest1(42)
    println(rt.r_i)
//    rt.r_i should be (42)
  }

  it should "also work for object types" in {
    @mRecord
    case class RTest1(a:Any)
    import O_RTest1._
    val rt = RTest1(List(1,2,3))
//    Def(rt.r_a) should be (unit(42))
  }

  behavior of "A case class with more argument"

  it should "work" in {
    @mRecord
    case class RTest2(i:Int, s:String)
    import O_RTest2._
    val rt = RTest2(42, "sdf")
  }

  behavior of "A case class with nested argument"

  it should "work" in {
    @mRecord
    case class RTest1(i:Int)
    import O_RTest1._
    @mRecord
    case class RTest2(i:Int, r:RTest1)
    import O_RTest2._
    val rt = RTest2(42, RTest1(45))
  }

  behavior of "A case class with polymorphic argument"

  "mRecord2ArgsPoly" should "work" in {
    an [Exception] should be thrownBy {
      @mRecord
      case class RTest2[T, S](i: T, s: S)
      //      import O_RTest2._
      //      val rt = RTest2(42, "sdf")
    }
  }

  it should "created and implicit refined manifest" in {
    @mRecord
    case class RTest(i:Int, s:String)
    import O_RTest._
    val x:Rep[RTest] = RTest(6, "ewf")
    m(x).size should be (2)
    val y = RTest(unit(6), unit("ewf"))
    m(y).size should be (2)
    val z:Rep[RTest] = y
    m(z).size should be (2)
    //    m(RTest(45, "afds"))
  }

  //how to read symbols?
}
