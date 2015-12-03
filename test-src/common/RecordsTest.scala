package scala.virtualization.lms
package common
import scala.virtualization.lms.epfl._

import scala.virtualization.lms.epfl.test2._
import scala.virtualization.lms.epfl.test3._

import org.scala_lang.virtualized.{RefinedManifest, virtualize, Struct, SourceContext}
import common._

@virtualize
trait ManifestProg extends TestOps {
  def print[T](x:Rep[T])(implicit mani: Manifest[T]) = {
    println("toString: "+mani.toString)
    println("typeArguments: "+mani.typeArguments)
    println("arrayManifest: "+mani.arrayManifest)
    println("runtimeClass: "+mani.runtimeClass)
    println("getClass: "+mani.getClass)
    mani match {
      case rf: RefinedManifest[_] => println("fields: "+rf.fields)
      case _ => println("fields: not refined")
    }
  }
}

@virtualize
trait ExtendedProg extends TestOps with LiftAll with BaseExp {
  def m[T](x:Manifest[T]):String = x match {
    case rf:org.scala_lang.virtualized.RefinedManifest[_] => "RF: "+rf.fields.map(x=> x._1 +" => "+ m(x._2) +", " ).reduce(_+_)
    case m: Manifest[_] => m.toString
  }
  def f():String = {
    val res = Record(name = "nme", lastName = 56)
    def x[T](r:Exp[T]): String = {
      m(r.tp)
    }
    x(res)
  }
}

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

@virtualize
trait FlatMapProg extends TestOps with ListOps {
  implicit def lift[T: Manifest](t: T): Rep[T]= unit(t)
  type Names = List[Record{val name: String}]
  def f(s: Rep[String]): Rep[String] = {
    val xs: Rep[List[Int]] = List(1,2,3)
    val ys: Rep[Names] = List(Record(name = "A"),Record(name = "B"))
    val xs1 = xs.flatMap { b => ys }
    "xx"
  }
}


trait TestOps extends Functions with Equal with IfThenElse with RecordOps with StructOps
trait TestExp extends FunctionsExp with EqualExp with IfThenElseExp with StructExp
trait TestGen extends ScalaGenFunctions with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenStruct {val IR: TestExp}

class RecordsTest extends FileDiffSuite {


  val prefix = home + "test-out/common/records-"

  def testRecordManifest = {
    object BasicProgExp extends ManifestProg with TestExp {
      //method to test that a refined manifest actually got created with the correct number of fields
      def m[T](r: Rep[T])(implicit m:Manifest[T]): List[_] = {
        m match {
          case rf:RefinedManifest[T] => rf.fields
          case _ => Nil
        }
      }
      val res:Record = Record(name = unit("nme"), lastName = unit(56))
      assert(m(res).size == 2)
//      print(res)
      val res2 = Record(name = unit("nme"), lastName = unit(56), rec=Record(name = unit("nme"), lastName = unit(56)))
//      print(res2)
      assert(m(res2).size == 3)
    }
    BasicProgExp //we need to call the object in order for it to be executed
  }

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
