package scala.virtualization.lms.common

//import language.experimental.macros
import org.scala_lang.virtualized.{RefinedManifest, SourceContext}
//import scala.virtualization.lms.common.record
import org.scalatest._

import scala.annotation.StaticAnnotation

/**
  * Created by cedricbastin on 09/11/15.
  */
class RecordsAnnotationTest extends FlatSpec with Suite with ShouldMatchers with TestOps with TestExp with LiftAll {

//  case class Exp[T](t:T)
//  type Rep[T] = Exp[T]
//  implicit def rep[T](t:T):Rep[T] = Exp(t)
//  implicit def unrep[T](r:Rep[T]):T = r.t
//  trait Record {// for use as Record {}
//  def list:Seq[Any]
//  }
//  object Record { // for use as Record
//  def apply(a:Any*) = () //FixMe: return type
//  }

  "virtualizeRecord" should "be virtualized" in {
    val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
    import universe._

    def m[T](r: Rep[T])(implicit m:Manifest[T]): List[_] = {
      m match {
        case rf:RefinedManifest[T] => rf.fields
        case _ => Nil
      }
    }

    type Test = Record

    object O {
      type RTest = Record {
        val r_key: Rep[Int];
        val r_name: Rep[String]
      }

      def RTest(key: Rep[Int], name: Rep[String]): Rep[RTest] = Record(r_key = key, r_name = name)
    }
    import O._
    val x:Rep[RTest] = RTest(unit(6), unit("ewf"))
    assert(m(x).size == 2)
    val y = RTest(unit(6), unit("ewf"))
    assert(m(y).size == 2)
    val z:Rep[RTest] = y
    assert(m(z).size == 2)
//    m(RTest(45, "afds"))

    object O_RTest3 {
      type RTest3 = Record {
        val r_key: Rep[Int];
        val r_name: Rep[String];
        val r_comment: Rep[String]
      };
      def RTest3(key: Rep[Int], name: Rep[String], comment: Rep[String]): Rep[RTest3] = Record(r_key = key, r_name = name, r_comment = comment)
    }

    println("done")

//    @mRecord
//    case class RTest0()
//    import O_RTest0._
//    val o0 = RTest0()

//    @mRecord
//    case class RTest1()
//    import O_RTest1._
//    val o1 = RTest1()
//    println(o1.getClass())
//
    @mRecord
    case class RTest2(key: Int, name: String, comment: String)
    import O_RTest2._
    val o2 = RTest2(4, "Hello", "World")
  }
}
