package org.scala_lang.virtualized
package annotation

import org.scala_lang.virtualized.SourceContext
import org.scalatest.{ FlatSpec, ShouldMatchers }


trait RecordsTests extends FlatSpec with ShouldMatchers with RecordOps {

  implicit def unit[T](x: T): Rep[T]

  def m[T:Manifest](x: Rep[T]) = manifest[T]

  // note: we normally don't write this manually, here only for code sharing purposes
  type Person = Record {
    val name: String
    val age: Int
    val height: Double
  }

  def anonymous() = {
    val person = Record(name = "Hans", age = 7, height = 5.5)
    app(person)
  }

  //TODO: best annotation name? (@record collides with type Record)
  def named() = {
    @mRecord case class Person(name: String, age: Int, height: Double)
    val person = Person("Hans", 7, 5.5)
    app(person)
  }

  def app(person: Rep[Person]) = {
    m(person) <:< manifest[Record] shouldBe true
    m(person) <:< manifest[Person] shouldBe true

    // dynamic field access methods
    record_select[String](person, "name") shouldBe unit("Hans")
    record_select[Int](person, "age") shouldBe unit(7)
    record_select[String](person, "height") shouldBe unit(5.5)

    m(record_select[String](person, "name")) shouldBe manifest[String]
    an [Exception] should be thrownBy record_select[String](person, "namee")

    // macro field access methods
    person.name shouldBe unit("Hans")
    person.age shouldBe unit(7)
    person.height shouldBe unit(5.5)

    m(person.name) shouldBe manifest[String]
    "person.namee" shouldNot compile
    
    // Records should always have a RefinedManifest
    m(person) shouldBe a [RefinedManifest[_]]
    m(person).asInstanceOf[RefinedManifest[_]].fields should contain inOrderOnly (("name", manifest[String]), ("age", manifest[Int]), ("height", manifest[Double]))
  }

  "anonymous records" should "be virtualized" in anonymous()

  "named records" should "be virtualized" in named()
  
  it should "not compile with methods" in {
    """@mRecord
    case class Person(name: String, age: Int, town: String) {
      def getName = name
    }
    """ shouldNot compile
  }

  it should "not compile with no fields" in {
    "@mRecord case class Person()" shouldNot compile
  }

  it should "not compile with type parameters" in {
    "@mRecord case class Person[T](name: T)" shouldNot compile
  }

  it should "not compile without a case class" in {
    "@mRecord class Person(val name: String)" shouldNot compile
  }

}

class RecordsLifted extends RecordsTests {
  case class Box[+T](x: T)
  type Rep[+T] = Box[T]

  def unit[T](x: T) = Box(x)

  case class SimpleRecord(fields: Seq[(String, _)]) extends Record
  
  def record_new[T: RefinedManifest](fields: (String, Rep[_])*): Rep[T] = 
    Box(SimpleRecord(fields)).asInstanceOf[Rep[T]]
  
  def record_select[T: Manifest](record: Rep[Record], field: String): Rep[T] = 
    record.x.asInstanceOf[SimpleRecord].fields.find(_._1 == field).get._2.asInstanceOf[Rep[T]]
}

class RecordsDirect extends RecordsTests {
  type Rep[+T] = T

  def unit[T](x: T) = x

  case class SimpleRecord(fields: Seq[(String, _)]) extends Record

  def record_new[T: RefinedManifest](fields: (String, Rep[_])*): Rep[T] = 
    SimpleRecord(fields).asInstanceOf[Rep[T]]
  
  def record_select[T: Manifest](record: Rep[Record], field: String): Rep[T] = 
    record.asInstanceOf[SimpleRecord].fields.find(_._1 == field).get._2.asInstanceOf[Rep[T]]
}
