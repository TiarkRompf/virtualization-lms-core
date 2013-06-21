package scala.virtualization.lms
package epfl
package test14

import common._
import internal._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

/*
Staged SQL-like queries, inspired by "the essence of LINQ":
http://homepages.inf.ed.ac.uk/slindley/papers/essence-of-linq-draft-december2012.pdf
*/

trait Schema extends Util {
  
  // people db schema

  case class Person(name: String, age: Int)
  case class Couple(her: String, him: String)
  case class PeopleDB(people: List[Person], couples: List[Couple])

  val db = PeopleDB(
    people = List(
      Person("Alex", 60),
      Person("Bert", 55),
      Person("Cora", 33),
      Person("Drew", 31),
      Person("Edna", 21),
      Person("Fred", 60)),
    couples = List(
      Couple("Alex", "Bert"),
      Couple("Cora", "Drew")))

  // 2.1 Comprehensions and queries / 2.2 Query via quotation

  val differences: List[{ val name: String; val diff: Int }] =
    for {
      c <- db.couples
      w <- db.people
      m <- db.people
      if c.her == w.name && c.him == m.name && w.age > m.age
    } yield new Record { 
      val name = w.name
      val diff = w.age - m.age
    }

  // 2.3 Abstracting over values

  type Names = List[{ val name: String}]
  def range(a: Int, b: Int): Names =
    for {
      w <- db.people
      if a <= w.age && w.age < b
    } yield new Record {
      val name = w.name
    }

  val thirtySomethings = range(30,40)

  // 2.4 Abstracting over a predicate

  def satisfies(p: Int => Boolean): Names = 
    for {
      w <- db.people
      if p(w.age)
    } yield new Record {
      val name = w.name
    }

  val thirtySomethings2 = satisfies(x => 30 <= x && x < 40)
  val evenAge = satisfies(_ % 2 == 0)

  // 2.5 Composing queries

  def ageFromName(s: String): List[Int] =  // paper has return type 'int' but says 'list of int' in the text (?)
    for {
      u <- db.people
      if u.name == s
    } yield u.age


  def rangeFromNames(s: String, t: String): Names =
    for {
      a <- ageFromName(s)
      b <- ageFromName(t)
      r <- range(a,b)
    } yield r

  val rangeBertEdna = rangeFromNames("Edna", "Bert")

  // 2.6 Dynamically generated queries

  abstract class Predicate
  case class Above(x: Int) extends Predicate
  case class Below(x: Int) extends Predicate
  case class And(x: Predicate, y: Predicate) extends Predicate
  case class Or(x: Predicate, y: Predicate) extends Predicate
  case class Not(x: Predicate) extends Predicate

  val t0: Predicate = And(Above(30), Below(40))
  val t1: Predicate = Not(Or(Below(30), Above(40)))

  def P(t: Predicate)(x: Int): Boolean = t match {
    case Above(a) => a <= x
    case Below(a) => x < a
    case And(t, u) => P(t)(x) && P(u)(x)
    case Or(t, u) => P(t)(x) || P(u)(x)
    case Not(t)=> !P(t)(x)
  }

  val thirtySomethings3 = satisfies(P(t0))
  val thirtySomethings4 = satisfies(P(t1))

}

trait Util {
  // Record supertype: pretty printing etc
  abstract class Record extends Product {
    lazy val elems = {
      val fields = getClass.getDeclaredFields.toList
      fields.map { f => 
        f.setAccessible(true)
        (f.getName, f.get(this))
      }
    }
    def canEqual(that: Any) = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator = elems.iterator
    override def toString = elems.map(e => s"${e._1}:${e._2}").mkString("{",",","}")
  }

}



class TestQueries extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test14-"
  
  trait DSL extends ScalaOpsPkg with Compile with LiftPrimitives {
    def test(): Unit
  }
  
  trait Impl extends DSL with ScalaOpsPkgExp with ScalaCompile { self =>
    override val verbosity = 1
    dumpGeneratedCode = true
    val codegen = new Codegen { val IR: self.type = self }
    val runner = new Runner { val p: self.type = self }
    runner.run()
  }
  
  trait Codegen extends ScalaCodeGenPkg {
    val IR: Impl
  }  
  
  trait Runner {
    val p: Impl
    def run() = {
      p.test()
    }
  }
  


  def testQueries1 = withOutFileChecked(prefix+"queries1") {
    trait Prog extends DSL with Schema {
      def test() = {

        Console.println(db)
        Console.println(differences)
        Console.println(thirtySomethings)
        Console.println(thirtySomethings2)
        Console.println(evenAge)
        Console.println(rangeBertEdna)
        Console.println(thirtySomethings3)
        Console.println(thirtySomethings4)


        val f = compile { x: Rep[Int] =>

          val a = x + 1
          val b = x * 2

          a+b
        }

        Console.println(f(9))
        Console.println(f(3))
        Console.println(f(1))

      }
    }
    new Prog with Impl
  }


}
