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

  case class Person(name: String, age: Int) extends Record
  case class Couple(her: String, him: String) extends Record
  case class PeopleDB(people: List[Person], couples: List[Couple]) extends Record

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

  // 3 nesting

  // corporate schema

  type Org = List[{
    val departments: List[{val dpt: String}]
    val employees: List[{val dpt: String; val emp: String}]
    val tasks: List[{val emp: String; val tsk: String}]
  }]

  val org = new Record {
    val departments = List(
      new Record { val dpt = "Product"},
      new Record { val dpt = "Quality"},
      new Record { val dpt = "Research"},
      new Record { val dpt = "Sales"})
    val employees = List(
      new Record { val dpt = "Product"; val emp = "Alex"},
      new Record { val dpt = "Product"; val emp = "Bert"},
      new Record { val dpt = "Research"; val emp = "Cora"}, 
      new Record { val dpt = "Research"; val emp = "Drew"}, 
      new Record { val dpt = "Research"; val emp = "Edna"}, 
      new Record { val dpt = "Sales"; val emp = "Fred"})
    val tasks = List(
      new Record { val emp = "Alex"; val tsk = "build"},
      new Record { val emp = "Bert"; val tsk = "build"}, 
      new Record { val emp = "Cora"; val tsk = "abstract"},
      new Record { val emp = "Cora"; val tsk = "build"},
      new Record { val emp = "Cora"; val tsk = "design"},
      new Record { val emp = "Drew"; val tsk = "abstract"},
      new Record { val emp = "Drew"; val tsk = "design"},
      new Record { val emp = "Edna"; val tsk = "abstract"},
      new Record { val emp = "Edna"; val tsk = "call"},
      new Record { val emp = "Edna"; val tsk = "design"},
      new Record { val emp = "Fred"; val tsk = "call"})
  }

  def exists[T](xs: List[T]) = xs.nonEmpty // helper method

  def expertise(u: String): List[{ val dpt: String }] =
    for {
      d <- org.departments
      if !exists(
        for {
          e <- org.employees
          if d.dpt == e.dpt && !exists(
            for {
              t <- org.tasks
              if e.emp == t.emp && t.tsk == u 
            } yield new Record {})
        } yield new Record {})
    } yield new Record { val dpt = d.dpt }

  val departmentsFullOfAbstracters = expertise("abstract")

  // 3.1 Nested structures

  type NestedOrg = List[{
    val dpt: String
    val employees: List[{
      val emp: String
      val tasks: List[String]
    }]
  }]

  val nestedOrg: NestedOrg =
    for { 
      d <- org.departments
    } yield new Record {
      val dpt = d.dpt
      val employees = for {
        e <- org.employees
        if d.dpt == e.dpt
      } yield new Record {
        val emp = e.emp 
        val tasks = for {
          t <- org.tasks 
          if e.emp == t.emp
        } yield t.tsk
      }
    }

  // 3.2 Higher-order queries

  def any[A](xs: List[A])(p: A => Boolean): Boolean =
    exists(for (x <- xs if p(x)) yield new Record { })

  def all[A](xs: List[A])(p: A => Boolean): Boolean =
    !any(xs)(x => !p(x))

  def contains[A](xs: List[A], u: A): Boolean =
    any(xs)(x => x == u)

  def expertise2(u: String): List[{ val dpt: String }] =
    for {
      d <- nestedOrg
      if all(d.employees)(e => contains(e.tasks, u)) 
    } yield new Record { val dpt = d.dpt }

  val departmentsFullOfAbstracters2 = expertise2("abstract")


  // 4 From XPath to SQl

  /*
  +----+--------+------+-----+------+
  | id | parent | name | pre | post |
  +----+--------+------+-----+------+
  |  0 |   -1   | #doc |  0  |  13  |
  |  1 |    0   |   a  |  1  |  12  |
  |  2 |    1   |   b  |  2  |   5  |
  |  3 |    2   |   c  |  3  |   4  |
  |  4 |    1   |   d  |  6  |  11  |
  |  5 |    4   |   e  |  7  |   8  |
  |  6 |    4   |   f  |  9  |  10  |
  +----+--------+------+-----+------+
  */

  case class Node(
    val id: Int,
    val parent: Int,
    val name: String,
    val pre: Int,
    val post: Int 
  ) extends Record

  val db_xml = List(
    Node(0, -1, "#doc", 0, 13),
    Node(1,  0, "a",    1, 12),
    Node(2,  1, "b",    2,  5),
    Node(3,  2, "c",    3,  4),
    Node(4,  1, "d",    6, 11),
    Node(5,  4, "e",    7,  8),
    Node(6,  4, "f",    9, 10))


  abstract class Axis
  case object Self extends Axis
  case object Child extends Axis
  case object Descendant extends Axis
  case object DescendantOrSelf extends Axis
  case object Following extends Axis
  case object FollowingSibling extends Axis
  case class Rev(x: Axis) extends Axis
 
  def parent = PAxis(Rev(Child))
  def ancestor = PAxis(Rev(Descendant))
  def preceding = PAxis(Rev(Following))

  abstract class Path
  case class PSeq(x: Path, y: Path) extends Path
  case class PAxis(x: Axis) extends Path
  case class NameTest(x: String) extends Path
  case class Filter(x: Path) extends Path

  def axis(ax: Axis)(s: Node, t: Node): Boolean = ax match {
    case Self             => s.id == t.id
    case Child            => s.id == t.parent
    case Descendant       => s.pre < t.pre && t.post < s.post
    case DescendantOrSelf => s.pre <= t.pre && t.post <= s.post
    case Following        => s.pre < t.pre 
    case FollowingSibling => s.post < t.pre && s.parent == t.parent
    case Rev(ax)          => axis(ax)(t, s)
  }

  // code in paper:
  // | Rev(axis) → <@ fun(s, t) → (%axis(ax))(t, s) @> 
  //       ^^^^                     ^^^^
  //   should be ax?

  def path(p : Path)(s: Node, u: Node): Boolean = p match {
    case PSeq(p, q) => 
      any(db_xml)(t => path(p)(s, t) && path(q)(t, u))
    case PAxis(ax) => 
      axis(ax)(s, u)
    case NameTest(name) =>
      s.id == u.id && s.name == name
    case Filter(p) => 
      s.id == u.id && any(db_xml)(t => path(p)(s, t))
  }

  def xpath(p : Path): List[Int] = for {
    root <- db_xml
    s <- db_xml
    if (root.parent == -1) && path(p)(root, s)
  } yield s.id


  //  /*/*
  val xp0 = PSeq(PAxis(Child), PAxis(Child))
  //  //*/parent::*
  val xp1 = PSeq(PAxis(Descendant), parent)
  // Q: this produces 0,1,2,4 but the paper says 1,2,4 (?)

  //  //*[following-sibling::d]
  val xp2 = PSeq(PAxis(Descendant), Filter(PSeq(PAxis(FollowingSibling),NameTest("d"))))
  //  //f[ancestor::*/preceding::b]()
  val xp3 = PSeq(PSeq(PAxis(Descendant), NameTest("f")), Filter(PSeq(ancestor,PSeq(preceding, NameTest("b")))))

  val xr0 = xpath(xp0)
  val xr1 = xpath(xp1)
  val xr2 = xpath(xp2)
  val xr3 = xpath(xp3)

}

trait Util {
  // Record supertype: pretty printing etc
  abstract class Record extends Product {
    lazy val elems = {
      val fields = getClass.getDeclaredFields.toList
      for (f <- fields if !f.getName.contains("$")) yield {
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
        Console.println(departmentsFullOfAbstracters)
        Console.println(nestedOrg)
        Console.println(departmentsFullOfAbstracters2)

        Console.println(xr0)
        Console.println(xr1)
        Console.println(xr2)
        Console.println(xr3)

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
