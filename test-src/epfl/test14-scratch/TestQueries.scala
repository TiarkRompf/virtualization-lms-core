/*TODO DISABLED
package scala.virtualization.lms
package epfl
package test14

import common._
import internal._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

/-*
Staged SQL-like queries, inspired by "the essence of LINQ":
http://homepages.inf.ed.ac.uk/slindley/papers/essence-of-linq-draft-december2012.pdf
*-/


// a non-staged, pure library implementation
trait Shallow extends Util {
  
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

  /-*
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
  *-/

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


  //  /-*-/-*
  val xp0 = PSeq(PAxis(Child), PAxis(Child))
  //  //-*-/parent::*
  val xp1 = PSeq(PAxis(Descendant), parent)
  // Q: this produces 0,1,2,4 but the paper says 1,2,4 (?)

  //  //-*[following-sibling::d]
  val xp2 = PSeq(PAxis(Descendant), Filter(PSeq(PAxis(FollowingSibling),NameTest("d"))))
  //  //f[ancestor::*-/preceding::b]()
  val xp3 = PSeq(PSeq(PAxis(Descendant), NameTest("f")), Filter(PSeq(ancestor,PSeq(preceding, NameTest("b")))))

  val xr0 = xpath(xp0)
  val xr1 = xpath(xp1)
  val xr2 = xpath(xp2)
  val xr3 = xpath(xp3)

}


// a staged implementation
trait Staged extends ScalaOpsPkg with LiftPrimitives with LiftString with StructOps {
  
  def database[T:Manifest](s: String): Rep[T]
  //trait Record extends Struct
  implicit def recordToRecordOps2(record: Rep[Record]) = new RecordOps(record.asInstanceOf[Rep[Record]])

  trait Inner {

  // people db schema

  type Person = Record {
    val name: String
    val age: Int
  }

  type Couple = Record {
    val her: String
    val him: String
  }

  type PeopleDB = Record {
    val people: List[Person]
    val couples: List[Couple]
  }

  //val db = staticData//database[PeopleDB]("PeopleDB")
  val db = database[PeopleDB]("db")
  /-*PeopleDB(
    people = List(
      Person("Alex", 60),
      Person("Bert", 55),
      Person("Cora", 33),
      Person("Drew", 31),
      Person("Edna", 21),
      Person("Fred", 60)),
    couples = List(
      Couple("Alex", "Bert"),
      Couple("Cora", "Drew")))*-/

  // 2.1 Comprehensions and queries / 2.2 Query via quotation

  val differences: Rep[List[{ val name: String; val diff: Int }]] =
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
  def range(a: Rep[Int], b: Rep[Int]): Rep[Names] =
    for {
      w <- db.people
      if a <= w.age && w.age < b
    } yield new Record {
      val name = w.name
    }

  val thirtySomethings = range(30,40)

  // 2.4 Abstracting over a predicate

  def satisfies(p: Rep[Int] => Rep[Boolean]): Rep[Names] = 
    for {
      w <- db.people
      if p(w.age)
    } yield new Record {
      val name = w.name
    }

  val thirtySomethings2 = satisfies(x => 30 <= x && x < 40)
  val evenAge = satisfies(_ % 2 == 0)

  // 2.5 Composing queries

  def ageFromName(s: Rep[String]): Rep[List[Int]] =  // paper has return type 'int' but says 'list of int' in the text (?)
    for {
      u <- db.people
      if u.name == s
    } yield u.age


  def rangeFromNames(s: Rep[String], t: Rep[String]): Rep[Names] =
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

  def P(t: Predicate)(x: Rep[Int]): Rep[Boolean] = t match {
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

  type Org = Record {
    val departments: List[Record {val dpt: String}]
    val employees: List[Record {val dpt: String; val emp: String}]
    val tasks: List[Record {val emp: String; val tsk: String}]
  }

  val org = database[Org]("org")

  /-*val org = new Record {
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
  }*-/

  def exists(xs: Rep[List[Record]]) = !xs.isEmpty // helper method

  val empty = new Record { val ignore = () }

  def expertise(u: Rep[String]): Rep[List[Record { val dpt: String }]] =
    for {
      d <- org.departments
      if !exists(
        for {
          e <- org.employees
          if d.dpt == e.dpt && !exists(
            for {
              t <- org.tasks
              if e.emp == t.emp && t.tsk == u 
            } yield empty)
        } yield empty )
    } yield new Record { val dpt = d.dpt }

  val departmentsFullOfAbstracters = expertise("abstract")

  // 3.1 Nested structures

  type NestedOrg = List[Record {
    val dpt: String
    val employees: List[Record {
      val emp: String
      val tasks: List[String]
    }]
  }]

  val nestedOrg: Rep[NestedOrg] =
    for { 
      d <- org.departments
    } yield {
      val employees1 = for {
        e <- org.employees
        if d.dpt == e.dpt
      } yield new Record {
        val emp = e.emp 
        val tasks = for {
          t <- org.tasks 
          if e.emp == t.emp
        } yield t.tsk
      }
      new Record {
        val dpt = d.dpt
        val employees = employees1  // FIXME: scalac crash with outer field if records are nested
      }
    }

  // 3.2 Higher-order queries

  def any[A:Manifest](xs: Rep[List[A]])(p: Rep[A] => Rep[Boolean]): Rep[Boolean] =
    exists(for (x <- xs if p(x)) yield empty)

  def all[A:Manifest](xs: Rep[List[A]])(p: Rep[A] => Rep[Boolean]): Rep[Boolean] =
    !any(xs)(x => !p(x))

  def contains[A:Manifest](xs: Rep[List[A]], u: Rep[A]): Rep[Boolean] =
    any(xs)(x => x == u)

  def expertise2(u: Rep[String]): Rep[List[{ val dpt: String }]] =
    for {
      d <- nestedOrg
      if all(d.employees)(e => contains(e.tasks, u)) 
    } yield new Record { val dpt = d.dpt }

  val departmentsFullOfAbstracters2 = expertise2("abstract")


  // 4 From XPath to SQl

  /-*
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
  *-/

  type Node = Record {
    val id: Int
    val parent: Int
    val name: String
    val pre: Int
    val post: Int 
  }

  val db_xml = database[Record { val nodes: List[Node]}]("xml").nodes

  /-*val db_xml = List(
    Node(0, -1, "#doc", 0, 13),
    Node(1,  0, "a",    1, 12),
    Node(2,  1, "b",    2,  5),
    Node(3,  2, "c",    3,  4),
    Node(4,  1, "d",    6, 11),
    Node(5,  4, "e",    7,  8),
    Node(6,  4, "f",    9, 10))*-/


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

  def axis(ax: Axis)(s: Rep[Node], t: Rep[Node]): Rep[Boolean] = ax match {
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

  def path(p : Path)(s: Rep[Node], u: Rep[Node]): Rep[Boolean] = p match {
    case PSeq(p, q) => 
      any(db_xml)(t => path(p)(s, t) && path(q)(t, u))
    case PAxis(ax) => 
      axis(ax)(s, u)
    case NameTest(name) =>
      s.id == u.id && s.name == name
    case Filter(p) => 
      s.id == u.id && any(db_xml)(t => path(p)(s, t))
  }

  def xpath(p : Path): Rep[List[Int]] = for {
    root <- db_xml
    s <- db_xml
    if (root.parent == -1) && path(p)(root, s)
  } yield s.id


  //  /-*-/-*
  val xp0 = PSeq(PAxis(Child), PAxis(Child))
  //  //-*-/parent::*
  val xp1 = PSeq(PAxis(Descendant), parent)
  // Q: this produces 0,1,2,4 but the paper says 1,2,4 (?)

  //  //-*[following-sibling::d]
  val xp2 = PSeq(PAxis(Descendant), Filter(PSeq(PAxis(FollowingSibling),NameTest("d"))))
  //  //f[ancestor::*-/preceding::b]()
  val xp3 = PSeq(PSeq(PAxis(Descendant), NameTest("f")), Filter(PSeq(ancestor,PSeq(preceding, NameTest("b")))))

  val xr0 = xpath(xp0)
  val xr1 = xpath(xp1)
  val xr2 = xpath(xp2)
  val xr3 = xpath(xp3)

  }
}




// internal staged implementation: IR node classes, rewrites for normalization
trait StagedExp extends Staged with ScalaOpsPkgExp with BooleanOpsExpOpt with StructExpOpt {

  // IR node representing database("name")
  case class Database[T](s: String) extends Def[T]
  def database[T:Manifest](s: String): Exp[T] = Database[T](s)

  // IR node representing for (x <- l) yield f(x)
  // we store two representations at the same time:
  // (l,f) and (db.tbl, x => block)
  // both serve different purposes:
  // - the latter is the normalized form, intensional representation,
  //   used for code generation
  // - the former is the input form, extensional representation,
  //   used to perform rewriting in NBE style ("normalization by evaluation")
  case class DBFor[A:Manifest, B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[List[B]], 
    db: String, tbl: String, x: Sym[A], block: Block[List[B]]) extends Def[List[B]]


  // some extractor objects to make pattern matching more convenient
  // (these are not strictly necessary)
  object Empty {
    def apply() = List()
    def unapply[A](x:Exp[List[A]]): Boolean = x match {
      case Def(ListNew(xs)) if xs.length == 0 => true
      case _ => false
    }
  }

  object Yield {
    def apply[A:Manifest](x:Exp[A]) = List(x)
    def unapply[A](x:Exp[List[A]]): Option[Exp[A]] = x match {
      case Def(ListNew(xs)) if xs.length == 1 => Some(xs.head)
      case _ => None
    }
  }

  object IfThen {
    def unapply[A](x:Exp[List[A]]): Option[(Exp[Boolean], Exp[List[A]])] = x match {
      case Def(IfThenElse(c,Block(a),Block(Empty()))) => Some((c,a))
      case _ => None
    }
  }

  object For {
    def unapply[B](x:Exp[List[B]]): Option[(Exp[List[Any]], Exp[Any] => Exp[List[B]])] = x match {
      case Def(DBFor(l,f,d,t,a,b)) => Some((l,f))
      case _ => None
    }
  }

  object Concat {
    def unapply[A](x:Exp[List[A]]): Option[(Exp[List[A]], Exp[List[A]])] = x match {
      case Def(ListConcat(a,b)) => Some((a,b))
      case _ => None
    }
  }


/-*
  normalized syntax:

      (SQLquery)      S ::= [] | X | X1@X2
      (collection)    X ::= database(db) | yield Y | if Z then yield Y | for x in database(db).l do X
      (record)        Y ::= x | {l=Z}
      (base)          Z ::= c | x.l | op(X) | exists S

  normalization 1:

                       (fun(x) → R) Q --> R[x:=Q] 
                             {l=Q}.li --> Qi
      
               for x in (yield Q)do R --> R[x:=Q]
      for y in (for x in P do Q) do R --> for x in P do (for y in Q do R)
          for x in (if P then Q) do R --> if P then (for x in Q do R)
                     for x in [] do N --> []
                for x in (P @ Q) do R --> (for x in P do R) @ (for x in Q do R)
                       if true then Q --> Q
                      if false then Q --> []
      
  normalization 2:

                for x in P do (Q @ R) --> (for x in P do Q) @ (for x in P do R)
                     for x in P do [] --> []
                    if P then (Q @ R) --> (if P then Q) @ (if P then R)
                         if P then [] --> []
              if P then (if Q then R) --> if (P && Q) then R
          if P then (for x in Q do R) --> for x in Q do (if P then R)
*-/



  // implement smart constructor for DBFor nodes; perform normalization.
  // note that we map IR nodes to staged code, not directly to other IR nodes.
  def dbfor[A:Manifest,B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[List[B]])(implicit pos: SourceContext): Exp[List[B]] = l match {
/-*
             for x in (yield Q)do R --> R[x:=Q]
    for y in (for x in P do Q) do R --> for x in P do (for y in Q do R)
        for x in (if P then Q) do R --> if P then (for x in Q do R)
                   for x in [] do N --> []
              for x in (P @ Q) do R --> (for x in P do R) @ (for x in Q do R)
*-/
    case Empty()              => List()
    case Yield(a)             => f(a)
    case IfThen(c,a)          => if (c) for (x <- a; y <- f(x)) yield y else List()
    case For(l2,f2)           => for (x <- l2; y <- f2(x); z <- f(y)) yield z
    case Concat(a,b)          => a.flatMap(f) ++ b.flatMap(f)
    case (Def(FieldApply(Def(Database(db)), tbl))) =>     
      val a = fresh[A]
      val b = reifyEffects(f(a))
      b match {
/-*                 for x in P do [] --> []           *-/
        case Block(Empty())              => List()
        case _ =>
          // no rewrites match, go ahead and create IR node
          reflectEffect(DBFor(l, f, db, tbl, a, b), summarizeEffects(b).star)
      }
    case (Def(ld)) => 
      // cannot normalize, report error and throw exception
      printerr("error: cannot normalize for expression")
      printerr(s"at $l=$ld")
      printsrc(s"in ${quotePos(fresh.withPos(pos::Nil))}")
      throw new MatchError(l)
  }


  // override `if (c) a else b` smart constructor to add rewrites
  override def ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: Block[T], elsep: Block[T])(implicit pos: SourceContext) = ((thenp.res,elsep.res) match {
/-*
                  if P then (Q @ R) --> (if P then Q) @ (if P then R)
                       if P then [] --> []
            if P then (if Q then R) --> if (P && Q) then R
        if P then (for x in Q do R) --> for x in Q do (if P then R)
*-/
    case (Empty(),Empty())       => List()
    case (IfThen(c,a),Empty())   => if (cond && c) a else List()
    case (For(l,f),Empty())      => for (x <- l if cond; y <- f(x)) yield y
    case (Concat(a,b),Empty())   => (if (cond) a else List()) ++ (if (cond) b else List())
    case _                       => super.ifThenElse(cond,thenp,elsep)
  }).asInstanceOf[Exp[T]]



  // override Rep[List[T]] methods to create For nodes
  override def list_flatMap[A:Manifest,B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[List[B]])(implicit pos: SourceContext) = {
    dbfor[A,B](l,f)
  }
  override def list_map[A:Manifest,B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[B])(implicit pos: SourceContext) = {
    list_flatMap[A,B](l, x => List(f(x)))
  }
  override def list_filter[A : Manifest](l: Exp[List[A]], f: Exp[A] => Exp[Boolean])(implicit pos: SourceContext) = {
    list_flatMap[A,A](l, x => if (f(x)) List(x) else List())
  }


  // implement some internal methods for new IR nodes
  override def syms(e: Any): List[Sym[Any]] = e match {
    case DBFor(_, _, _, _, _, body) => syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DBFor(_, _, _, _, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    // normally we'd want `freqHot` to hoist code out of the loop, 
    // but here we want to keep it inside for clarity
    case DBFor(_, _, _, _, _, body) => freqCold(body) 
    case _ => super.symsFreq(e)
  }

}


// code generator for specific IR nodes -- alternative impl would emit SQL
trait ScalaGenStaged extends ScalaCodeGenPkg with ScalaGenStruct {
  val IR: StagedExp
  import IR._

  override def emitFileHeader(): Unit = {
    super.emitFileHeader()
    stream.println("import scala.virtualization.lms.epfl.test14.Schema")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Database(s) => 
      emitValDef(sym, "Schema."+s)
    case DBFor(l, f, db, tbl, x, b) => 
      val sdb = "Schema."+db+"."+tbl
      stream.println("val " + quote(sym) + " = " + sdb + ".flatMap { " + quote(x) + " => ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    case Struct(tag, elems) =>
      emitValDef(sym, "new Schema.Record { " + (for ((n, v) <- elems) yield "val " + n + " = " + quote(v)).mkString("; ") + " }")
    case _ => super.emitNode(sym, rhs)
  }
}

// accessed by generated code
object Schema extends Shallow


// pretty printing for records
trait Util {
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


// test cases
class TestQueries extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test14-"
  
  trait DSL extends Staged with Compile {
    def test(): Unit
  }
  
  trait Impl extends DSL with StagedExp with ScalaCompile { self =>
    override val verbosity = 1
    dumpGeneratedCode = true
    val codegen = new Codegen { val IR: self.type = self }
    val runner = new Runner { val p: self.type = self }
    runner.run()
  }
  
  trait Codegen extends ScalaGenStaged {
    val IR: Impl
  }  
  
  trait Runner {
    val p: Impl
    def run() = {
      p.test()
    }
  }
  


  def testQueries1 = withOutFileChecked(prefix+"queries1") {
    trait Prog extends Shallow {
      def test() = {
        println("db")
        println(db)
        println(differences)
        println(thirtySomethings)
        println(thirtySomethings2)
        println(evenAge)
        println(rangeBertEdna)
        println(thirtySomethings3)
        println(thirtySomethings4)
        println(departmentsFullOfAbstracters)
        println(nestedOrg)
        println(departmentsFullOfAbstracters2)

        println(xr0)
        println(xr1)
        println(xr2)
        println(xr3)
      }
    }
    new Prog {} test
  }

  def testQueries2 = withOutFileChecked(prefix+"queries2") {
    trait Prog extends DSL with Staged {
      def test() = {

        val f = compile { x: Rep[Unit] =>
          val x = new Inner {}
          import x._

          //println(db)
          //println(differences)
          //println(thirtySomethings)
          //println(thirtySomethings2)
          //println(evenAge)
          println("rangeFromNames(\"Edna\",\"Bert\"):")
          println(rangeBertEdna)
          //println(thirtySomethings3)
          //println(thirtySomethings4)
          //println(departmentsFullOfAbstracters)
          //println(nestedOrg)
          //println(departmentsFullOfAbstracters2)

          //println(xr0)
          //println(xr1)
          //println(xr2)
          //println(xr3)


        }

        f()

      }
    }
    val o = new Prog with Impl
    //println(o.)
  }

  def testQueries3 = withOutFileChecked(prefix+"queries3") {
    trait Prog extends DSL with Staged {
      def test() = {

        val f = compile { x: Rep[Unit] =>
          val x = new Inner {}
          import x._

          //println(db)
          //println(differences)
          //println(thirtySomethings)
          //println(thirtySomethings2)
          //println(evenAge)
          //println(rangeBertEdna)
          //println(thirtySomethings3)
          //println(thirtySomethings4)
          println("expertise(\"abstract\"):")
          println(departmentsFullOfAbstracters)
          //println(nestedOrg)
          //println(departmentsFullOfAbstracters2)

          //println(xr0)
          //println(xr1)
          //println(xr2)
          //println(xr3)


        }

        f()

      }
    }
    val o = new Prog with Impl
    //println(o.)
  }
  def testQueries4 = withOutFileChecked(prefix+"queries4") {
    trait Prog extends DSL with Staged {
      def test() = {

        val f = compile { x: Rep[Unit] =>
          val x = new Inner {}
          import x._

          //println(db)
          //println(differences)
          //println(thirtySomethings)
          //println(thirtySomethings2)
          //println(evenAge)
          //println(rangeBertEdna)
          //println(thirtySomethings3)
          //println(thirtySomethings4)
          //println(departmentsFullOfAbstracters)
          //println(nestedOrg)
          println("expertise2(\"abstract\"):")
          println(departmentsFullOfAbstracters2)

          //println(xr0)
          //println(xr1)
          //println(xr2)
          //println(xr3)


        }

        f()

      }
    }
    val o = new Prog with Impl
    //println(o.)
  }

}
*/
