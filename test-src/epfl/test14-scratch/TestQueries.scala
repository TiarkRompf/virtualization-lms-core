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

trait Schema {
  
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
    trait Prog extends DSL {
      def test() = {

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
