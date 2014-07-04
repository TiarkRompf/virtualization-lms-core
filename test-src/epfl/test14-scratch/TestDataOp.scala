package scala.virtualization.lms
package epfl
package test14

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



class TestDataOp extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test14-"
  
  trait DSL extends ScalaOpsPkg with TupledFunctions with UncheckedOps with LiftPrimitives with LiftString with LiftVariables {
    // keep track of top level functions
    case class TopLevel[A,B](name: String, mA: Manifest[A], mB:Manifest[B], f: Rep[A] => Rep[B])
    val rec = new scala.collection.mutable.HashMap[String,TopLevel[_,_]]
    def toplevel[A:Manifest,B:Manifest](name: String)(f: Rep[A] => Rep[B]): Rep[A] => Rep[B] = {
      val g = (x: Rep[A]) => unchecked[B](name,"(",x,")")
      rec.getOrElseUpdate(name, TopLevel(name, manifest[A], manifest[B], f))
      g
    }
    val L = scala.List

    case class Record(fields: (String, Rep[Int])*) {
      def apply(k: String) = fields.toMap.apply(k)
      def toStrings: Seq[Rep[Any]] =
        fields.flatMap { case (f,x) => unit(f)::x::Nil }
    }

    case class Schema(fields: String*)

    abstract class Table {
      def schema: Schema
      def size: Rep[Int]
      def incr: Rep[Int]
      def get(pos: Rep[Int]): Record
    }

    def loadData(s:String): Rep[Array[Int]]

    case class RowBasedTable(name: String, schema: Schema) extends Table {
      val data = loadData(name + ".dat")
      val size = data.length
      val incr = schema.fields.length: Rep[Int]
      def get(pos: Rep[Int]) = Record(
        schema.fields.zipWithIndex map { case (f,i) => (f, data(pos+i)) }: _*
      )
    }

    case class ColBasedTable(name: String, schema: Schema) extends Table {
      val data = schema.fields.map(f => (f,loadData(name + "_" + f + ".dat")))
      val size = data.head._2.length
      val incr = unit(1)
      def get(pos: Rep[Int]) = Record(
        schema.fields.map(f => (f, data.toMap.apply(f)(pos))): _*
      )
    }



    abstract class Operator {
      def open()
      def next(): Rep[Boolean]
      def elem(): Record
    }


    case class ScanOp(table: Table) extends Operator {

      private val pos = var_new(0)

      def open() = { pos = 0 } // first elem?
      def next() = {
        val more = pos < table.size
        if (more) pos += table.incr
        more
      }
      def elem(): Record = table.get(pos)
    }

    case class FilterOp(f: Record => Rep[Boolean])(up: Operator) extends Operator {

      def open() = up.open()
      def next() = {
        var more = up.next()
        // ?? up.next()

        def cont = if (more) !f(up.elem) else unit(false)
        while (cont) {
          more = up.next()
        }
        more
      }
      def elem() = up.elem()
    }

    case class MapOp(f: Record => Record)(up: Operator) extends Operator {

      def open() = up.open()
      def next() = up.next()
      def elem() = f(up.elem())
    }

    case class JoinOp(k1: Record => Rep[Int], k2: Record => Rep[Int])(up1: Operator, up2: Operator) extends Operator {
      // **** preliminary ****
      val more1 = var_new(unit(false))

      def open() = { up1.open(); more1 = up1.next(); up2.open() }
      def next() = {
        var search = true
        def cont1 = if (more1) search else unit(false)
        while (cont1) {
          var more2 = up2.next()
          def cont = if (more2) (k1(up1.elem) != k2(up2.elem)) else unit(false)
          while (cont) {
            more2 = up2.next()
          }

          if (!more2) {
            more1 = up1.next()
            up2.open()
          } else {
            search = false //found
          }
        }
        more1
      }
      def elem() = {
        Record((up1.elem().fields ++ up2.elem.fields): _*)
      }

    }



    case class PrintOp(up: Operator) extends Operator {
      def open() = {
        up.open()
        while (up.next) {
          val rec = up.elem()
          val format = rec.fields.map(_._1 + ": %d").mkString(", ") + "\n"
          val data = rec.fields.map(_._2)
          printf(format, data:_*)
        }
      }
      def next() = unit(false)
      def elem() = Record()
    }


  }

  trait Impl extends DSL with ScalaOpsPkgExp with VariablesExpOpt with TupledFunctionsRecursiveExp with UncheckedOpsExp { self => 
    val codegen = new CCodeGenPkg with CGenVariables with CGenTupledFunctions with CGenUncheckedOps { 
      val IR: self.type = self 
      override def remap[A](a: Manifest[A]) = 
        if (a == manifest[Array[Int]]) "int*"
        else super.remap(a)
      override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
        case LoadData(x)      => emitValDef(sym, s"mmap('$x') // XX TODO ")
        case ArrayApply(xs,i) => emitValDef(sym, s"${quote(xs)}[${quote(i)}]") // should fix in ArrayOps
        case _ => super.emitNode(sym,rhs)
      }
    }


    def loadData(s:String): Rep[Array[Int]] = LoadData(s)
    case class LoadData(s:String) extends Def[Array[Int]]


    def emitAll(): Unit = {
      assert(codegen ne null) //careful about initialization order
      rec.foreach { case (k,x) =>
        val stream = new PrintWriter(System.out)
        stream.println("/* FILE: " + x.name + ".c */")
        for ((_,v) <- rec) codegen.emitForwardDef(mtype(v.mA)::Nil, v.name, stream)(mtype(v.mB))
        codegen.emitSource1(x.f, x.name, stream)(mtype(x.mA), mtype(x.mB))
      }
    }
    emitAll()
  }

  
  def testDataOp1 = {
    withOutFile(prefix+"dataop1") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[Int] =>

          val table = ColBasedTable("table",Schema("field1","field2","field3"))

          val plan = 
            PrintOp(
              MapOp(rec => Record("foo" -> rec("field2"), "bar" -> 2 * rec("field1")))(
                ScanOp(table)))

          plan.open()

          0
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"dataop1")
  }

  def testDataOp2 = {
    withOutFile(prefix+"dataop2") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[Int] =>

          val table = RowBasedTable("table",Schema("field1","field2","field3"))

          val plan = 
            PrintOp(
              MapOp(rec => Record("foo" -> rec("field2"), "bar" -> 2 * rec("field1")))(
                FilterOp(rec => rec("field2") > 0)(
                  ScanOp(table))))

          plan.open()

          0
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"dataop2")
  }

  def testDataOp3 = {
    withOutFile(prefix+"dataop3") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[Int] =>

          val tableA = RowBasedTable("A",Schema("a1","a2"))
          val tableB = RowBasedTable("B",Schema("b1","b2"))
          val tableC = RowBasedTable("C",Schema("c1","c2"))

          val plan = 
            PrintOp(
              JoinOp(_ apply "a2", _ apply "b1")(
                ScanOp(tableA),
                JoinOp(_ apply "b2", _ apply "c1")(
                  ScanOp(tableB),
                  ScanOp(tableC))))

          plan.open()

          0
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"dataop3")
  }


}