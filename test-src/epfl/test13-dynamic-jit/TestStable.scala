package scala.virtualization.lms
package epfl
package test13

import common._
import internal._
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}
import test8._
import test10._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

case class SCell[T](tag: String) {
  var value: T = _
  def set(x: T) = { value = x; this }
}



class TestStable extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test13-"
  
  
  trait StableVars extends Equal with NumericOps with HashMapOps with ArrayOps with Compile { self =>
    
    abstract class Continue[A]
    case class Done[A](x: Rep[A]) extends Continue[A]
    case class ReadValue[A:Manifest,B](s: SCell[A], f: Rep[A] => Continue[B], fv: List[Rep[Any]]) extends Continue[B] { val m = manifest[A] }

    def readValue[A:Manifest,B](s: SCell[A])(f: Rep[A] => Rep[B]) = ReadValue(s, (x:Rep[A]) => Done(f(x)), freesyms(f))
    def compileStable[A:Manifest,B:Manifest](f: Rep[A] => Continue[B]): A=>B 

    def dcompile[A:Manifest,B:Manifest](fv: List[Rep[Any]])(f: Rep[A] => Rep[B]): Rep[A=>B]

    def dcompile[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = dcompile(freesyms(f))(f)

    def freesyms(x:Any): List[Rep[Any]]

    type Cell[T] = Rep[SCell[T]]
    def infix_set[T:Manifest](c: Cell[T], x: Rep[T]): Rep[Unit]
    def infix_get[T:Manifest](c: Cell[T]): Rep[T]
  }

  trait StableVarsExp extends EffectExp with StaticDataExp with FunctionsExp with StableVars with EqualExpOpt with IfThenElseFatExp with UncheckedOpsExp {
    
    import scala.collection.mutable.HashMap
    
    override def toString = "IR:" + getClass.getName

    case class CellInit[T](tag: String, x: Rep[T]) extends Def[SCell[T]]
    case class CellSet[T](c: Cell[T], x: Rep[T]) extends Def[Unit]
    case class CellGet[T](c: Cell[T]) extends Def[T]
    
    def cell[T:Manifest](tag: String): Cell[T] = staticData(new SCell[T](tag))//reflectMutable(CellInit(tag, x))
    def infix_set[T:Manifest](c: Cell[T], x: Rep[T]): Rep[Unit] = reflectWrite(c)(CellSet(c,x))
    def infix_get[T:Manifest](c: Cell[T]): Rep[T] = CellGet(c)

    
    def freesyms(x:Any): List[Sym[Any]] = { // switch to syms again ...
      val fields = x.getClass.getDeclaredFields
      fields.foreach(_.setAccessible(true))
      val res = fields.map(_.get(x)).collect{case x: Sym[Any] => x}.toList
      println("free vars: " + res)
      res
    }


    def dcompile[A:Manifest,B:Manifest](fv: List[Exp[Any]])(f: Rep[A] => Rep[B]): Rep[A=>B] = {
      
      // the tricky bit: we must insert all free variables as staticData, redefining the corresponding symbols
      val fvIds = fv map { case Sym(i) =>  i }
      val maxid = (0::fvIds).max + 1
      val p = staticData[Compile with StaticDataExp](this)
      val f2 = staticData(f.asInstanceOf[AnyRef])
      unchecked("{import ",p,"._;\n",
        fvIds.map(x => "val s"+x+" = infix_lhs(findDefinition(Sym("+x+")).get).head;\n").mkString, // XX codegen uses identity hash map ...
        "reset;",p,".nVars="+maxid+";compile{(x:Rep[",manifest[A],"]) => \n",                         // FIXME: reset harmful ???
        fvIds.map(x => "createDefinition(s"+x+",StaticData(x"+x+"));\n").mkString,
        "val r = ",f2,".asInstanceOf[Rep[",manifest[A],"]=>Rep[",manifest[B],"]](x)\n",
        "println(globalDefs); r}}","//",fv) // last comment item necessary for dependency

      /*raw"""{import $p._
        ${ fvIds.map(x => "val s"+x+" = infix_lhs(findDefinition(Sym("+x+")).get).head;\n").mkString } // XX codegen uses identity hash map ...
        reset;$p.nVars="+maxid+";compile{(x:Rep[",manifest[A],"]) => \n",                          // FIXME: reset harmful ???
        fvIds.map(x => "createDefinition(s"+x+",StaticData(x"+x+"));\n").mkString,
        "val r = ",f2,".asInstanceOf[Rep[",manifest[A],"]=>Rep[",manifest[B],"]](x)\n",
        "println(globalDefs); r}}","//",$fv""".as) // last comment item necessary for dependency*/
      
    }
      
    
    def compileStable[A:Manifest,B:Manifest](f: Rep[A] => Continue[B]): A=>B = {

      val codeHolder = SCell[A=>B]("code")

      def compPart[A:Manifest](m: Continue[A]): Rep[A] = m match {
        case e@ReadValue(s,f:(Rep[a]=>Continue[A]), fv) => 
          implicit val m = e.m 

          val s2 = staticData(s)
          println("read value " + s + " sym " + s2)
          
          val s2val = s2.get
          if (s2val == s.value) {
            compPart(f(unit(s.value)))
          } else {
            staticData(codeHolder).set(unit(null))
            // TODO: we're not *really* specializing the continuation yet,
            // just using s2val as static data (we should use unit(..))
            val compiledCont = dcompile(s2val::fv)((x:Rep[a]) => compPart(f(s2val)))  // <---- should specialize this to new value!  (OSR!!)
            println("compiled " + compiledCont)
            doApply(compiledCont, s2.get) 
          }

        case Done(c) => c
      }
      
      { x: A => 
        println("call with arg " + x)
        if (codeHolder.value eq null) {
          println("(re) compiling")
          codeHolder.value = compile((x:Rep[A]) => compPart(f(x)))
        }
        val g = codeHolder.value
        g(x)
      }
        

    }

  }
  
  trait ScalaGenStableVars extends ScalaGenBase with ScalaGenUncheckedOps {
    val IR: StableVarsExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case CellInit(tag,x) =>  emitValDef(sym, "scala.virtualization.lms.epfl.test12.SCell[" + remap(x.tp) + "](\"" + tag + "\")")
      case CellSet(c,x) =>  emitValDef(sym, quote(c) + ".set(" + quote(x) + ")")
      case CellGet(c) =>  emitValDef(sym, quote(c) + ".value")
      case _ => super.emitNode(sym, rhs)
    }
  }
  
  
  trait DSL extends VectorOps with Arith with OrderingOps with BooleanOps with LiftVariables 
    with IfThenElse with While with RangeOps with Print with Compile with NumericOps 
    with ArrayOps with HashMapOps with CastingOps with StableVars {
    
    def test(): Unit
  }
  
  trait Impl extends DSL with VectorExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
    with EqualExpOpt with IfThenElseFatExp with LoopsFatExp with WhileExp
    with RangeOpsExp with PrintExp with FatExpressions with CompileScala
    with NumericOpsExp with ArrayOpsExp with HashMapOpsExp with CastingOpsExp with StaticDataExp 
    with StableVarsExp { self =>
    override val verbosity = 1
    dumpGeneratedCode = true
    val codegen = new Codegen { val IR: self.type = self }
    val runner = new Runner { val p: self.type = self }
    runner.run()
  }
  
  trait Codegen extends ScalaGenVector with ScalaGenArith with ScalaGenOrderingOps with ScalaGenBooleanOps
    with ScalaGenVariables with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhile
    with ScalaGenRangeOps with ScalaGenPrint with ScalaGenFunctions
    with ScalaGenNumericOps with ScalaGenArrayOps with ScalaGenHashMapOps with ScalaGenCastingOps with ScalaGenStaticData 
    with ScalaGenStableVars {
    val IR: Impl
  }
  
  
  trait Runner {
    val p: Impl
    def run() = {
      p.test()
    }
  }
  
  
  def testStable1 = withOutFileChecked(prefix+"stable1") {
    trait Prog extends DSL with Functions with StaticData {
      def test() = {


        val s = new SCell[Int]("stable")
        s.value = 0

        val f = compile { x: Rep[Int] =>

          val a = x + 1
          val b = x * 2

          // we need to pass them explicitly: lambda lifting

          val g = dcompile { y : Rep[Int] =>

            val z = y * (a + b)

            z
          }

          doApply(g, staticData(s).get)
        }

        s.value = 1

        println(f(9))

        s.value = 5

        println(f(9))

        s.value = 2

        println(f(9))


      }
    }
    new Prog with Impl
  }





  def testStable2 = withOutFileChecked(prefix+"stable2") {
    trait Prog extends DSL {
      def test() = {


        val s = SCell[Int]("stable")

        s.value = 0

        val f = compileStable { x: Rep[Int] =>

          val a = x + 1
          val b = x * 2

          // specialize to the value of s when first run
          // next time, if s has changed:
          //  - recompile the continuation, specializing to new value, branch there
          //  - throw away compiled code for outer function, so it will be recompiled next time

          readValue(s) { y =>

            val z = y * (a + b)

            z
          }
        }

        s.value = 1

        println(f(9)) // triggers first full compilation (specialized to s = 1)

        s.value = 5

        println(f(9)) // find out s has changed, triggers OSR compilation, invalidates compiled method

        s.value = 2

        println(f(9)) // triggers second full compilation (specialized to s = 2)


      }
    }
    new Prog with Impl
  }



}
