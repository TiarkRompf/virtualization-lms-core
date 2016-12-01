package scala.lms
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

case class RCell[T](tag: String) {
  var value: T = _
  def set(x: T) = { value = x; this }
}


trait CellOps extends Base {
  type Cell[T] = Rep[RCell[T]]
  implicit def cellTyp[T:Typ]: Typ[RCell[T]]
  def cell[T:Typ](tag: String): Cell[T]
  def infix_set[T:Typ](c: Cell[T], x: Rep[T]): Rep[Unit]
  def infix_get[T:Typ](c: Cell[T]): Rep[T]
}

trait CellOpsExp extends CellOps with BaseExp with StaticDataExp {
  implicit def cellTyp[T:Typ]: Typ[RCell[T]] = {
    implicit val ManifestTyp(m) = typ[T]
    manifestTyp
  }

  case class CellInit[T](tag: String, x: Rep[T]) extends Def[RCell[T]]
  case class CellSet[T](c: Cell[T], x: Rep[T]) extends Def[Unit]
  case class CellGet[T](c: Cell[T]) extends Def[T]
  
  def cell[T:Typ](tag: String): Cell[T] = staticData(new RCell[T](tag))//reflectMutable(CellInit(tag, x))
  def infix_set[T:Typ](c: Cell[T], x: Rep[T]): Rep[Unit] = reflectWrite(c)(CellSet(c,x))
  def infix_get[T:Typ](c: Cell[T]): Rep[T] = CellGet(c)
}

trait ScalaGenCellOps extends ScalaGenBase {
  val IR: CellOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case CellInit(tag,x) =>  emitValDef(sym, "scala.lms.epfl.test13.RCell[" + remap(x.tp) + "](\"" + tag + "\")")
    case CellSet(c,x) =>  emitValDef(sym, quote(c) + ".set(" + quote(x) + ")")
    case CellGet(c) =>  emitValDef(sym, quote(c) + ".value")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CompileDyn extends Base with Compile {
  
  def dcompile[A:Typ,B:Typ](fv: List[Rep[Any]])(f: Rep[A] => Rep[B]): Rep[A=>B]

  def dcompile[A:Typ,B:Typ](f: Rep[A] => Rep[B]): Rep[A=>B] = dcompile(freesyms(f))(f)

  def dlet[A:Typ,B:Typ](x:Rep[A], fv: List[Rep[Any]])(f: A => Rep[B]): Rep[B]

  def dlet[A:Typ,B:Typ](x:Rep[A])(f: A => Rep[B]): Rep[B] = dlet(x, freesyms(f))(f)

  def unstage[A:Typ,B:Typ](x:Rep[A])(f: A => Rep[B]): Rep[B] = dlet(x)(f)

  // TODO: @cps version of unstage

  def freesyms(x:Any): List[Rep[Any]]


}

trait CompileDynExp extends CompileDyn with BaseExp with StaticDataExp with UncheckedOpsExp {

  override def toString = "IR:" + getClass.getName
    
  def freesyms(x:Any): List[Sym[Any]] = { // switch to syms again ...
    val fields = x.getClass.getDeclaredFields
    fields.foreach(_.setAccessible(true))
    val res = fields.map(_.get(x)).collect{case x: Sym[Any] => x}.toList
    //println("free vars: " + res)
    res
  }


  def dcompile[A:Typ,B:Typ](fv: List[Exp[Any]])(f: Rep[A] => Rep[B]): Rep[A=>B] = {
    
    // compile { u: Rep[A] => f(u) }

    dcompileInternal[A,Rep[A],B](fv, (u,v) => u)(f)
  }
  
  def dlet[A:Typ,B:Typ](x:Exp[A], fv: List[Exp[Any]])(f: A => Rep[B]): Rep[B] = {
    
    // compile { u: Rep[Unit] => f(x) }  <--- x is runtime value

    val fc = dcompileInternal[Unit,A,B](x::fv, (u,v) => v.head.asInstanceOf[A])(f) // don't really want x as free var but need lower bound on sym id for fresh ones
    unchecked[B](fc,".apply(())")
  }

  def dcompileInternal[U:Typ,A,B:Typ](fv: List[Exp[Any]], g: (Rep[U],List[Any]) => A)(f: A => Rep[B]): Rep[U=>B] = {

    // will generate:  compile { u => f(g(u)) }

    // the tricky bit: we must insert all free variables as staticData, redefining the corresponding symbols
    val fvIds = fv map { case Sym(i) =>  i }
    val maxid = (0::fvIds).max + 1

    val callback = { (fvVals: List[Any]) => 
      this.reset
      this.nVars = maxid
      compile { x: Rep[U] =>
        (fv zip fvVals).foreach { case (si:Sym[_],xi) => createDefinition(si, StaticData(xi)) }
        f(g(x,fvVals))
      }
    }

    implicit val ManifestTyp(mA) = typ[U]
    implicit val ManifestTyp(mB) = typ[B]
    implicit val cbTyp: Typ[List[Any] => (U=>B)] = manifestTyp
    implicit val resTyp: Typ[U=>B] = manifestTyp

    unchecked[U=>B](staticData(callback),".apply("+fvIds.map(i=>"x"+i)+")","// compile dynamic: fv = ",fv)

    /*unchecked("{import ",IR,"._;\n",
      fvIds.map(i => "val s"+i+" = findDefinition(Sym("+i+")).map(infix_lhs(_).head).getOrElse(Sym("+i+"));\n").mkString, // XX codegen uses identity hash map ...
      IR,".reset;",IR,".nVars="+maxid+"\n",                                                      // FIXME: reset harmful ???
      "compile{(x:",atyp,") => \n",
      fvIds.map(i => "createDefinition(s"+i+",StaticData(x"+i+"));\n").mkString,
      "val y = ",f2,".asInstanceOf[",ftyp,"](",g("x"),")\n",
      "println(\"freeVars/globalDefs for function of type "+f.getClass.getName+": "+fv+"\")\n",
      "println(globalDefs)\n",
      "y}}","//",fv) // last comment item necessary for dependency*/

  }
}


trait StableVars extends CellOps with CompileDyn with Equal with PrimitiveOps with ArrayOps with Compile { self =>
    
    abstract class Continue[A]
    case class Done[A](x: Rep[A]) extends Continue[A]
    case class ReadValue[A:Typ,B](s: RCell[A], f: A => Continue[B], fv: List[Rep[Any]]) extends Continue[B] { val m = typ[A] }

    def readValue[A:Typ,B](s: RCell[A])(f: A => Rep[B]) = ReadValue(s, (x:A) => Done(f(x)), freesyms(f))
    def readOneValue[A:Typ,B](s: RCell[A])(f: A => Continue[B]) = ReadValue(s, f, freesyms(f))
    def compileStable[A:Typ,B:Typ](f: Rep[A] => Continue[B]): A=>B

  }

  trait StableVarsExp extends CellOpsExp with CompileDynExp with EffectExp with StaticDataExp with FunctionsExp with StableVars with EqualExpOpt with IfThenElseFatExp with UncheckedOpsExp {
    
    def compileStable[A:Typ,B:Typ](f: Rep[A] => Continue[B]): A=>B = {

      val codeHolder = RCell[A=>B]("code")

      def compPart[A:Typ](m: Continue[A]): Rep[A] = m match {
        case e@ReadValue(s,f:((a)=>Continue[A]), fv) => 
          implicit val m = e.m 

          val s2 = staticData(s)
          println("read value " + s + " sym " + s2)
          
          val s2val = s2.get
          if (s2val == staticData(s.value)) {
            compPart(f(s.value))
          } else {
            staticData(codeHolder).set(unit(null))
            // TODO: we're not *really* specializing the continuation yet,
            // just using s2val as static data (we should use unit(..))
            //val compiledCont = dcompile(s2val::fv)((x:Rep[a]) => compPart(f(s2val)))  // <---- should specialize this to new value!  (OSR!!)
            //println("compiled " + compiledCont)
            //doApply(compiledCont, s2val)
            // BETTER YET: have f take static arg instead of Rep
            dlet(s2val,fv)(z => compPart(f(z)))
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



class TestStable extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test13-"
  
  
  trait DSL extends VectorOps with LiftPrimitives with PrimitiveOps with OrderingOps with BooleanOps with LiftVariables 
    with IfThenElse with While with RangeOps with Print with Compile
    with ArrayOps with CastingOps with StableVars {
    
    def test(): Unit
  }
  
  trait Impl extends DSL with VectorExp with OrderingOpsExpOpt with BooleanOpsExp 
    with EqualExpOpt with IfThenElseFatExp with LoopsFatExp with WhileExp
    with RangeOpsExp with PrintExp with FatExpressions with CompileScala
    with SeqOpsExp with StringOpsExp
    with PrimitiveOpsExp with ArrayOpsExp with CastingOpsExp with StaticDataExp 
    with StableVarsExp { self =>
    override val verbosity = 1
    dumpGeneratedCode = true
    val codegen = new Codegen { val IR: self.type = self }
    val runner = new Runner { val p: self.type = self }
    runner.run()
  }
  
  trait Codegen extends ScalaGenVector with ScalaGenOrderingOps with ScalaGenBooleanOps
    with ScalaGenVariables with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhile
    with ScalaGenRangeOps with ScalaGenPrint with ScalaGenFunctions
    with ScalaGenPrimitiveOps with ScalaGenArrayOps with ScalaGenCastingOps with ScalaGenStaticData 
    with ScalaGenCellOps with ScalaGenUncheckedOps {
    val IR: Impl
  }
  
  
  trait Runner {
    val p: Impl
    def run() = {
      p.test()
    }
  }
  


  def testUnstage = withOutFileChecked(prefix+"unstage1") {
    trait Prog extends DSL with Functions with StaticData {
      def test() = {

        val f = compile { x: Rep[Int] =>

          val a = x + 1
          val b = x * 2

          // specialize continuation at runtime to value of a+b

          unstage(a+b) { y: Int =>

            val z = unit(y) * (a + b)

            z
          }
        }

        println(f(9))

        println(f(3))

        println(f(1))

      }
    }
    new Prog with Impl
  }


  
  def testStable1 = withOutFileChecked(prefix+"stable1") {
    trait Prog extends DSL with Functions with StaticData {
      def test() = {


        val s = new RCell[Int]("stable")
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


        val s = RCell[Int]("stable")

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
