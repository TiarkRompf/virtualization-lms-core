package scala.lms
package epfl
package test9

import common._
import internal.{FatExpressions,GenericFatCodegen}


import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ArrayLoopsFatExp,ScalaGenArrayLoops,ScalaGenFatArrayLoopsFusionOpt}
import scala.reflect.SourceContext

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait ComplexArith extends PrimitiveOps with ComplexBase with OverloadHack {
  
  def infix_+(x: Rep[Complex], y: Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re + y.re, x.im + y.im)
  def infix_-(x: Rep[Complex], y: Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re - y.re, x.im - y.im)
  //def infix_*(x: Rep[Complex], y: Rep[Complex]): Rep[Complex] = Complex(x.re + y.re, x.im + y.im)
  
}

trait ComplexBase extends StructOps {
  type Complex = Record { val re: Double; val im: Double }
  def Complex(r: Rep[Double], i: Rep[Double]): Rep[Complex] = new Record { val re = r; val im = i }
}

// ------ struct impl follows, will move to common once stable

trait StructExpOptLoops extends StructExpOptCommon with ArrayLoopsExp with PrimitiveOpsExp {
  
  case class ArraySoaTag[T](base: StructTag[T], len: Exp[Int]) extends StructTag[T]
  
  override def simpleLoop[A:Typ](size: Exp[Int], v: Sym[Int], body: Def[A])(implicit pos: SourceContext): Exp[A] = body match {
    case ArrayElem(Block(Def(Struct(tag:StructTag[A], elems)))) => 
      struct[A](ArraySoaTag[A](tag,size), elems.map(p=>(p._1,simpleLoop(size, v, ArrayElem(Block(p._2)))(p._2.tp.arrayTyp, pos))))
    case ArrayElem(Block(Def(ArrayIndex(b,`v`)))) if infix_length(b)(mtype(ManifestTyp[Any](implicitly)/*FIXME:wrong type!*/)) == size => b.asInstanceOf[Exp[A]] 
    // eta-reduce! <--- should live elsewhere, not specific to struct
    // rewrite loop(a.length) { i => a(i) } to a
    case _ => super.simpleLoop(size, v, body)
  }
  
  
  override def infix_at[T:Typ](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = a match {
    case Def(Struct(ArraySoaTag(tag,len),elems: Iterable[(String,Rep[Array[T]])])) =>
      def unwrap[A](m:Typ[Array[A]]): Typ[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ =>
          if (m.isArray) mtype(simpleClassTyp(m.runtimeClass.getComponentType))
          else { printerr("warning: expect type Array[A] but got "+m); mtype(ManifestTyp[Any](implicitly)) }
      }
      struct[T](tag.asInstanceOf[StructTag[T]], elems.map(p=>(p._1,infix_at(p._2, i)(unwrap(p._2.tp)))))
    case _ => super.infix_at(a,i)
  }
  
  override def infix_length[T:Typ](a: Rep[Array[T]]): Rep[Int] = a match {
    case Def(Struct(ArraySoaTag(tag,len),elems)) => len
    case _ => super.infix_length(a)
  }

}

// ----- test cases


class TestStruct extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test9-"
  
  trait DSL extends ComplexArith with ArrayLoops with PrimitiveOps with OrderingOps with Variables with LiftVariables with LiftPrimitives with IfThenElse with RangeOps with Print {
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
    def test(x: Rep[Int]): Rep[Unit]
  }

  trait Impl extends DSL with StructExp with ArrayLoopsExp with StructExpOptLoops with PrimitiveOpsExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 1
    val codegen = new ScalaGenArrayLoops with ScalaGenStruct with ScalaGenPrimitiveOps with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self }

  /*override def fresh[T:Typ]: Sym[T] = Sym[T] { 
    if (nVars < 3) {
      System.out.println(nVars)
      (new Exception).printStackTrace
    }

    nVars += 1; nVars -1 
  }*/

    {
      val x = fresh[Int]
      val y = reifyEffects(test(x))
      //globalDefs.foreach(Console.println _)
      codegen.emitSource(List(x),y, "Test", new PrintWriter(System.out))
      codegen.emitDataStructures(new PrintWriter(System.out))
    }
  }

  trait ImplFused extends DSL with StructExp with StructExpOptLoops with StructFatExpOptCommon with ArrayLoopsFatExp with PrimitiveOpsExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 1
    val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenFatStruct with ScalaGenPrimitiveOps with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self;
        override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
    {
      val x = fresh[Int]
      val y = reifyEffects(test(x))
      //globalDefs.foreach(Console.println _)
      codegen.emitSource(List(x),y, "Test", new PrintWriter(System.out))
      codegen.emitDataStructures(new PrintWriter(System.out))
    }
  }

  
  
  def testStruct1 = {
    withOutFile(prefix+"struct1") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = Complex(x.toDouble, 0.0)
          c = c + Complex(0.0,x.toDouble)
          print(c)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"struct1")
  }

  def testStruct2 = {
    withOutFile(prefix+"struct2") {
      // test basic struct flattening (loops, variables, conditionals)
      println("REMARK: this makes only sense with fat codegen (computation duplicated and some structs not removed otherwise)")
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          // split loops (rely on fusion, don't want to duplicate computation!)

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          var vvar = vector2

          // split conditionals (be careful about effects)

          val vector3 = if (x > 7) vector1 else vvar

          // conditional is reflected because it reads vvar -- effect ordering for split terms?

          vvar = vector1

          print(vvar)
          print(vector3)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"struct2")
  }

  def testStruct2b = {
    withOutFile(prefix+"struct2b") {
      // test basic struct flattening (loops, variables, conditionals)
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          // split loops (rely on fusion, don't want to duplicate computation!)

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          var vvar = vector2

          // split conditionals (be careful about effects)

          val vector3 = if (x > 7) { 
            print("foobar true")
            vector1
          } else {
            print("foobar false")
            vvar 
          }

          vvar = vector1

          print(vvar)
          print(vector3)
        }
      }
      new Prog with ImplFused
    }
    assertFileEqualsCheck(prefix+"struct2b")
  }

  def testStruct3 = {
    withOutFile(prefix+"struct3") {
      // fuse conjugate computation with construction, essentially a no-op
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }

          def conj(c: Rep[Complex]) = Complex(c.re, 0.0 - c.im)
          def infix_map[A:Typ,B:Typ](c: Rep[Array[A]], f: Rep[A] => Rep[B]) = array(c.length) { i => f(c.at(i)) }

          val vector3 = vector1.map(conj)

          print(vector3)
        }
      }
      new Prog with ImplFused {
        // TODO: use a generic Opt trait instead of defining rewrites here...
        override def double_minus(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
          case (x, Def(DoubleMinus(Const(0.0),y))) => infix_+(x,y)
          case _ => super.double_minus(x,y)
        }
        override def double_plus(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
          case (Const(0.0), y) => y
          case _ => super.double_plus(x,y)
        }
      }
    }
    assertFileEqualsCheck(prefix+"struct3")
  }

  def testStruct4 = {
    withOutFile(prefix+"struct4") {
      trait Prog extends DSL {
        // recognize that only imaginary part is modified, real part untouched
        def test(x: Rep[Int]) = {

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          def conj(c: Rep[Complex]) = Complex(c.re, 0.0 - c.im)
          def infix_map[A:Typ,B:Typ](c: Rep[Array[A]], f: Rep[A] => Rep[B]) = array(c.length) { i => f(c.at(i)) }

          var vvar = vector1 // force access outside conditional, otherwise construction will be moved inside, defeating purpose of test

          // result of this conditional should be a *single* array 
          // containing the flattened im fields. re fields should be
          // unconditional.
          val vector3 = if (x > 7) {
            vector1.map(conj)
          } else {
            vector1
          }

          print(vector3)
        }
      }
      new Prog with ImplFused
    }
    assertFileEqualsCheck(prefix+"struct4")
  }

  // Two classes are generated if the refined type’s fields have the same type but different names
  def testStruct5 = {
    withOutFile(prefix+"struct5") {

      trait Vectors extends StructOps {
        type Vector2D = Record { val x: Double; val y: Double }
        def Vector2D(px: Rep[Double], py: Rep[Double]): Rep[Vector2D] = new Record { val x = px; val y = py }
      }

      trait Prog extends DSL with Vectors {
        def test(x: Rep[Int]) = {
          print(Vector2D(1, 2))
          print(Complex(3, 4))
        }
      }

      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"struct5")
  }

  // Only one class is generated if refined types are equivalent (their fields have the same names and types)
  def testStruct6 = {
    withOutFile(prefix+"struct6") {

      trait Complex2 extends PrimitiveOps with StructOps {
        type Complex2 = Record { val re: Double; val im: Double }
        def Complex2(r: Rep[Double], i: Rep[Double]): Rep[Complex2] = new Record { val re = r; val im = i }
      }

      trait Prog extends DSL with Complex2 {
        def test(x: Rep[Int]) = {
          print(Complex2(1, 2))
          print(Complex(3, 4))
        }
      }

      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"struct6")
  }
}
