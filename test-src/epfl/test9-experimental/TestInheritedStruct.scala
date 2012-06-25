package scala.virtualization.lms
package epfl
package test9

import common._
import internal.{FatExpressions,GenericFatCodegen,GenericNestedCodegen}


import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ArrayLoopsFatExp,ScalaGenArrayLoops,ScalaGenFatArrayLoopsFusionOpt}
import scala.reflect.SourceContext

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


/******* Null types *******/
trait NullOps extends Base{
  def Null[T:Manifest] : Rep[T]
}

trait NullOpsExp extends NullOps with EffectExp{
  case class NullGuy[T]() extends Def[T]
  def Null[T:Manifest] : Exp[T] = NullGuy[T]()
}

trait ScalaGenNullOps extends GenericNestedCodegen{
  val IR: NullOpsExp
  import IR._

  //quick hack for getting 0 as a null for doubles
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NullGuy() => emitValDef(sym, "0")
    case _ => super.emitNode(sym, rhs)
  }
}
    

trait ComplexInheritArith extends Arith with ComplexInheritBase with OverloadHack {
  def infix_+(x: Rep[Complex], y: Rep[Complex]): Rep[Complex] = {
   val xCart : Rep[CartesianComplex] = x.toCartesian
   val yCart : Rep[CartesianComplex]= y.toCartesian
   CartesianComplex(xCart.re + yCart.re, xCart.im + yCart.im) 
  }
//  def infix_-(x: Rep[Complex], y:0 Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re - y.re, x.im - y.im)
  //def infix_*(x: Rep[Complex], y: Rep[Complex]): Rep[Complex] = Complex(x.re + y.re, x.im + y.im)
  
}

trait ComplexInheritBase extends Arith with MathOps with CastingOps with IfThenElse{

  abstract class Complex
  class CartesianComplex extends Complex
  class PolarComplex extends Complex
  class SubPolarComplex extends PolarComplex

  //constructor
  def CartesianComplex(re: Rep[Double], im: Rep[Double]) : Rep[CartesianComplex]
  def infix_re(c: Rep[CartesianComplex]) : Rep[Double]
  def infix_im(c: Rep[CartesianComplex]) : Rep[Double]

  def PolarComplex(r: Rep[Double], phi: Rep[Double]): Rep[PolarComplex]
  def infix_r(c: Rep[PolarComplex]) : Rep[Double]
  def infix_phi(c: Rep[PolarComplex]) : Rep[Double]

  def SubPolarComplex(r: Rep[Double], phi: Rep[Double]): Rep[SubPolarComplex]

  def infix_toCartesian(c: Rep[Complex]): Rep[CartesianComplex] = {
      if(c.IsInstanceOf[CartesianComplex]) {
       c.AsInstanceOf[CartesianComplex]
      }else /*if(c.IsInstanceOf[PolarComplex])*/ {
        val p = c.AsInstanceOf[PolarComplex]
        CartesianComplex(p.r * math_cos(p.phi), p.r * math_sin(p.phi))
      }/*else{
        //throw an exception at some point?
        CartesianComplex(0, 0)
      }*/
  }
}

trait SuperIfThenElseExp extends IfThenElseExpOpt with BooleanOpsExp with EqualExp

//the exps
//mixing with Complex base is hopefully temporary
trait StructInheritanceExp extends StructExpOpt with CastingOpsExp with SuperIfThenElseExp with MathOpsExp with VariablesExp{

  case class MyClassTag[T](mT: Manifest[T]) extends StructTag[T]

  /**
   * need a separate class for identifying ifThenElse tags
   * treating them is different for instanceOf tests (where code needs
   * to be generated)
   * @param mT: the manifest "thought" to be the type of the ifThenElseStruct
   * @param mL: manifest of the then branch
   * @param mR: manifest of the else branch
   */
  case class LubTag[T,L,R](mT: Manifest[T], mL: Manifest[L], mR: Manifest[R]) extends StructTag[T]

  //in a copy-paste style!
  def deReiify[T:Manifest](a: Block[T]): (Block[Unit], Rep[T]) = a match { // take Reify(stms, e) and return Reify(stms, ()), e
    case Block(Def(Reify(x,es,u))) => (Block(Reify(Const(()), es, u)), x)
    case Block(x) => (Block(Const(())), x)
  }

  override def rep_isinstanceof[A,B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext) = lhs match {
      case Def(SimpleStruct(MyClassTag(mT), _)) =>
        unit(mT <:< mB)
      case Def(SimpleStruct(LubTag(_, _, _), elems)) =>
        val aField = elems("clzz").asInstanceOf[Rep[Manifest[_]]]
        //TODO: replace with generation of <:< test
        val myequality = equals(aField,unit(mB.erasure))
        myequality

      case _ => super.rep_isinstanceof(lhs, mA, mB)
  }

  override def rep_asinstanceof[A, B:Manifest](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext): Exp[B] = lhs match {
      case scls@Def(SimpleStruct(sc@MyClassTag(mSc),elems)) => 
        scls.asInstanceOf[Exp[B]]
      //if in presence of a lub tag, instantiate a MyClassTag
      case Def(SimpleStruct(LubTag(mT, mL, mR),elems)) =>
          assert(mB <:< mL || mB <:< mR)
          struct[B](MyClassTag[B](mB), elems + ("clzz" -> unit(mB)))
      case _ => 
        super.rep_asinstanceof(lhs, mA, mB)
  }
}

trait ComplexInheritStructExp extends ComplexInheritBase with StructInheritanceExp{

  def CartesianComplex(re: Exp[Double], im: Exp[Double]) = {
    //val superclassTag = MyClassTag[Complex](manifest[Complex]) 
    val cartesianTag = MyClassTag[CartesianComplex](manifest[CartesianComplex])
    //val subclassTag = SubclassTag[CartesianComplex](cartesianTag, List(superclassTag), manifest[CartesianComplex])

    struct[CartesianComplex](cartesianTag, Map("clzz"-> unit(manifest[CartesianComplex]), "re"->re, "im"->im))
  }
  def infix_re(c: Exp[CartesianComplex]): Exp[Double] = field[Double](c, "re")
  def infix_im(c: Exp[CartesianComplex]): Exp[Double] = field[Double](c, "im")

  //Polar complex numbers
  def PolarComplex(r: Exp[Double], phi: Exp[Double]) = {
    //val superclassTag = MyClassTag[Complex](manifest[Complex]) 
    val polarTag = MyClassTag[PolarComplex](manifest[PolarComplex])
    //val subclassTag = SubclassTag[PolarComplex](polarTag, List(superclassTag), manifest[PolarComplex])

    struct[PolarComplex](polarTag, Map("clzz"-> unit(manifest[PolarComplex]), "r"->r, "phi"->phi))
  } 
  
  def infix_r(c: Exp[PolarComplex]): Exp[Double] = field[Double](c, "r")
  def infix_phi(c: Exp[PolarComplex]): Exp[Double] = field[Double](c, "phi")

  //subPolar complex numbers
  def SubPolarComplex(r: Exp[Double], phi: Exp[Double]) = {
    //val superclassTag = MyClassTag[Complex](manifest[Complex]) 
    //val parentclassTag = MyClassTag[PolarComplex](manifest[PolarComplex])
    val subpolarTag = MyClassTag[SubPolarComplex](manifest[SubPolarComplex])
    //val subclassTag = SubclassTag[SubPolarComplex](subpolarTag, List(parentclassTag, superclassTag), manifest[SubPolarComplex])

    struct[SubPolarComplex](subpolarTag, Map("clzz" -> unit(manifest[SubPolarComplex]), "r"->r, "phi"->phi))
  }

}

// ------ struct impl follows, will move to common once stable
trait StructInheritExpOptLoops extends StructExpOptCommon with StructInheritanceExp with ArrayLoopsExp with NullOpsExp{
  
  case class ArraySoaTag[T](base: StructTag[T], len: Exp[Int]) extends StructTag[T]

  //should assume a topmost type like "Object"
  private def lub(hierarchy1: List[MyClassTag[Any]], hierarchy2: List[MyClassTag[Any]]) : List[MyClassTag[Any]] = {
    def inner(temp: List[MyClassTag[Any]], h1: List[MyClassTag[Any]], h2: List[MyClassTag[Any]]): List[MyClassTag[Any]] = {
      (h1, h2) match {
        case (x::xs, y::ys) => if(x ==y) inner(x::temp, xs, ys) else return temp
        case _ => temp
      }
    }
    inner(hierarchy1.reverse.head::Nil, hierarchy1.reverse.tail, hierarchy2.reverse.tail)
  }

  override def ifThenElse[T:Manifest](cond: Exp[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = {
    (deReiify(a),deReiify(b)) match {
      case ((u, Def(s1@Struct(tagA, elemsA))), (v, Def(Struct(tagB, elemsB)))) =>
        //match the tags, which can now be subclasses
        //the superclass is abstract, and has no keySet 
        (tagA, tagB) match {
          case (MyClassTag(c1), MyClassTag(c2)) => 
            assert(c1 <:< manifest[T] && c2 <:< manifest[T])

            //val hierarchy1 = (c1::parents1).asInstanceOf[List[MyClassTag[Any]]]
            //val hierarchy2 = (c2::parents2).asInstanceOf[List[MyClassTag[Any]]]

            //find the least upperbound
            //val commonAncestry: List[MyClassTag[Any]] = lub(hierarchy1, hierarchy2) 
            val commonClass = if(c1.equals(manifest[T]) || c2.equals(manifest[T]))
            MyClassTag(manifest[T])
            else LubTag(manifest[T], c1, c2)
            //val commonClass:StructTag[_] = commonAncestry match {
            //  case x::Nil => x
            //  case (x@MyClassTag(mfest))::xs => SubclassTag(x, xs, mfest) 
            //}

            //merge the keyset of c1 and c2
            val elemsNewkeys = elemsA.keySet union elemsB.keySet
            val elemsNew = for (k <- elemsNewkeys) yield (
              k -> ifThenElse(cond, Block(elemsA.getOrElse(k, Null)), Block(elemsB.getOrElse(k, Null))) //TODO : add sourcecontext
            )
            struct(commonClass, elemsNew.toMap)

          //TODO: think about nested ifThenElse for structs
          //case(LubTag(_,_,_), LubTag(_,_,_)) => 
          case _ => super.ifThenElse(cond, a, b)
      }
    case _ => super.ifThenElse(cond, a, b)
  }}

override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case ArrayElem(Block(Def(Struct(tag:StructTag[A], elems)))) => 
      struct[A](ArraySoaTag[A](tag,size), elems.map(p=>(p._1,simpleLoop(size, v, ArrayElem(Block(p._2)))(p._2.tp.arrayManifest))))
    case ArrayElem(Block(Def(ArrayIndex(b,`v`)))) if infix_length(b) == size => b.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
    case _ => super.simpleLoop(size, v, body)
  }
  
 /* override def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = a match {
    case Def(Struct(ArraySoaTag(tag,len),elems:Map[String,Exp[Array[T]]])) =>
      def unwrap[A](m:Manifest[Array[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ =>
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: expect type Array[A] but got "+m); mtype(manifest[Any]) }
      }
      struct[T](tag.asInstanceOf[StructTag[T]], elems.map(p=>(p._1,infix_at(p._2, i)(unwrap(p._2.tp)))))
    case _ => super.infix_at(a,i)
  }
  
  override def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int] = a match {
    case Def(Struct(ArraySoaTag(tag,len),elems:Map[String,Exp[Array[T]]])) => len
    case _ => super.infix_length(a)
  }*/

}

// ----- test cases
class TestInheritedStruct extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends ComplexInheritArith with ArrayLoops with Arith with OrderingOps with Variables with LiftVariables with IfThenElse with RangeOps with Print {
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
    def test(x: Rep[Int]): Rep[Any]
  }

  trait Impl extends DSL with ComplexInheritStructExp with ArrayLoopsExp with StructInheritExpOptLoops with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 1
    //import util.SymbolDependency
    val codegen = new ScalaGenArrayLoops with ScalaGenStruct with ScalaGenArith with ScalaGenMathOps with ScalaGenOrderingOps with
    ScalaGenNullOps with ScalaGenEqual 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint /*with SymbolDependency*/ { val IR: self.type = self}
          
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  trait ImplFused extends DSL with ComplexInheritStructExp with StructInheritExpOptLoops with StructFatExpOptCommon with ArrayLoopsFatExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp  { self => 
    override val verbosity = 1
    val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenFatStruct with ScalaGenArith with ScalaGenMathOps with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self;
        override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  def testStruct1a = {
    withOutFile(prefix+"inheritedstruct1a") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(x.toDouble, 0)
          print(c.toCartesian)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct1a")
  }
  
  def testStruct1b = {
    withOutFile(prefix+"inheritedstruct1b") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = PolarComplex(x.toDouble, 0)
          print(c.toCartesian)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct1b")
  }

  def testStruct1c = {
    withOutFile(prefix+"inheritedstruct1c") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = SubPolarComplex(x.toDouble, 0)
          print(c.toCartesian)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct1c")
  }

  def testStruct2a = {
    withOutFile(prefix+"inheritedstruct2a") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = CartesianComplex(x.toDouble, 0)
          c = c+d
          print(c)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct2a")
  }

  def testStruct2b = {
    withOutFile(prefix+"inheritedstruct2b") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = PolarComplex(x.toDouble, 0)
          c = c+d
          print(c)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct2b")
  }  
  
  def testStruct3a = {
    withOutFile(prefix+"inheritedstruct3a") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = CartesianComplex(x.toDouble, 0)
          var e = if(x > 0) c else d
          print(e)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct3a")
  }

  def testStruct3b = {
    withOutFile(prefix+"inheritedstruct3b") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = PolarComplex(x.toDouble, 0)
          var e = if(x > 0) c else d
          print(e)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct3b")
  }

  def testStruct4a = {
    withOutFile(prefix+"inheritedstruct4a") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = CartesianComplex(x.toDouble, 0)
          var e = if(x > 0) c else d
          var f = e.toCartesian
          print(f)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct4a")
  }

  def testStruct4b = {
    withOutFile(prefix+"inheritedstruct4b") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = PolarComplex(0, x.toDouble)
          var d = PolarComplex(x.toDouble, 0)
          var e = if(x > 0) c else d
          var f = e.toCartesian
          print(f)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct4b")
  }  

  def testStruct4c = {
    withOutFile(prefix+"inheritedstruct4c") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = CartesianComplex(x.toDouble, 0)
          var e = if(x > 0) c else d
          if(e.IsInstanceOf[CartesianComplex]) print("definitely a cartesian!")
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct4c")
  } 
 
  def testStruct4d = {
    withOutFile(prefix+"inheritedstruct4d") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = PolarComplex(x.toDouble, 0)
          var e = if(x > 0) c else d
          if(e.IsInstanceOf[CartesianComplex]) print("definitely a cartesian!")
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct4d")
  } 

  def testStruct4e = {
    withOutFile(prefix+"inheritedstruct4e") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0, x.toDouble)
          var d = PolarComplex(x.toDouble, 0)
          var e = if(x > 0) c else d
          var f = e.toCartesian
          print(f)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct4e")
  }

/*  def testStruct5a = {
    withOutFile(prefix+"inheritedstruct5a") {
      // test basic struct flattening (loops, variables, conditionals)
      println("REMARK: this makes only sense with fat codegen (computation duplicated and some structs not removed otherwise)")
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          // split loops (rely on fusion, don't want to duplicate computation!)

          val vector1 = array(100) { i => CartesianComplex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => CartesianComplex(0.0 - i.toDouble, i.toDouble) }

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
    assertFileEqualsCheck(prefix+"inheritedstruct5a")
  }

  def testStruct5b = {
    withOutFile(prefix+"inheritedstruct5b") {
      // test basic struct flattening (loops, variables, conditionals)
      println("REMARK: this makes only sense with fat codegen (computation duplicated and some structs not removed otherwise)")
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          // split loops (rely on fusion, don't want to duplicate computation!)

          val vector1 = array[Complex](100) { i => CartesianComplex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array[Complex](100) { i => PolarComplex(0.0 - i.toDouble, i.toDouble) }

          val vector3 = if (x > 7) vector1 else vector2
          print(vector3)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct5b")
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
          def infix_map[A:Manifest,B:Manifest](c: Rep[Array[A]], f: Rep[A] => Rep[B]) = array(c.length) { i => f(c.at(i)) }

          val vector3 = vector1.map(conj)

          print(vector3)
        }
      }
      new Prog with ImplFused {
        // TODO: use a generic Opt trait instead of defining rewrites here...
        override def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
          case (x, Def(Minus(Const(0.0),y))) => infix_+(x,y)
          case _ => super.infix_-(x,y)
        }
        override def infix_+(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
          case (Const(0.0), y) => y
          case _ => super.infix_+(x,y)
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
          def infix_map[A:Manifest,B:Manifest](c: Rep[Array[A]], f: Rep[A] => Rep[B]) = array(c.length) { i => f(c.at(i)) }

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
*/
}
