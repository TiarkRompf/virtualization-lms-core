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
  case class NullGuy[T](mT:Manifest[T]) extends Def[T]
  def Null[T:Manifest] : Exp[T] = NullGuy[T](manifest[T])
}

trait ScalaGenNullOps extends GenericNestedCodegen{
  val IR: NullOpsExp
  import IR._

  //quick hack for getting 0 as a null for doubles
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NullGuy(inner) => emitValDef(sym, "0")
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
  def conj(c: Rep[Complex]) = {
    val temp = c.toCartesian
    CartesianComplex(temp.re, 0.0 - temp.im)
  }
}

abstract class Complex
class CartesianComplex extends Complex
class PolarComplex extends Complex
class SubPolarComplex extends PolarComplex



trait ComplexInheritBase extends Arith with MathOps with CastingOps with IfThenElse{

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
trait StructInheritanceExp extends StructExpOpt with CastingOpsExp with SuperIfThenElseExp with MathOpsExp with VariablesExp{
  
  //new node!
  case class StructIsInstanceOf[A,B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B]) extends Def[Boolean]

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

  //case class ArraySoaTag[T](base: StructTag[T], len: Exp[Int]) extends StructTag[Array[T]]
  //in a copy-paste style!
  def deReiify[T:Manifest](a: Block[T]): (Block[Unit], Rep[T]) = a match { // take Reify(stms, e) and return Reify(stms, ()), e
    case Block(Def(Reify(x,es,u))) => (Block(Reify(Const(()), es, u)), x)
    case Block(x) => (Block(Const(())), x)
  }

  override def rep_isinstanceof[A,B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext) = lhs match {
      case Def(Struct(tag, elems)) => tag match {
        case MyClassTag(mT) => unit(mT <:< mB)
        case LubTag(_,_,_) =>
          val aField = elems("clzz").asInstanceOf[Rep[A]]
          //TODO: replace with generation of <:< test
          //super.rep_isinstanceof(aField, mA, mB)
          StructIsInstanceOf(aField, mA, mB)
        //case ArraySoaTag(mT,_) => unit(true)//lhs
        case _ => 
          super.rep_isinstanceof(lhs,mA,mB)
        } 
      /*case Def(ArrayIndex(Def(Struct(ArraySoaTag(tag,_), elems)),_)) => 
        tag match {
        case MyClassTag(mT) => unit(mT <:< mB)
        case LubTag(_,_,_) => 
          val aField = elems("clzz").asInstanceOf[Rep[Manifest[_]]]
          val myequality = equals(aField,unit(mB.erasure))
          myequality
        case _ => super.rep_isinstanceof(lhs, mA, mB)

      }*/
      case _ => 
        super.rep_isinstanceof(lhs, mA, mB)
  }

  override def rep_asinstanceof[A, B:Manifest](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B])(implicit pos: SourceContext): Exp[B] = lhs match {
      case Def(Struct(tag, elems)) => tag match {
        case MyClassTag(_) => lhs.asInstanceOf[Exp[B]]
        case LubTag(mT, mL, mR) =>
          assert(mB <:< mL || mB <:< mR)
          struct[B](MyClassTag[B](mB), elems + ("clzz" -> unit(mB)))
        case _ => super.rep_asinstanceof(lhs, mA, mB)
      }
      /*case Def(ArrayIndex(Def(Struct(ArraySoaTag(tag,_), elems)),i)) => 
        tag match {
        case MyClassTag(mT) =>
          lhs.asInstanceOf[Exp[B]]
        case LubTag(_,_,_) =>
          lhs.asInstanceOf[Exp[B]]
          
        case _ => 
          super.rep_asinstanceof(lhs, mA, mB)
      }*/     
      case _ => 
        super.rep_asinstanceof(lhs, mA, mB)
  }
}

trait ComplexInheritStructExp extends ComplexInheritBase with StructInheritanceExp{

  def CartesianComplex(re: Exp[Double], im: Exp[Double]) = {
    val cartesianTag = MyClassTag[CartesianComplex](manifest[CartesianComplex])

    struct[CartesianComplex](cartesianTag, Map("clzz"-> unit(manifest[CartesianComplex]), "re"->re, "im"->im))
  }
  def infix_re(c: Exp[CartesianComplex]): Exp[Double] = field[Double](c, "re")
  def infix_im(c: Exp[CartesianComplex]): Exp[Double] = field[Double](c, "im")

  //Polar complex numbers
  def PolarComplex(r: Exp[Double], phi: Exp[Double]) = {
    val polarTag = MyClassTag[PolarComplex](manifest[PolarComplex])

    struct[PolarComplex](polarTag, Map("clzz"-> unit(manifest[PolarComplex]), "r"->r, "phi"->phi))
  } 
  
  def infix_r(c: Exp[PolarComplex]): Exp[Double] = field[Double](c, "r")
  def infix_phi(c: Exp[PolarComplex]): Exp[Double] = field[Double](c, "phi")

  //subPolar complex numbers
  def SubPolarComplex(r: Exp[Double], phi: Exp[Double]) = {
    val subpolarTag = MyClassTag[SubPolarComplex](manifest[SubPolarComplex])

    struct[SubPolarComplex](subpolarTag, Map("clzz" -> unit(manifest[SubPolarComplex]), "r"->r, "phi"->phi))
  }
}

// ------ struct impl follows, will move to common once stable
trait StructInheritExpOptLoops extends StructExpOptCommon with StructInheritanceExp with ArrayLoopsExp with NullOpsExp{

  //TODO
  protected def getManifestOf[T:Manifest](mT: Manifest[T]) = manifest[Manifest[T]]
  
  case class ArraySoaTag[T](base: StructTag[T], len: Exp[Int]) extends StructTag[Array[T]]

  //should assume a topmost type like "Object"
  /*private def lub(hierarchy1: List[MyClassTag[Any]], hierarchy2: List[MyClassTag[Any]]) : List[MyClassTag[Any]] = {
    def inner(temp: List[MyClassTag[Any]], h1: List[MyClassTag[Any]], h2: List[MyClassTag[Any]]): List[MyClassTag[Any]] = {
      (h1, h2) match {
        case (x::xs, y::ys) => if(x ==y) inner(x::temp, xs, ys) else return temp
        case _ => temp
      }
    }
    inner(hierarchy1.reverse.head::Nil, hierarchy1.reverse.tail, hierarchy2.reverse.tail)
  }*/

  override def ifThenElse[T:Manifest](cond: Exp[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = {
    (deReiify(a),deReiify(b)) match {
      case ((u, Def(s1@Struct(tagA, elemsA))), (v, Def(Struct(tagB, elemsB)))) =>
        //merge the keyset of c1 and c2
        val elemsNewkeys = elemsA.keySet union elemsB.keySet
        val (newStructTag, elemsNew) = (tagA, tagB) match {
          case (MyClassTag(c1), MyClassTag(c2)) => 
            assert(c1 <:< manifest[T] && c2 <:< manifest[T])

            val tempStructTag = if(c1.equals(manifest[T]) || c2.equals(manifest[T])) 
              MyClassTag(manifest[T])
            else LubTag(manifest[T], c1, c2)

            val tempElems = (for (k <- elemsNewkeys) yield (
              k -> ifThenElse(cond, Block(elemsA.getOrElse(k, Null)), Block(elemsB.getOrElse(k, Null)))(mtype(elemsA.getOrElse(k,
                    elemsB(k)).tp), SourceContext("",Nil)) //TODO : add sourcecontext
            )).toMap
            val tempElems2 = tempElems + ("clzz" -> rep_asinstanceof(tempElems("clzz"), manifest[Any], manifest[Manifest[T]]))
            (tempStructTag, tempElems2)
            
          //TODO: think about nested ifThenElse for structs
          //case(LubTag(a,_,_), LubTag(b,_,_)) => LubTag(_,a,b)
          case (ArraySoaTag(MyClassTag(c1),len1), ArraySoaTag(MyClassTag(c2),len2)) =>
            //assert(len1 == len2)
            val requiredManifest = manifest[T]
            val innerManifest = requiredManifest.typeArguments.head

            assert(c1 <:< innerManifest && c2 <:< innerManifest)
            val temp = if(c1.equals(innerManifest) || c2.equals(innerManifest)) 
              MyClassTag(innerManifest.asInstanceOf[Manifest[T]])
            else LubTag(innerManifest.asInstanceOf[Manifest[T]], c1, c2)

            val tempElems = (for (k <- elemsNewkeys) yield (
              //we need an arraySoaTag of nulls here, we know this
              //TODO: is this too hackish?
              k -> ifThenElse(cond, Block(elemsA.getOrElse(k, array(len1){i=> unit(0.0)})), Block(elemsB.getOrElse(k, array(len1){i =>
                    unit(0.0)}))) //TODO : add sourcecontext
            )).toMap
            val tempElems2 = tempElems + ("clzz" -> rep_asinstanceof(tempElems("clzz"), manifest[Any], getManifestOf(innerManifest).arrayManifest))

            (ArraySoaTag(temp.asInstanceOf[StructTag[T]], len1), tempElems)


          case _ => throw new Error("tags dont match!")//super.ifThenElse(cond, a, b)
      }
      struct(newStructTag.asInstanceOf[StructTag[T]], elemsNew.toMap)
    case _ => super.ifThenElse(cond, a, b)
  }}

override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case ArrayElem(Block(Def(Struct(tag:StructTag[A], elems)))) => 
      struct[Array[A]](ArraySoaTag[A](tag,size), elems.map(p=>(p._1,simpleLoop(size, v, ArrayElem(Block(p._2)))(p._2.tp.arrayManifest))))
    case ArrayElem(Block(Def(ArrayIndex(b,`v`)))) if infix_length(b) == size => b.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
    case _ => super.simpleLoop(size, v, body)
  }
  
  override def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = a match {
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
    case Def(Struct(ArraySoaTag(_,len),_)) => len
    case _ => super.infix_length(a)
  }

}

trait StructInheritFatExpOptLoops extends StructInheritExpOptLoops with StructFatExpOptCommon {
    override def ifThenElse[T:Manifest](cond: Exp[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = {
    (deReiify(a),deReiify(b)) match {
      case ((u, Def(s1@Struct(tagA, elemsA))), (v, Def(Struct(tagB, elemsB)))) =>

        // create stm that computes all values at once
        // return struct of syms
        val combinedResult = super.ifThenElse(cond,u,v)
        
        //merge the keyset of c1 and c2
        val elemsNewkeys = elemsA.keySet union elemsB.keySet
        val (newStructTag, elemsNew) = (tagA, tagB) match {
          case (MyClassTag(c1), MyClassTag(c2)) => 
            assert(c1 <:< manifest[T] && c2 <:< manifest[T])

            val tempStruct = if(c1.equals(manifest[T]) || c2.equals(manifest[T])) 
              MyClassTag(manifest[T])
            else LubTag(manifest[T], c1, c2)

            val tempElems = (for (k <- elemsNewkeys) yield (
                k -> phi(cond, u, elemsA.getOrElse(k, Null),v, elemsB.getOrElse(k,
                    Null))(combinedResult)(mtype(elemsA.getOrElse(k,elemsB(k)).tp)) //TODO : add sourcecontext
            )).toMap
            (tempStruct, tempElems+("clzz"->rep_asinstanceof(tempElems("clzz"), manifest[Any], manifest[Manifest[T]])))

            
          //TODO: think about nested ifThenElse for structs
          //case(LubTag(a,_,_), LubTag(b,_,_)) => LubTag(_,a,b)
          case (ArraySoaTag(MyClassTag(c1),len1), ArraySoaTag(MyClassTag(c2),len2)) =>
            //assert(len1 == len2)
            val requiredManifest = manifest[T]
            val innerManifest = requiredManifest.typeArguments.head

            assert(c1 <:< innerManifest && c2 <:< innerManifest)
            val temp = if(c1.equals(innerManifest) || c2.equals(innerManifest)) 
              MyClassTag(innerManifest.asInstanceOf[Manifest[T]])
            else LubTag(innerManifest.asInstanceOf[Manifest[T]], c1, c2)

            val tempStruct = ArraySoaTag(temp.asInstanceOf[StructTag[T]], len1)
            val tempElems = (for (k <- elemsNewkeys) yield (
                k -> phi(cond, u, elemsA.getOrElse(k, array(len1){i => unit(0.0)}),v, elemsB.getOrElse(k, array(len1){i =>
                    unit(0.0)}))(combinedResult)(mtype(elemsA.getOrElse(k,elemsB(k)).tp))//TODO : add sourcecontext
            )).toMap
            (tempStruct, tempElems+("clzz"->rep_asinstanceof(tempElems("clzz"), manifest[Any], getManifestOf(innerManifest).arrayManifest)))

          case _ => throw new Error("tags dont match!")//super.ifThenElse(cond, a, b)
      }
      struct(newStructTag.asInstanceOf[StructTag[T]], elemsNew.toMap)
    case _ => super.ifThenElse(cond, a, b)
  }}
}

trait ScalaInheritedGenCastingOps extends ScalaGenCastingOps{
    val IR: StructInheritanceExp
    import IR._
   //override generation of is_instanceof
    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match{
      case StructIsInstanceOf(x, mA, mB) => 
        emitValDef(sym, quote(x)+ " <:< manifest["+mB+"]")
      case _ => super.emitNode(sym, rhs)
    }
}

// ----- test cases
class TestInheritedStruct extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends ComplexInheritArith with ArrayLoops with Arith with OrderingOps with Variables with LiftVariables with IfThenElse with
  RangeOps with ImplicitOps with Print with CompileScala{
    def infix_toDouble(x: Rep[Int]): Rep[Double] = implicit_convert[Int, Double](x)//.asInstanceOf[Rep[Double]]
    def test(x: Rep[Int]): Rep[Any]
  }

  trait Impl extends DSL with ComplexInheritStructExp with ArrayLoopsExp with StructInheritExpOptLoops with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 1
    //import util.SymbolDependency
    val codegen = new ScalaGenArrayLoops with ScalaGenStruct with ScalaGenArith with ScalaGenMathOps with ScalaGenOrderingOps with
    ScalaGenNullOps with ScalaGenCastingOps with ScalaGenEqual 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenImplicitOps with ScalaInheritedGenCastingOps with ScalaGenPrint /*with SymbolDependency*/ { 
        val IR: self.type = self

        override def quote(x: Exp[Any]) : String = x match {
          case Const(mT: Manifest[_]) => "manifest["+mT.toString+"]"
          case _ => super.quote(x)
        }
      }
  }

  trait ImplFused extends DSL with ComplexInheritStructExp with StructInheritFatExpOptLoops with StructFatExpOptCommon with ArrayLoopsFatExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp  { self => 
    override val verbosity = 1
    val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenFatStruct with ScalaGenArith with ScalaGenMathOps with ScalaGenOrderingOps 
      with ScalaGenNullOps with ScalaGenCastingOps with ScalaGenEqual
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenImplicitOps with ScalaInheritedGenCastingOps with ScalaGenPrint { val IR: self.type = self;
        override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true 
        override def quote(x: Exp[Any]) : String = x match {
          case Const(mT: Manifest[_]) => "manifest["+mT.toString+"]"
          case _ => super.quote(x)
        }
      }
  }

  def testStruct1a = {
    withOutFile(prefix+"inheritedstruct1a") {
      // test variable splitting
      trait Prog extends DSL{
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(x.toDouble, 1.3)
          print(c.toCartesian)
        }
      }
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      } 
    }
    assertFileEqualsCheck(prefix+"inheritedstruct1c")
  }

  def testStruct2a = {
    withOutFile(prefix+"inheritedstruct2a") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(0.0, x.toDouble)
          var d = CartesianComplex(x.toDouble, 0.0)
          c = c+d
          print(c)
        }
      }
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      } 
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct4a")
  }

  def testStruct4b = {
    withOutFile(prefix+"inheritedstruct4b") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = PolarComplex(3.0, x.toDouble)
          var d = PolarComplex(x.toDouble, 2.0)
          var e = if(x > 0) c else d
          var f = e.toCartesian
          print(f)
        }
      }
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct4e")
  }

  def testStruct5a = {
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
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

          val vector1 = array(100) { i => CartesianComplex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => CartesianComplex(0.0 - i.toDouble, i.toDouble) }

          val vector3 = if (x > 7) vector1 else vector2
          print(vector3)
        }
      }
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct5b")
  }

  def testStruct5c = {
    withOutFile(prefix+"inheritedstruct5c") {
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
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct5c")
  }

  def testStruct5d = {
    withOutFile(prefix+"inheritedstruct5d") {
      // test basic struct flattening (loops, variables, conditionals)
      println("REMARK: this makes only sense with fat codegen (computation duplicated and some structs not removed otherwise)")
      trait Prog extends DSL{
        def test(x: Rep[Int]) = {
          // split loops (rely on fusion, don't want to duplicate computation!)

          val vector1 = array[Complex](100) { 
            i => {
              if(i < 4)
                CartesianComplex(i.toDouble, 0.0 - i.toDouble) 
              else
                PolarComplex(i.toDouble, 0.0 - i.toDouble)
            }
          }
          print(vector1)
        }
      }
      new Prog with Impl{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct5c")
  }  

  def testStruct6a = {
    withOutFile(prefix+"inheritedstruct6a") {
      // fuse conjugate computation with construction, essentially a no-op
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val vector1 = array(100) { i => CartesianComplex(i.toDouble, 0.0 - i.toDouble) }
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
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct6a")
  }

  def testStruct6b = {
    withOutFile(prefix+"inheritedstruct6b") {
      // fuse conjugate computation with construction, essentially a no-op
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {

          val vector1 = array(100) { i => PolarComplex(i.toDouble, 0.0 - i.toDouble) }
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
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct6b")
  }

  def testStruct6c = {
    withOutFile(prefix+"inheritedstruct6c") {
      // fuse conjugate computation with construction, essentially a no-op
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {

          val vector1 = array[Complex](100) { i => CartesianComplex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array[Complex](100) { i => PolarComplex(i.toDouble, 0.0 - i.toDouble) }

          def infix_map[A:Manifest,B:Manifest](c: Rep[Array[A]], f: Rep[A] => Rep[B]) = array(c.length) { i => f(c.at(i)) }

          val vector3 = if(x > 0) vector1 else vector2
          val vector4 = vector3.map(conj)
          print(vector4)
        }
      }
      new Prog with ImplFused{
        // TODO: use a generic Opt trait instead of defining rewrites here...
        override def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
          case (x, Def(Minus(Const(0.0),y))) => infix_+(x,y)
          case _ => super.infix_-(x,y)
        }
        override def infix_+(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
          case (Const(0.0), y) => y
          case _ => super.infix_+(x,y)
        }
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct6c")
  }
  
    def testStruct6d = {
    withOutFile(prefix+"inheritedstruct6d") {
      trait Prog extends DSL {
        // recognize that only imaginary part is modified, real part untouched
        def test(x: Rep[Int]) = {

          val vector1 = array(100) { i => CartesianComplex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => CartesianComplex(0.0 - i.toDouble, i.toDouble) }

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
      new Prog with ImplFused{
        val tests = (input: Rep[Int]) => test(input)
        codegen.emitSource(tests, "Test", new PrintWriter(System.out))
        val testc = compile(tests)
        testc(3)
      }
    }
    assertFileEqualsCheck(prefix+"inheritedstruct6d")
  }

}
