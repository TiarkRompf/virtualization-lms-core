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


abstract class RFun {
  def exec(f: java.util.HashMap[String,Any]): Any
  def execInt(f: java.util.HashMap[String,Any]): Int
}


class TestInterpret extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test13-"
  
  trait InterpretPlain {

    import java.util.HashMap
    
    case class Unexpected(x: Any) extends Exception
    
    class Frame {
      //var locals: Array[Any] = _
      val data = new HashMap[String, Any]
    }
    

    class FunHolder extends Fun {
      var fun: Fun = _
      def exec(f: Frame): Any = fun.exec(f)
      override def execInt(f: Frame): Int = fun.execInt(f)
      def spec(f: Fun) = fun = f
    }
    
    abstract class Fun {
      def exec(f: Frame): Any
      def execInt(f: Frame): Int = exec(f) match { case x: Int => x case x => throw Unexpected(x) }
    }
      
    case class AddN(a: Fun, b: Fun) extends FunHolder {
      
      spec(new Fun {
        def exec(f: Frame) = {
          val x = a.exec(f)
          val y = b.exec(f)
          (x,y) match {
            case (x:Int, y: Int) => specializeInt(); x + y
            case _ => specializeGeneric(); x.toString + y.toString
          }
        }
      })
      
      def specializeInt() = spec(new Fun {
        println("spec-add-int: " + AddN.this)
        def exec(f: Frame) = execInt(f)
        override def execInt(f: Frame): Int = {
          val x = try { a.execInt(f) } catch { case Unexpected(x) => specializeGeneric(); throw Unexpected(x.toString + b.exec(f).toString) }
          val y = try { b.execInt(f) } catch { case Unexpected(y) => specializeGeneric(); throw Unexpected(x.toString + y.toString) }
          x + y
        }
      })

      def specializeGeneric() = spec(new Fun {
        println("spec-add-int: " + AddN.this)
        def exec(f: Frame) = {
          val x = a.exec(f)
          val y = b.exec(f)
          (x,y) match {
            case (x:Int, y: Int) => x + y
            case _ => x.toString + y.toString
          }
        }
      })
      
    }
    
    case class SeqN(xs: List[Fun]) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          xs.init.foreach(_.exec(f))
          xs.last.exec(f)
        }
      })
    }

    case class AssignN(x: String, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          val b = y.exec(f)
          f.data.put(x,b)
        }
      })
    }

    case class LookupN(x: String) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          f.data.get(x)
        }
      })
    }
    
    /*case class NotEqual(x: Fun, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          val b = y.exec(f)
          f.data(x) = b
        }
      })
    }*/

    case class AppN(x: Fun, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          val a = x.exec(f)
          val b = y.execInt(f)
          a.asInstanceOf[Array[Int]](b) // unsafe
        }
      })
    }


    case class ConstN(x: Any) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          x
        }
      })
    }

    case class WhileN(c: Fun, body: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          while (c.execInt(f) != 0) { body.exec(f); () }
        }
      })
    }
    
  }
  
  
  trait InterpretStaged extends DSL with Equal with NumericOps with PrimitiveOps with HashMapOps with ArrayOps with CellOps with StaticData { self =>

    // option 1: stage fully, one big method -- remove interpreter abstraction overhead but retain generic types
    // option 2: stage locally, one compiled fun per op -- no global optimizations
    
    // (this one uses option 2)

    import java.util.HashMap
    
    def infix_exec(x: Rep[RFun], f: Rep[HashMap[String,Any]]): Rep[Any]
    def infix_execInt(x: Rep[RFun], f: Rep[HashMap[String,Any]]): Rep[Int]
    
    def dcompile(x: Rep[Compile], fA: Rep[HashMap[String,Any]]=>Rep[Any], 
                                  fI: Rep[HashMap[String,Any]]=>Rep[Int]): Rep[RFun]
    
    class Frame(val data: Rep[HashMap[String, Any]])
    
    abstract class Fun {
      def exec(f: Frame): Rep[Any]
      def execInt(f: Frame): Rep[Int] = exec(f).AsInstanceOf[Int] //= exec(f) match { case x: Int => x case x => throw Unexpected(x) }
    }
    
    class FunHolder extends Fun {
      //val sfun: Cell[Fun] = cell("sta - " + this.toString)
      val dfun: Cell[RFun] = cell("dyn - " + this.toString)
      def exec(f: Frame): Rep[Any] = dfun.get.exec(f.data)
      override def execInt(f: Frame): Rep[Int] = dfun.get.execInt(f.data)
      def spec(f: Fun) = {
        //sfun.set(staticData(f))
        dfun.set(compileFun(f))
      }
    }

    def compileFun(f: Fun): Rep[RFun] = dcompile(staticData[Compile](self), 
      (x: Rep[HashMap[String,Any]]) => f.exec(new Frame(x)),
      (x: Rep[HashMap[String,Any]]) => f.execInt(new Frame(x)))
    
    def compileFun1(f: Fun): Rep[RFun] = staticData[RFun](new RFun {
      
      val fAny = compile((x: Rep[HashMap[String,Any]]) => f.exec(new Frame(x)))
      val fInt = compile((x: Rep[HashMap[String,Any]]) => f.execInt(new Frame(x)))
      
      def exec(f: HashMap[String,Any]): Any = fAny(f)
      def execInt(f: HashMap[String,Any]): Int = fInt(f)
    })
    
    
    case class AddN(a: Fun, b: Fun) extends FunHolder {
      
      spec(new Fun {
        def exec(f: Frame) = {
          val x = a.exec(f)
          val y = b.exec(f)
          if (x.IsInstanceOf[Int] && y.IsInstanceOf[Int]) {
            specializeInt()
            x.AsInstanceOf[Int] + y.AsInstanceOf[Int]
          } else {
            print("GENERIC")
          }
        }
      })
      
      def specializeInt() = spec(new Fun {
        print("spec-add-int: " + AddN.this)
        def exec(f: Frame) = execInt(f)
        override def execInt(f: Frame) = {
          val x = a.execInt(f)
          val y = b.execInt(f)
          numeric_plus(x, y)
        }
      })
      
      /*spec(new Fun {
        def exec(f: Frame) = {
          val x = a.exec(f)
          val y = b.exec(f)
          (x,y) match {
            case (x:Int, y: Int) => specializeInt(); x + y
            case _ => specializeGeneric(); x.toString + y.toString
          }
        }
      })
      
      def specializeInt() = spec(new Fun {
        println("spec-add-int: " + AddN.this)
        def exec(f: Frame) = execInt(f)
        override def execInt(f: Frame): Int = {
          val x = try { a.execInt(f) } catch { case Unexpected(x) => specializeGeneric(); throw Unexpected(x.toString + b.exec(f).toString) }
          val y = try { b.execInt(f) } catch { case Unexpected(y) => specializeGeneric(); throw Unexpected(x.toString + y.toString) }
          x + y
        }
      })

      def specializeGeneric() = spec(new Fun {
        println("spec-add-int: " + AddN.this)
        def exec(f: Frame) = {
          val x = a.exec(f)
          val y = b.exec(f)
          (x,y) match {
            case (x:Int, y: Int) => x + y
            case _ => x.toString + y.toString
          }
        }
      })*/
      
    }
    
    case class SeqN(xs: List[Fun]) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          xs.init.foreach(_.exec(f))
          xs.last.exec(f)
        }
      })
    }

    case class AssignN(x: String, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          val b = y.exec(f)
          f.data(x) = b
        }
      })
    }

    case class LookupN(x: String) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          f.data(x)
        }
      })
    }
    
    /*case class NotEqual(x: Fun, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          val b = y.exec(f)
          f.data(x) = b
        }
      })
    }*/

    case class AppN(x: Fun, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          val a = x.exec(f)
          val b = y.execInt(f)
          a.AsInstanceOf[Array[Int]].apply(b) // unsafe
        }
      })
    }


    case class ConstN(x: Any) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          staticData(x)
        }
      })
    }

    case class WhileN(c: Fun, body: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          while (c.execInt(f) != unit(0)) { body.exec(f); unit(()) }
        }
      })
    }

  }
  

  trait InterpretStagedReopt extends DSL with Equal with NumericOps with PrimitiveOps with HashMapOps 
      with ArrayOps with CellOps with StableVars with StaticData { self =>

    // option 1: stage fully, one big method -- remove interpreter abstraction overhead but retain generic types
    // option 2: stage locally, one compiled fun per op -- no global optimizations
    
    // (this one uses option 3)

    import java.util.HashMap
    
    //def infix_exec(x: Rep[Fun], f: Rep[HashMap[String,Any]]): Rep[Any]
    //def infix_execInt(x: Rep[Fun], f: Rep[HashMap[String,Any]]): Rep[Int]
    
    //def dcompile(x: Rep[Compile], fA: Rep[HashMap[String,Any]]=>Rep[Any], 
    //                              fI: Rep[HashMap[String,Any]]=>Rep[Int]): Rep[RFun]
    
    class Frame(val data: Rep[HashMap[String, Any]])
    
    abstract class Fun {
      override def toString = "FUN:"+ this.getClass.getName + "@XXX"
      def exec(f: Frame)(k: Rep[Any] => Continue[Any]): Continue[Any]
      def execInt(f: Frame)(k: Rep[Int] => Continue[Any]): Continue[Any] = exec(f) { r => k(r.AsInstanceOf[Int]) }
    }
    
    class FunHolder extends Fun {
      // casts and AnyRef are to avoid unprintable types in generated code
      //val sfun: Cell[Fun] = cell("sta - " + this.toString)
      val dfun: RCell[AnyRef] = new RCell("dyn - " + this.toString)
      def exec(f: Frame)(k: Rep[Any] => Continue[Any]): Continue[Any] = {
        val data = f.data
        readOneValue(dfun) { ef => id(data)/*free var*/; ef.asInstanceOf[Fun].exec(f)(k) }
      }
      override def execInt(f: Frame)(k: Rep[Int] => Continue[Any]): Continue[Any] = {
        val data = f.data
        readOneValue(dfun) { ef => id(data)/*free var*/; ef.asInstanceOf[Fun].execInt(f)(k) }
      }
      def id(x:Any) = { println("-- " + x) } //ignore
      def spec(f: Fun) = {
        dfun.set(f)
      }
      def spec1(f: Fun) = {
        staticData(dfun).set(staticData[AnyRef](f))
      }
    }
    
    case class AddN(a: Fun, b: Fun) extends FunHolder {
      
      spec(new Fun {
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = {
          a.exec(f) { x =>
          b.exec(f) { y =>
          k(if (x.IsInstanceOf[Int] && y.IsInstanceOf[Int]) {
            specializeInt()
            x.AsInstanceOf[Int] + y.AsInstanceOf[Int]
          } else {
            print("GENERIC")
          })
        }}}
      })
      
      def specializeInt() = spec1(new Fun {
        print("spec-add-int: " + AddN.this)
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = execInt(f)(k)
        override def execInt(f: Frame)(k: Rep[Int] => Continue[Any]) = {
          a.execInt(f) { x =>
          b.execInt(f) { y =>
          k(numeric_plus(x, y))
        }}}
      })
      
    }
    
    case class SeqN(xs: List[Fun]) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = {
          def iter(xs: List[Fun]): Continue[Any] = xs match {
            case x::Nil => x.exec(f)(k)
            case x::xs => x.exec(f)(r => iter(xs))
          }
          iter(xs)
        }
      })
    }

    case class AssignN(x: String, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = {
          y.exec(f) { b => 
          f.data(x) = b
          k(())
        }}
      })
    }

    case class LookupN(x: String) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = {
          k(f.data(x))
        }
      })
    }
    
    /*case class NotEqual(x: Fun, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame) = {
          val b = y.exec(f)
          f.data(x) = b
        }
      })
    }*/

    case class AppN(x: Fun, y: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = {
          x.exec(f) { a =>
          y.execInt(f) { b =>
          k(a.AsInstanceOf[Array[Int]].apply(b)) // unsafe
        }}}
      })
    }


    case class ConstN(x: Any) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = {
          k(staticData(x))
        }
      })
    }

    case class WhileN(c: Fun, body: Fun) extends FunHolder {
      spec(new Fun {
        def exec(f: Frame)(k: Rep[Any] => Continue[Any]) = {
          //def recurse = lambda { }

          //while (c.execInt(f) != unit(0)) { body.exec(f); unit(()) }
          c.execInt(f) { c1 =>
//            if (c1 != unit(0)) { body.exec(f)(k) } else k(unit())
            if (true) { body.exec(f)(k) } else k(unit())
          }


        }
      })
    }

  }




  trait InterpretStagedExp extends EffectExp with StaticDataExp with CellOpsExp with UncheckedOpsExp {
    
    override def toString = "IR:" + getClass.getName

    import java.util.HashMap
        
    case class RFunExec(x: Rep[RFun], f: Rep[HashMap[String,Any]]) extends Def[Any]
    case class RFunExecInt(x: Rep[RFun], f: Rep[HashMap[String,Any]]) extends Def[Int]
    
    def infix_exec(x: Rep[RFun], f: Rep[HashMap[String,Any]]): Rep[Any] = reflectEffect(RFunExec(x,f))
    def infix_execInt(x: Rep[RFun], f: Rep[HashMap[String,Any]]): Rep[Int] = reflectEffect(RFunExecInt(x,f))
    
    //case class DCompile(x: Rep[Compile], fA: Rep[Any], fI: Rep[Any]) extends Def[RFun]

    def dcompile(x: Rep[Compile], fA: Rep[HashMap[String,Any]]=>Rep[Any], 
      fI: Rep[HashMap[String,Any]]=>Rep[Int]): Rep[RFun] = 
      unchecked("new ",manifest[RFun]," {\n"+/*}*/
        "type Rep[T] = ",x,".Rep[T]\n"+
        "type HM = java.util.HashMap[String,Any]\n"+
        "val fAny = ",x,".compile(",staticData[AnyRef](fA),".asInstanceOf[Rep[HM]=>Rep[Any]])\n"+
        "val fInt = ",x,".compile(",staticData[AnyRef](fI),".asInstanceOf[Rep[HM]=>Rep[Int]])\n"+
        "def exec(f: HM): Any = fAny(f)\n"+
        "def execInt(f: HM): Int = fInt(f)\n"+
        /*{*/"}")
  }
  
  trait ScalaGenInterp extends ScalaGenBase {
    val IR: InterpretStagedExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case RFunExec(x,f) =>  emitValDef(sym, quote(x) + ".exec(" + quote(f) + ")")
      case RFunExecInt(x,f) =>  emitValDef(sym, quote(x) + ".execInt(" + quote(f) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
  
  
  trait DSL extends VectorOps with Arith with OrderingOps with BooleanOps with LiftVariables 
    with IfThenElse with While with RangeOps with Print with Compile with NumericOps with PrimitiveOps 
    with ArrayOps with HashMapOps with CastingOps {
    
    def test(): Unit
  }
  
  trait Impl extends DSL with VectorExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp 
    with EqualExpOpt with IfThenElseFatExp with LoopsFatExp with WhileExp
    with RangeOpsExp with PrintExp with FatExpressions with CompileScala
    with NumericOpsExp with PrimitiveOpsExp with ArrayOpsExp with HashMapOpsExp with CastingOpsExp with StaticDataExp 
    with InterpretStagedExp { self =>
    override val verbosity = 1
    dumpGeneratedCode = true
    val codegen = new Codegen { val IR: self.type = self }
    val runner = new Runner { val p: self.type = self }
    runner.run()
  }
  
  trait Codegen extends ScalaGenVector with ScalaGenArith with ScalaGenOrderingOps with ScalaGenBooleanOps
    with ScalaGenVariables with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhile
    with ScalaGenRangeOps with ScalaGenPrint 
    with ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenArrayOps with ScalaGenHashMapOps with ScalaGenCastingOps with ScalaGenStaticData 
    with ScalaGenInterp with ScalaGenCellOps with ScalaGenUncheckedOps {
    val IR: Impl
  }
  
  
  trait Runner {
    val p: Impl
    def run() = {
      p.test()
    }
  }
  
  
  def testInterpret1 = withOutFileChecked(prefix+"interpret1") {
    trait Prog extends DSL with InterpretPlain {
      def test() = {

        /*
        function f(a, n) {
          var x = 0;
          while (n-- > 0) {
            x = x + a[n]
          }
          return x;
        }
        */
        val A = scala.Array

        val p = SeqN(List(
          AssignN("x", ConstN(0)),
          WhileN(LookupN("n"), SeqN(List(
            AssignN("n", AddN(LookupN("n"), ConstN(-1))),
            AssignN("x", AddN(LookupN("x"), AppN(LookupN("a"),LookupN("n"))))
          ))),
          LookupN("x")
        ))
        
        
        val env = new Frame
        env.data.put("n", 256)
        env.data.put("a", A.tabulate(256)(2*_))
        val y = p.exec(env)
        
        println(y) // expected: 65280
      }
    }
    new Prog with Impl
  }

  def testInterpret2 = withOutFileChecked(prefix+"interpret2") {
    trait Prog extends DSL with InterpretStaged {
      def test() = {

        /*
        function f(a, n) {
          var x = 0;
          while (n-- > 0) {
            x = x + a[n]
          }
          return x;
        }
        */

        val q = compile { env: Rep[java.util.HashMap[String,Any]] => 
        
          val p = SeqN(List(
            AssignN("x", ConstN(0)),
            WhileN(LookupN("n"), SeqN(List(
              AssignN("n", AddN(LookupN("n"), ConstN(-1))),
              AssignN("x", AddN(LookupN("x"), AppN(LookupN("a"),LookupN("n"))))
            ))),
            LookupN("x")
          ))
        
          p.exec(new Frame(env))
        }
        
        val env = new java.util.HashMap[String,Any]()
        env.put("n",256)
        env.put("a", scala.Array.tabulate(256)(2*_))
        val y = q(env)
        
        println(y) // expected: 65280
      }
    }
    new Prog with Impl
  }

  def testInterpret3 = withOutFileChecked(prefix+"interpret3") {
    trait Prog extends DSL with InterpretStagedReopt {
      def test() = {

        /*
        function f(a, n) {
          var x = 0;
          while (n-- > 0) {
            x = x + a[n]
          }
          return x;
        }
        */

        val q = compileStable { env: Rep[java.util.HashMap[String,Any]] => 
        
          var x = env// go through a var, because we currently rely on def lookup which fails for free syms
          val env1 = x

          val p = AddN(LookupN("n"), ConstN(-1))/*SeqN(List(
            AssignN("x", ConstN(0)),
            WhileN(LookupN("n"), SeqN(List(
              AssignN("n", AddN(LookupN("n"), ConstN(-1))),
              AssignN("x", AddN(LookupN("x"), AppN(LookupN("a"),LookupN("n"))))
            ))),
            LookupN("x")
          ))*/
        
          p.exec(new Frame(env1))(r=>Done(r))
        }
        
        val env = new java.util.HashMap[String,Any]()
        env.put("n",256)
        env.put("a", "SomethingElse") // "a" -> scala.Array.tabulate(256)(2*_)
        val y = q(env)
        
        println(y) // expected: 65280
      
        val z = q(env)
        
        println(z) // expected: 65280
      }
    }
    new Prog with Impl with StableVarsExp
  }
}
