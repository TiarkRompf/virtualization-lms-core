package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

/*
  TODO outstanding issues:

    - mutable variable need to be heap-allocated if referenced from within a closure
      and stack allocated from within a nested function that is not a closure.
      this can be done conservatively (always alloc ref cells on the heap).
      but a better approach would be to heap- or stack-alloc only the necessary ones.
      
    - closures should not unnecessarily be heap-allocated as well (currently all
      nested functions are closures and heap-allocated).
      
      this complicates the analysis for variables, as witnessed by scalac:
      
        def foo = {
          var x = 0
          val f = (y:Int) => x = y
          f(7)
        }
        
      compiled with -optimize results in 
      
        // methods
        def foo(): Unit {
        locals: variable x$1
        startBlock: 1
        blocks: [1]
  
        1: 
          1 NEW REF(class IntRef)
          1 DUP(REF(class IntRef))
          1 CONSTANT(Constant(0))
          1 CALL_METHOD scala.runtime.IntRef.<init> (static-instance)
          1 STORE_LOCAL(variable x$1)
          1 SCOPE_ENTER variable x$1
          1 SCOPE_ENTER value f
          undef LOAD_LOCAL(variable x$1)
          1 CONSTANT(Constant(7))
          1 STORE_FIELD variable elem (dynamic)
          1 SCOPE_EXIT variable x$1
          1 SCOPE_EXIT value f
          1 RETURN(UNIT)
    
        }
        
      which still allocates the IntRef although the closure record is eliminated 
      and the code inlined.
      
      Phase ordering bites again!
      
*/



class TestLambdalift extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test7-"
  
  def `testLambdalift1 ` = {
    // test lambda lifting
    withOutFile(prefix+"lambdalift1") {
      var classes: List[String] = Nil
      
      trait ScalaGenBla extends ScalaGenBase {
        import IR._
        def emitFocused[A,B](name: String, params: List[Exp[Any]], x: Exp[A], y: Block[B], out: PrintWriter): Unit
      }
      
      new NestLambdaProg1 with ArithExp with FunctionsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { 
          val IR: self.type = self
          
          /*def boundAndUsedInScope(x: Exp[Any], y: Exp[Any]): (List[Sym[Any]], List[Sym[Any]]) = {
            val used = (syms(y):::innerScope.flatMap(t => syms(t.rhs))).distinct
            val bound = (syms(x):::innerScope.flatMap(t => t.sym::boundSyms(t.rhs))).distinct
            (bound, used)
          }
          def freeInScope(x: Exp[Any], y: Exp[Any]): List[Sym[Any]] = {
            val (bound, used) = boundAndUsedInScope(x,y)
            used diff bound
          }*/
          
          override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
            case e@Lambda(fun, x, y) =>
            
              focusBlock(y) {
                var free = freeInScope(List(x),List(getBlockResultFull(y)))
                
                val sw = new StringWriter
                codegenInner.emitFocused("Anonfun_"+quote(sym), free, x, y, new PrintWriter(sw))
                classes = sw.toString :: classes
            
                stream.println("val " + quote(sym) + " = new Anonfun_" + quote(sym) + "("+free.map(quote).mkString(",")+")")
              }

            case _ => super.emitNode(sym, rhs)
          }
        }
        val codegen2 = new ScalaGenBla with ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { 
          val IR: self.type = self
          
          override def initialDefs = codegen.availableDefs
          
          def emitFocused[A,B](name: String, params: List[Exp[Any]], x: Exp[A], y: Block[B], out: PrintWriter) = {
            // TODO: this is not valid Scala code. the types are missing.
            withStream(out) {
              stream.println("class "+name+"("+params.map(quote).mkString(",")+") {")
              stream.println("def apply("+quote(x)+") = {")
              traverseBlockFocused(y)
              stream.println(quote(getBlockResult(y)))
              stream.println("}")
              stream.println("}")
            }
          }
        }
        val codegenInner: ScalaGenBla { val IR: self.type; type Block[+T] = self.Block[T] } = codegen2
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        classes.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"lambdalift1")
  }
  
}
