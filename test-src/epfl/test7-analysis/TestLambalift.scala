package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

class TestLambdalift extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testLambdalift1 = {
    // test lambda lifting
    withOutFile(prefix+"lambdalift1") {
      var classes: List[String] = Nil
      
      trait ScalaGenBla extends ScalaGenBase {
        import IR._
        def emitFocused[A,B](name: String, params: List[Exp[_]], x: Exp[A], y: Exp[B])(implicit stream: PrintWriter): Unit
      }
      
      new NestLambdaProg with ArithExp with FunctionsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { 
          val IR: self.type = self
          
          def boundAndUsedInScope(x: Exp[_], y: Exp[_]): (List[Sym[_]], List[Sym[_]]) = {
            val used = (syms(y):::innerScope.flatMap(t => syms(t.rhs))).distinct
            val bound = (syms(x):::innerScope.flatMap(t => t.sym::boundSyms(t.rhs))).distinct
            (bound, used)
          }
          def freeInScope(x: Exp[_], y: Exp[_]): List[Sym[_]] = {
            val (bound, used) = boundAndUsedInScope(x,y)
            used diff bound
          }
          
          override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
            case e@Lambda(fun, x, y) =>
            
              focusBlock(y) {
                var free = freeInScope(x,y)
            
                val sw = new StringWriter
                codegenInner.emitFocused("Anonfun_"+quote(sym), free, x, y)(new PrintWriter(sw))
                classes = sw.toString :: classes
            
                stream.println("val " + quote(sym) + " = new Anonfun_" + quote(sym) + "("+free.map(quote).mkString(",")+")")
              }

            case _ => super.emitNode(sym, rhs)
          }
        }
        val codegen2 = new ScalaGenBla with ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { 
          val IR: self.type = self
          
          override def initialDefs = codegen.availableDefs
          
          def emitFocused[A,B](name: String, params: List[Exp[_]], x: Exp[A], y: Exp[B])(implicit stream: PrintWriter) = {
            // TODO: this is not valid Scala code. the types are missing.
            stream.println("class "+name+"("+params.map(quote).mkString(",")+") {")
            stream.println("def apply("+quote(x)+") = {")
            emitBlockFocused(y)
            stream.println(quote(getBlockResult(y)))
            stream.println("}")
            stream.println("}")
          }
        }
        val codegenInner: ScalaGenBla { val IR: self.type } = codegen2
        codegen.emitScalaSource(test, "Test", new PrintWriter(System.out))
        classes.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"lambdalift1")
  }
  
}