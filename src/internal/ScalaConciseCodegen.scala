package scala.virtualization.lms
package internal

import java.io.{File, FileWriter, PrintWriter}
import scala.virtualization.lms.util.ReflectionUtil
import scala.reflect.SourceContext

/**
 * ScalaConciseCodegen is just an extension to ScalaCodegen
 * which inlines expressions that are possible to inline,
 * instead of creating a new val-def for each of them, leading
 * to a more compact and concise code.
 * 
 * @author Mohammad Dashti (mohammad.dashti@epfl.ch)
 */
trait ScalaConciseCodegen extends ScalaNestedCodegen { self =>
  val IR: ExtendedExpressions with Effects
  import IR._

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    val extra = if ((sourceinfo < 2) || sym.pos.isEmpty) "" else {
      val context = sym.pos(0)
      "      // " + relativePath(context.fileName) + ":" + context.line
    }
    sym match {
      case s@Sym(n) => isVoidType(s.tp) match {
        case true => stream.println("" + rhs + extra)
        case false => if(s.possibleToInline) {
            stream.print("("+rhs+")")
          } else {
            stream.println("val " + quote(sym) + " = " + rhs + extra)
          }
      }
      case _ => stream.println("val " + quote(sym) + " = " + rhs + extra)
    }
  }
  
  override def emitAssignment(sym: Sym[Any], lhs: String, rhs: String): Unit = {
    if(isVoidType(sym.tp)) {
      stream.println(lhs + " = " + rhs)
    } else {
      emitValDef(sym, lhs + " = " + rhs)
    }
  }
  
  override def emitForwardDef(sym: Sym[Any]): Unit = {
    if(!isVoidType(sym.tp)) { stream.println("var " + quote(sym, true) + /*": " + remap(sym.tp) +*/ " = null.asInstanceOf[" + remap(sym.tp) + "]") }
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) => if(!sym.possibleToInline && sym.refCount > 0 /*for eliminating read-only effect-ful statements*/) emitNode(sym,rhs)
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }

  override def quote(x: Exp[Any], forcePrintSymbol: Boolean) : String = {
    def printSym(s: Sym[Any]): String = {
      if(s.possibleToInline) {
        Def.unapply(s) match {
          case Some(d: Def[Any]) => {
            val strWriter: java.io.StringWriter = new java.io.StringWriter;
            val stream = new PrintWriter(strWriter);
            withStream(stream) { 
              emitNode(s, d)
            }
            strWriter.toString
          }
          case None => "x"+s.id
        }
      } else {
        "x"+s.id
      }
    }
    x match {
      case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
      case Const(c: Char) => "'"+c+"'"
      case Const(f: Float) => "%1.10f".format(f) + "f"
      case Const(l: Long) => l.toString + "L"
      case Const(null) => "null"
      case Const(z) => z.toString
      case s@Sym(n) => if (forcePrintSymbol) {
        printSym(s)
      } else { 
        isVoidType(s.tp) match {
          case true => "(" + /*"x" + n +*/ ")"
          case false => printSym(s)
        }
      }
      case _ => throw new RuntimeException("could not quote " + x)
    }
  }

  override def performTransformations[A:Manifest](body: Block[A]): Block[A] = {
    val transformedBody = super.performTransformations[A](body)
    val fixer = new SymMetaDataFixerTransform{ val IR: self.IR.type = self.IR }
    fixer.traverseBlock(transformedBody.asInstanceOf[fixer.Block[A]])
    transformedBody
  }

}