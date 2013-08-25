package scala.virtualization.lms
package util

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

trait Timing extends Base {
    def timeGeneratedCode[A: Manifest](f: => Rep[A], msg: Rep[String] = unit("")): Rep[A]
}

trait TimingExp extends BaseExp with EffectExp {
    case class TimeGeneratedCode[A: Manifest](start: Exp[Long], end: Exp[Long], f: Block[A], msg: Rep[String] = unit("")) extends Def[A]
    
    def timeGeneratedCode[A: Manifest](f: => Rep[A], msg: Rep[String] = unit("")) = {
        val b = reifyEffects(f)
        val start = fresh[Long]
        val end = fresh[Long]
        reflectEffect(TimeGeneratedCode[A](start, end, b, msg), summarizeEffects(b).star)
    }

    override def syms(e: Any): List[Sym[Any]] = e match {
        case TimeGeneratedCode(a, x, body, msg) => syms(body)
        case _ => super.syms(e)
    }

    override def boundSyms(e: Any): List[Sym[Any]] = e match {
        case TimeGeneratedCode(a, x, body, msg) => effectSyms(body)
        case _ => super.boundSyms(e)
    }

    override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
        case TimeGeneratedCode(a, x, body, msg) => freqHot(body)
        case _ => super.symsFreq(e)
    }  
}

trait ScalaGenTiming extends ScalaGenBase with GenericNestedCodegen {
	val IR: TimingExp
	import IR._
 
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  { 
        rhs match {
            case TimeGeneratedCode(start, end, f, msg) => {
                stream.println("val " + quote(start) + " = System.nanoTime")
                stream.print("val " + quote(sym) + " = { ")
                emitBlock(f)
                stream.println(quote(getBlockResult(f)))
                stream.println("}")
                stream.println("val " + quote(end) + " = System.nanoTime")
                stream.print("println(\"Generated Code Profiling Info: Operation " + quote(msg).replaceAll("\"","") + " completed")
                val calcStr = "(" + quote(end) + "-" + quote(start) + ")"
                stream.println(" in \" + " + calcStr + " + \" nanoseconds\")") 
            }
            case _ => super.emitNode(sym, rhs)
  	    }
    }
}
