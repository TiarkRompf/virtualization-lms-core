package scala.lms
package util

import scala.reflect.{SourceContext, RefinedManifest}
import scala.lms.common._
import scala.lms.internal._

import scala.collection.mutable.HashSet

import java.io.PrintWriter

trait Timing extends Base {
  def timeGeneratedCode[A: Manifest](f: => Rep[A], msg: Rep[String] = unit("")): Rep[A]
}

trait TimingExp extends BaseExp with EffectExp {

  val localSet = new HashSet[String]()

  case class TimeGeneratedCode[A: Manifest](start: Exp[Long], end: Exp[Long], f: Block[A], msg: Rep[String] = unit("")) extends Def[A] {
    localSet += "<sys/time.h>"
    val diff = fresh[Long]
  }

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

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(TimeGeneratedCode(s,e,body,msg),u,ef) => reflectMirrored(Reflect(TimeGeneratedCode(f(s),f(e),f(body),f(msg)), mapOver(f,u), f(ef)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenTiming extends ScalaGenBase with GenericNestedCodegen {
  val IR: TimingExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case TimeGeneratedCode(start, end, f, msg) =>
      stream.println("val " + quote(start) + " = System.nanoTime")
      if (sym.tp != manifest[Unit])
        stream.print("val " + quote(sym) + " = { ")
      emitBlock(f)
      stream.println(quote(getBlockResult(f)))
      if (sym.tp != manifest[Unit])
        stream.println("}")
      stream.println("val " + quote(end) + " = System.nanoTime")
      stream.print("System.out.println(\"Generated Code Profiling Info: Operation " + quote(msg).replaceAll("\"","") + " completed")
      val calcStr = "((" + quote(end) + "-" + quote(start) + ")/(1000*1000))"
      stream.println(" in \" + " + calcStr + " + \" milliseconds\")")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CGenTiming extends CGenBase with GenericNestedCodegen {
  val IR: TimingExp
  import IR._

  override def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]) = rhs match {
    case TimeGeneratedCode(start, end, f, msg) => {
      LIRTraversal(f)
      sym.atPhase(LIRLowering) {
        reflectEffect(TimeGeneratedCode(start, end, LIRLowering(f), msg)).asInstanceOf[Exp[T]]
      }
    }
    case _ => super.lowerNode(sym, rhs)
  }

  override def headerSet = super.headerSet ++ localSet

  // override def emitFunctions(out: PrintWriter) = {
  //   out.println(
  //     """|int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1) {
  //       |long int diff = (t2->tv_usec + 1000000 * t2->tv_sec) - (t1->tv_usec + 1000000 * t1->tv_sec);
  //       |result->tv_sec = diff / 1000000;
  //       |result->tv_usec = diff % 1000000;
  //       |return (diff<0);
  //     |}""".stripMargin)
  //   super.emitFunctions(out)
  // }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  {
    rhs match {
      case t@TimeGeneratedCode(start, end, f, Const(msg)) =>
        gen"struct timeval t_$start, t_$end;"
        gen"gettimeofday(&t_$start, NULL);"
        emitBlock(f)
        gen"gettimeofday(&t_$end, NULL);"
        emitValDef(t.diff, src"t_$end.tv_usec + 1000000L * t_$end.tv_sec - (t_$start.tv_usec + 1000000L * t_$start.tv_sec)")
        gen"""fprintf(stderr,"$msg: Generated Code Profiling Info: Operation completed in %ld milliseconds\n", ${t.diff}/1000L);"""
      case _ => super.emitNode(sym, rhs)
    }
  }
}
