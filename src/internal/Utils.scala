package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import scala.io.Source


// TODO: add logging, etc.
trait Utils extends Config {

  def __ = throw new RuntimeException("unsupported embedded dsl operation")

  def printmsg(x: =>Any) { System.out.println(x) }
  def printdbg(x: =>Any) { if (verbosity >= 2) System.err.println(x) }
  def printlog(x: =>Any) { if (verbosity >= 1) System.err.println(x) }
  def printerr(x: =>Any) { System.err.println(x); if (testsuite) System.out.println(x); _hadErrors = true }

  def printsrc(x: =>Any) { if (sourceinfo >= 1) System.err.println(x) }
  
  def cerr(x: => Any) { System.err.println("[\u001B[31merror\u001B[0m] " + x); _hadErrors = true}
  def cerr(cond: Boolean, x: => Any) { if (!cond) cerr(x) }

  def cwarn(x: =>Any) { System.err.println("[\u001B[33mwarn\u001B[0m] " + x) }
  def cwarn(cond: Boolean, x: => Any) { if (!cond) cwarn(x) }

  def fatalerr(x: =>Any) { 
    if (!testsuite) { printerr(x); sys.exit() }
    else { throw new Exception(x.toString) }
  }

  private var _hadErrors = false
  def hadErrors = _hadErrors
  def resetErrors() {_hadErrors = false}

  private var debugging: Boolean = false
  def inDebugMode[A](block: => A): A = {
    val prevMode = debugging
    debugging = true
    val out = block
    debugging = prevMode
    (out)
  }
  
  def printDebug(x: => Any) { if (debugging) printmsg(x) }
}

trait UtilsExp extends Utils {this: Expressions =>

  def strDef(e: Exp[Any]): String = e match {
    case Const(z) => z.toString
    case Def(d) => e.toString + " = " + d.toString
    case e: Exp[_] => "(bound " + e.toString + ")"
  }

  private def all(cs: SourceContext): List[SourceContext] = cs.parent match {
    case None => List(cs)
    case Some(p) => cs::all(p)
  }

  def getPathAndLine(ctx: List[SourceContext]): List[(String,Int)] = {
    ctx.map{c => val top = all(c).last; (top.fileName, top.line) }
  }

  def quoteSymPos(symPos: List[SourceContext], defPos: List[SourceContext]): String = {
    val opPaths = getPathAndLine(defPos).map{_._1}
    val symPaths = getPathAndLine(symPos).map{_._1}
    if (opPaths.zip(symPaths).map{a => a._1 == a._2}.reduce{_&&_})
      "on line " + getPathAndLine(symPos).map{_._2}.mkString(";")
    else
      "at " + quotePos(symPos)
  }

  // if only a single SourceContext is given, fetch corresponding source code line
  // from top filename and line number. Otherwise returns None
  // FIXME: This is extremely kludgey right now
  // Commented out for now
  def quoteCode(ctx: SourceContext): Option[String] = quoteCode(List(ctx))
  def quoteCode(ctx: List[SourceContext]): Option[String] = {
    val pos = getPathAndLine(ctx)
    if (pos.length == 1) { 
      None
      //Some(Source.fromFile(pos.head._1).getLines().toList.apply(pos.head._2 - 1))
    }
    else None
  }

  def quotePos(ctx: SourceContext): String = quotePos(List(ctx))

  // Fetch full top filename and line number for each SourceContext separated by semicolons
  def quotePos(ctx: List[SourceContext]): String = getPathAndLine(ctx) match {
    case Nil => "<unknown>"
    case cs => cs.map(p => p._1 + ":" + p._2).distinct.mkString(";")
  }

  def quotePos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs => 
      cs.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
  }

  def check(cond: Boolean, x: => Any)(implicit ctx: SourceContext) {
    if (!cond) printerr(quotePos(ctx) + ": " + x + quoteCode(ctx).map{"\n\t" + _}.getOrElse(""))
  }

}