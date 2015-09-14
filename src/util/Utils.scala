package scala.lms
package util

import scala.reflect.SourceContext

trait Utils extends Config {
  val testsuite = false
  private var _hadErrors = false
  def hadErrors = _hadErrors
  def resetErrors() {_hadErrors = false}
  
  def cmsg(x: =>Any) { System.out.println(x) }
  def cdbg(x: =>Any) { if (verbosity >= 2) System.err.println(x) }
  def clog(x: =>Any) { if (verbosity >= 1) System.err.println(x) }
  def cerror(x: =>Any) { System.err.println("[\u001B[31merror\u001B[0m] " + x); _hadErrors = true }
  def cassert(cond: Boolean, x: => Any) { if (!cond) cerror(x) }
  def cwarn(x: =>Any) { System.err.println("[\u001B[33mwarn\u001B[0m] " + x) }
  def ccheck(cond: Boolean, x: => Any) { if (!cond) cwarn(x) }

  def cfatal(x: =>Any): Nothing = { 
    if (!testsuite) { cerror(x); sys.exit() }
    else { throw new Exception(x.toString) }
  }

  private var debugging: Boolean = false
  def inDebugMode[A](block: => A): A = {
    val prevMode = debugging
    debugging = true
    val out = block
    debugging = prevMode
    (out)
  }
  
  def printDebug(x: => Any) { if (debugging) cmsg(x) }
  
  // Type X or Some[X]
  object EatSome {
    def unapply(x: Any): Option[Any] = x match {
      case Some(y) => Some(y)
      case None => None
      case _ => Some(x)
    }
  }
  
  // --- SourceContext
  def all(cs: SourceContext): List[SourceContext] = cs.parent match {
    case None => List(cs)
    case Some(p) => cs::all(p)
  }
  def getFirstStack(cs: SourceContext): SourceContext = cs.parent match {
    case None => cs
    case Some(p) => getFirstStack(p)
  }
  
  def getPathAndLine(ctx: List[SourceContext]): List[(String,Int)] = {
    ctx.map{c => val top = all(c).last; (top.fileName, top.line) }
  }
  
  def quoteCtx(ctx: SourceContext): String = quoteCtx(List(ctx))

  // Fetch full top filename and line number for each SourceContext separated by semicolons
  def quoteCtx(ctx: List[SourceContext]): String = getPathAndLine(ctx) match {
    case Nil => "<unknown>"
    case cs => cs.map(p => p._1 + ":" + p._2).distinct.mkString(";")
  }
  
  // if only a single SourceContext is given, fetch corresponding source code line
  // from top filename and line number. Otherwise returns None
  // FIXME: This is extremely kludgey right now
  def quoteCode(ctx: SourceContext): Option[String] = quoteCode(List(ctx))
  def quoteCode(ctx: List[SourceContext]): Option[String] = {
    val pos = getPathAndLine(ctx)
    if (pos.length == 1) { 
      None
      //Some(Source.fromFile(pos.head._1).getLines().toList.apply(pos.head._2 - 1))
    }
    else None
  }
}