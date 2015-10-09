package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import java.nio.file.{Files,Paths}
import scala.io.Source

// TODO: add logging, etc.
trait Utils extends Config {
  def __ = throw new RuntimeException("unsupported embedded dsl operation")

  def printdbg(x: =>Any) { if (verbosity >= 2) System.err.println(x) }
  def printlog(x: =>Any) { if (verbosity >= 1) System.err.println(x) }
  def printerr(x: =>Any) { System.err.println(x); hadErrors = true }

  def printsrc(x: =>Any) { if (sourceinfo >= 1) System.err.println(x) }

  def printwarn(x: =>Any) { if (verbosity >= 1) System.err.println(x) }

  var hadErrors = false

  // Hacks for mirroring, etc.
  def mtype[A,B](m: Manifest[A]): Manifest[B] = m.asInstanceOf[Manifest[B]]
  def mpos(s: List[SourceContext]): SourceContext = if (s.nonEmpty) s.head else implicitly[SourceContext]

  // TODO: Better way to do this? manifest <:< comparisons seem to fail
  def isSubtype(x: java.lang.Class[_], cls: java.lang.Class[_]): Boolean = {
    if ((x == cls) || x.getInterfaces().contains(cls)) true
    else if (x.getSuperclass() == null && x.getInterfaces().length == 0) false
    else {
      val superIsSub = if (x.getSuperclass() != null) isSubtype(x.getSuperclass(), cls) else false
      superIsSub || x.getInterfaces().exists(s=>isSubtype(s,cls))
    }
  }

  protected def all(cs: SourceContext): List[SourceContext] = cs.parent match {
    case None => List(cs)
    case Some(p) => cs::all(p)
  }

  def getPathAndLine(ctx: List[SourceContext]): List[(String,Int)] = {
    ctx.map{c => val top = all(c).last; (top.fileName, top.line) }
  }

  // if only a single SourceContext is given, fetch corresponding source code line
  // from top filename and line number. Otherwise returns None
  def quoteCode(ctx: SourceContext): Option[String] = quoteCode(List(ctx))
  def quoteCode(ctx: List[SourceContext]): Option[String] = {
    val pos = getPathAndLine(ctx)
    if (pos.length == 1) {
      val path = pos.head._1
      val line = pos.head._2

      if (Files.exists(Paths.get(path)))
        Some(Source.fromFile(path).getLines.toList.apply(line - 1))
      else None
    }
    else None
  }

}