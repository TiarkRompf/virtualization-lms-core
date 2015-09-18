package scala.virtualization.lms
package internal

import scala.reflect.SourceContext

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
}