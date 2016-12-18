package scala.virtualization.lms
package internal

// TODO: add logging, etc.
trait Utils {
  def __ = throw new RuntimeException("unsupported embedded dsl operation")

  def printdbg(x: =>Any) { if (Config.verbosity >= 2) System.err.println(x) }
  def printlog(x: =>Any) { if (Config.verbosity >= 1) System.err.println(x) }
  def printerr(x: =>Any) { System.err.println(x); }
  def printsrc(x: =>Any) { if (Config.sourceinfo >= 1) System.err.println(x) }  
}
