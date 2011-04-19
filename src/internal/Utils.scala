package scala.virtualization.lms
package internal

// TODO: add logging, etc.
trait Utils extends Config {
  def __ = throw new RuntimeException("unsupported embedded dsl operation")

  def printdbg(x: =>Any) { if (verbosity >= 2) println(x) }
  def printlog(x: =>Any) { if (verbosity >= 1) println(x) }
  def printerr(x: =>Any) { println(x) }
}