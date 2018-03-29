package scala.lms
package internal

trait Config {
  var verbosity = System.getProperty("lms.verbosity","0").toInt
  var sourceinfo = System.getProperty("lms.sourceinfo","0").toInt
  val addControlDeps = System.getProperty("lms.controldeps","true").toBoolean
  val sanitychecks = System.getProperty("lms.sanitychecks","0").toInt
  val unsafeopt = System.getProperty("lms.unsafeopt","0").toInt
}
