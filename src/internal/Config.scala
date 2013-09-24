package scala.virtualization.lms
package internal

trait Config {
  val verbosity = System.getProperty("lms.verbosity","0").toInt
  val sourceinfo = System.getProperty("lms.sourceinfo","0").toInt
  val addControlDeps = System.getProperty("lms.controldeps","true").toBoolean
  val sanitychecks = System.getProperty("lms.sanitychecks","0").toBoolean
}