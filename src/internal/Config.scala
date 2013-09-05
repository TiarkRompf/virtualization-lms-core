package scala.virtualization.lms
package internal

object Config {
  val verbosity = System.getProperty("lms.verbosity","0").toInt
  val sourceinfo = System.getProperty("lms.sourceinfo","0").toInt
}
