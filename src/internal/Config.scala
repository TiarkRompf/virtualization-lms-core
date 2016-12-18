package scala.lms
package internal

object Config {
  var verbosity = System.getProperty("lms.verbosity","0").toInt
  var sourceinfo = System.getProperty("lms.sourceinfo","0").toInt
}
