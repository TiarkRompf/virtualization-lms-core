package scala.virtualization.lms
package internal

trait Config {
  val verbosity = System.getProperty("lms.verbosity","0").toInt
  val sourceinfo = System.getProperty("lms.sourceinfo","0").toInt
  val addControlDeps = System.getProperty("lms.controldeps","false").toBoolean

  // memory management type for C++ target (refcnt or gc)
  val cppMemMgr = System.getProperty("lms.cpp.memmgr","malloc")
}
