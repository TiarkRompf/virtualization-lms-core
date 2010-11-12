package scala.virtualization.lms
package internal

object Config {
  val build_dir = System.getProperty("embedding-build-dir", "embedding-gen/")
  val gen_kernels : Boolean = java.lang.Boolean.parseBoolean(System.getProperty("embedding-gen-kernels", "false"))
}