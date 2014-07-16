import sbt._
import java.io.File

object LMSBuild extends Build {
  System.setProperty("showSuppressedErrors", "false")

  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.1.2"
  
  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.2")

  lazy val lms = Project("LMS", file("."))
}
