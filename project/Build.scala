import sbt._
import java.io.File

object LMSBuild extends Build {
  System.setProperty("showSuppressedErrors", "false")
  System.setProperty("showTimings", "true")

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.11.2")

  lazy val lms = Project("LMS", file("."))
}
