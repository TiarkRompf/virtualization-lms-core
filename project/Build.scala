import sbt._
import java.io.File

object LMSBuild extends Build {
  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0")

  lazy val lms = Project("LMS", file("."))
}
