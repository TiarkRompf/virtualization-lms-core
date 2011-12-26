import sbt._
import java.io.File

object LMSBuild extends Build {
  // FIXME: custom-built scalatest
  val dropboxScalaTestRepo = "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
  val scalaTest = "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" //% "test"
  val virtScala = "2.10.0-virtualized-SNAPSHOT"

  lazy val lms = Project("LMS", file("."))
}
