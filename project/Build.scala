import sbt._
import java.io.File

object LMSBuild extends Build {

  // -DshowSuppressedErrors=false
  System.setProperty("showSuppressedErrors", "false")

  // FIXME: custom-built scalatest
  //val dropboxScalaTestRepo = "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized
  
  val mavenLocal = "Maven Local" at "file://"+Path.userHome+"/.m2/repository"  //"file:///Users/me/scala/dists/maven/2.10.0-20121008-191448-74daa02069" //

  val prereleaseScalaTest = "Pre-Release ScalaTest" at "https://scala-webapps.epfl.ch/jenkins/view/2.10.x/job/community-nightly-2.10.0/ws/target/repositories/bccb0f4eb0bf6024577064915da437e3574281cb/"

  //val prereleaseScalaTest = "SonatypeX" at "https://oss.sonatype.org/content/groups/public" // "org/scalatest/scalatest_2.10.0-M5/1.9-2.10.0-M5-B2/"
  
  //val scalaTest = "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test"
  // TODO: use M7 version
  
  //https://scala-webapps.epfl.ch/jenkins/view/2.10.x/job/community-nightly-2.10.0/ws/target/repositories/bccb0f4eb0bf6024577064915da437e3574281cb/org/scalatest/scalatest/1.9-2.10.0-M6-B2/

  val scalaTest = "org.scalatest" % "scalatest" % "1.9-2.10.0-M6-B2" % "test" //"org.scalatest" % "scalatest_2.10.0-M7" % "1.9-2.10.0-M7-B1" % "test"
  
  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M7")

  lazy val lms = Project("LMS", file("."))
}
