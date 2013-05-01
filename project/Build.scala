import sbt._
import Keys._
import java.io.File
import com.typesafe.sbt.{SbtSite, site => sbtsite}
import SbtSite.{ site }
import com.typesafe.sbt.site.SphinxSupport.Sphinx
import com.typesafe.sbt.SbtGhPages.ghpages
import com.typesafe.sbt.SbtGit.git

object LMSBuild extends Build {

  // -DshowSuppressedErrors=false
  System.setProperty("showSuppressedErrors", "false")

  lazy val mavenLocal = "Maven Local" at "file://"+Path.userHome+"/.m2/repository" // for custom-built scala version

  //val prereleaseScalaTest = "Pre-Release ScalaTest" at "https://scala-webapps.epfl.ch/jenkins/view/2.10.x/job/community-nightly-2.10.0/ws/target/repositories/bccb0f4eb0bf6024577064915da437e3574281cb/"

  lazy val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.1")

  lazy val buildSettings = Seq(
    name         := "lms",
    organization := "ch.epfl",
    version      := "0.4-SNAPSHOT"
  )

  // settings for Virtualized Scala
  lazy val virtSettings = Seq(
    resolvers += ScalaToolsSnapshots,
    scalaOrganization := "org.scala-lang.virtualized",
    scalaVersion := virtScala,
    scalacOptions += "-Yvirtualize",
    // tests are not thread safe
    parallelExecution in Test := false,
    libraryDependencies += scalaTest,
    libraryDependencies += "org.scala-lang" % "scala-actors" % virtScala, // for ScalaTest
    // needed for scala.tools, which is apparently not included in sbt's built in version
    libraryDependencies += "org.scala-lang" % "scala-library" % virtScala,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala
  )

  // continuations plugin
  lazy val continuationsSettings = Seq(
   autoCompilerPlugins := true,
   addCompilerPlugin("org.scala-lang.plugins" % "continuations" % virtScala),
   scalacOptions += "-P:continuations:enable"
  )

  lazy val lms = Project(
    id = "lms",
    base = file("."),
    settings = Project.defaultSettings ++ buildSettings ++ virtSettings ++ continuationsSettings ++ Seq (
      // disable publishing of main docs
      publishArtifact in (Compile, packageDoc) := false
    )
  )

  // documentation
  lazy val siteSettings = site.settings ++ site.sphinxSupport()

  // documentation settings
  val docsSettings = Seq(
    scalaSource in Test <<= baseDirectory(_ / "src/sphinx/code"),
    parallelExecution in Test := false, // tests are not thread-safe
    includeFilter in Sphinx := ("*.html" | "*.png" | "*.js" | "*.css" | "*.gif" | "*.txt"),
    git.remoteRepo := "git@github.com:TiarkRompf/virtualization-lms-core.git"
  ) ++ ghpages.settings

  lazy val docs = Project(
    id = "docs",
    base = file("./docs"),
    settings = Project.defaultSettings ++ virtSettings ++ continuationsSettings ++ siteSettings ++ docsSettings
  ) dependsOn (lms)

}
