name := "LMS"

organization := "org.scala-lang.virtualized"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)

libraryDependencies += ("org.scala-lang.virtualized" % "scala-library" % virtScala)

// Transitive dependency through scala-continuations-library
libraryDependencies += ("org.scala-lang.virtualized" % "scala-compiler" % virtScala).
  exclude ("org.scala-lang", "scala-library").
  exclude ("org.scala-lang", "scala-compiler")

libraryDependencies += ("org.scala-lang.plugins" % "scala-continuations-library_2.11" % "1.0.2").
  exclude ("org.scala-lang", "scala-library").
  exclude ("org.scala-lang", "scala-compiler")

libraryDependencies += ("org.scalatest" % "scalatest_2.11" % "2.2.2").
  exclude ("org.scala-lang", "scala-library").
  exclude ("org.scala-lang", "scala-compiler").
  exclude ("org.scala-lang", "scala-reflect")

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false


// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")

scalacOptions += "-P:continuations:enable"

// code coverage
scoverage.ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting := false

// maven publishing
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  val repo = if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots" at nexus + "content/repositories/snapshots"
  else
    "releases" at nexus + "service/local/staging/deploy/maven2"
  Some(repo)
}
