name := "LMS"

version := "0.3-SNAPSHOT"

organization := "EPFL"

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
