// --- project info ---

name := "lms-core"

organization := "org.scala-lang.lms"

description := "Lightweight Modular Staging"

homepage := Some(url("https://scala-lms.github.io"))

licenses := List("BSD-like" -> url("https://github.com/TiarkRompf/virtualization-lms-core/blob/develop/LICENSE"))

scmInfo := Some(ScmInfo(url("https://github.com/TiarkRompf/virtualization-lms-core"), "git@github.com:TiarkRompf/virtualization-lms-core.git"))

// developers := List(Developer("tiarkrompf", "Tiark Rompf", "@tiarkrompf", url("http://github.com/tiarkrompf")))


// --- scala settings ---

scalaVersion := virtScala

scalaOrganization := "org.scala-lang.virtualized"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)


// --- dependencies ---

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

// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")

scalacOptions += "-P:continuations:enable"

// --- testing ---

// tests are not thread safe
parallelExecution in Test := false

// code coverage
scoverage.ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting := false
