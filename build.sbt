name := "LMS"

version := "0.3-SNAPSHOT"

organization := "EPFL"

scalaOrganization := "org.scala-lang" // "org.scala-lang.virtualized"

scalaVersion := virtScala

// BRING YOUR OWN SCALAC:
// this should point to a scala compiler built from this repo/branch:
// https://github.com/TiarkRompf/scala/tree/topic-virt-2.11.2

scalaHome := Some(file("/Users/me/scala/build/pack/"))

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)


libraryDependencies += "org.scala-lang" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala

libraryDependencies += "org.scala-lang.plugins" % "scala-continuations-library_2.11" % "1.0.2"


libraryDependencies += scalaTest


// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false


// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")

scalacOptions += "-P:continuations:enable"
