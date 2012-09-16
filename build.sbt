name := "LMS"

version := "0.3-SNAPSHOT"

organization := "EPFL"

resolvers += ScalaToolsSnapshots

//resolvers += dropboxScalaTestRepo

resolvers += prereleaseScalaTest

//scalaHome := Some(file("/Users/tiark/scala/build/pack"))

scalaOrganization := "org.scala-lang.virtualized"

//scalaBinaryVersion := virtScala // necessary??

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)

// needed for scala.tools, which is apparently not included in sbt's built in version
libraryDependencies += "org.scala-lang" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala

libraryDependencies += scalaTest

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % virtScala)

scalacOptions += "-P:continuations:enable"
