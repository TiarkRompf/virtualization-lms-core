name := "LMS"

version := "0.1"

organization := "EPFL"

resolvers += ScalaToolsSnapshots

resolvers += dropboxScalaTestRepo

scalaVersion := virtScala

// quick way of getting artifact naming compatibility right now
//crossPaths := false
//scalaBinaryVersion := virtScala // necessary??

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize" 

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

scalacOptions in Test += "-P:continuations:enable"

EclipseKeys.withSource := true

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE16)

// Get rid of java source directories in compile
unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

// Get rid of java source directories in test
unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))
