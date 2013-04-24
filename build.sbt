name := "scala.lms"

version := "0.4-SNAPSHOT"

organization := "ch.epfl"

//resolvers += Resolver.sonatypeRepo("snapshots") // for ScalaTest


//scalaHome := Some(file(Path.userHome + "/scala/build/pack"))

scalaOrganization := "org.scala-lang.virtualized"

//scalaBinaryVersion := virtScala // necessary??

scalaVersion := virtScala

//scalaSource in Compile <<= baseDirectory(_ / "src")

//scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)


// needed for scala.tools, which is apparently not included in sbt's built in version
libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % virtScala

libraryDependencies += "org.scala-lang" % "scala-actors" % virtScala // for ScalaTest

libraryDependencies += scalaTest


// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false


// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % virtScala)

scalacOptions += "-P:continuations:enable"
