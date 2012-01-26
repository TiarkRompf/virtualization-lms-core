name := "LMS"

version := "0.1"

organization := "EPFL"

resolvers += ScalaToolsSnapshots

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize" 

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)

// disable publishing of main docs 
publishArtifact in (Compile, packageDoc) := false

// needed for scala.tools, which is apparently not included in sbt's built in version
libraryDependencies += "org.scala-lang" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala

resolvers += dropboxScalaTestRepo

libraryDependencies += scalaTest

parallelExecution in Test := false

// continuations plugin
//autoCompilerPlugins := true
//addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0-virtualized-SNAPSHOT")

// TODO: scalaHome appears to be undefined by default (how do we get the scalac path?)
//       and even if it was fixed, the scalatools published version of scala-virtualized needs to include the continuations jar for this to work
//scalacOptions in Test <++= (scalaHome) map { home => Seq("-Xplugin:" + home +"/misc/scala-devel/plugins/continuations.jar", "-P:continuations:enable") }

//scalacOptions in Test += "-P:continuations:enable"
