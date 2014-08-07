name := "macro-LMS"

version := "0.3-SNAPSHOT"

organization := "EPFL"

scalaVersion := "2.11.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")

libraryDependencies ++= Seq(
  "org.scala-lang.virtualized" %% "scala-virtualized" % "0.0.1-SNAPSHOT"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// TODO: how to enable for 2.11?
// continuations
// autoCompilerPlugins := true

// libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
//     deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
// }

// scalacOptions += "-P:continuations:enable"

val paradiseVersion = "2.0.1"

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
