name := "macro-LMS"

version := "1.0.0-wip-macro"

organization := "EPFL"

scalaVersion := "2.11.2"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value % "compile"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile"

libraryDependencies ++= Seq(
  "org.scala-lang.virtualized" %% "scala-virtualized" % "1.0.0-macrovirt"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false


publishArtifact in (Test, packageBin) := true

// continuations
val contVersion = "1.0.2"

autoCompilerPlugins := true

libraryDependencies ++= Seq(
  "org.scala-lang.plugins" %% "scala-continuations-library" % contVersion % "compile"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
     deps :+ compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin" % contVersion cross CrossVersion.full)
}

//fork := true
//connectInput := true
//outputStrategy := Some(StdoutOutput)

scalacOptions += "-P:continuations:enable"

val paradiseVersion = "2.0.1"

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
