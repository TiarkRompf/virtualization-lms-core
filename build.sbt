// --- project info ---
lazy val macros = (project in file("macros"))

lazy val lms = Project("LMS", file("."))
	.dependsOn(macros)
	.settings(
		// include the macro classes and resources in the main jar
		mappings in (Compile, packageBin) ++= mappings.in(macros, Compile, packageBin).value,
		// include the macro docs in the main dpcs jar
		mappings in (Compile, packageDoc) ++= mappings.in(macros, Compile, packageDoc).value,
		// include the macro sources in the main source jar
		mappings in (Compile, packageSrc) ++= mappings.in(macros, Compile, packageSrc).value)



envVars := Map("showSuppressedErrors" -> "false", "showTimings" -> "false")

name := "lms-core-macrovirt"

organization := "org.scala-lang.lms"

description := "Lightweight Modular Staging"

homepage := Some(url("https://scala-lms.github.io"))

licenses := List("BSD-like" -> url("https://github.com/TiarkRompf/virtualization-lms-core/blob/develop/LICENSE"))

scmInfo := Some(ScmInfo(url("https://github.com/TiarkRompf/virtualization-lms-core"), "git@github.com:TiarkRompf/virtualization-lms-core.git"))

// developers := List(Developer("tiarkrompf", "Tiark Rompf", "@tiarkrompf", url("http://github.com/tiarkrompf")))


// --- scala settings ---

scalaVersion := "2.11.8"

scalaOrganization := "org.scala-lang"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test-src"


// --- dependencies ---

libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value % "compile"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile"

libraryDependencies += ("org.scalatest" %% "scalatest" % "2.2.2" % "test")


// continuations
val contVersion = "1.0.2"

autoCompilerPlugins := true

libraryDependencies ++= Seq(
  "org.scala-lang.plugins" %% "scala-continuations-library" % contVersion % "compile"
)

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")

scalacOptions += "-P:continuations:enable"


// macro paradise
val paradiseVersion = "2.0.1"

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)


// --- testing ---

// tests are not thread safe
parallelExecution in Test := false

// code coverage
scoverage.ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting := false
