name := "scala-virtualized"

organization := "org.scala-lang.virtualized"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"

addCompilerPlugin("org.scalamacros" % "paradise_2.12.4" % "2.1.0")

