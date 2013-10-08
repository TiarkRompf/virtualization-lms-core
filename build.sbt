name := "LMS"

version := "0.3-SNAPSHOT"

organization := "EPFL"

//resolvers += Resolver.sonatypeRepo("snapshots") // for ScalaTest


//scalaHome := Some(file(Path.userHome + "/scala/build/pack"))

scalaOrganization := "org.scala-lang.virtualized"

//scalaBinaryVersion := virtScala

scalaVersion := "2.10.2-RC1"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

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

testGrouping <<= definedTests in Test map { tests =>
  tests.map { test =>
    import Tests._
    new Group(
      name = test.name,
      tests = Seq(test),
      runPolicy = InProcess)
  }.sortWith(_.name < _.name)
}

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false


// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % virtScala)

scalacOptions += "-P:continuations:enable"
