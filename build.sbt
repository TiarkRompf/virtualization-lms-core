name := "virtualization-lms-core"

organization := "scala"

version := "0.1"

//--- Local Scala

scalaHome <<= baseDirectory { f =>
  val props = new java.util.Properties()
  IO.load(props, f / "local.properties")
  val x = props.getProperty("scala.virtualized.home")
  if (x == null)
    sys.error("Did you forget to set scala.virtualized.home property in local.properties file?")
  else Some(file(x))
}

scalacOptions in Test <+= scalaHome map (_.map(f => "-Xplugin:"+f+"/misc/scala-devel/plugins/continuations.jar").get)

//--- End of Local Scala

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

//--- Paths

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

resourceDirectory in Compile <<= baseDirectory(_ / "resources")

scalaSource in Test <<= baseDirectory(_ / "test-src")

resourceDirectory in Test <<= baseDirectory(_ / "test-resources")

//--- End of Paths

//--- Dependencies

resolvers ++= Seq(
    ScalaToolsSnapshots,
    //needed for custom build of scala test
    "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
    )

libraryDependencies += "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.0-virtualized-SNAPSHOT"

//--- End of Dependencies

scalacOptions += "-deprecation"

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

//continuations plugin

//Uncomment the following two lines if you are not using local Scala

//autoCompilerPlugins := true

//addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0-virtualized-SNAPSHOT")

scalacOptions in Test += "-P:continuations:enable"
