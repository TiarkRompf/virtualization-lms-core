// maven publishing
publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  val repo = if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots" at nexus + "content/repositories/snapshots"
  else
    "releases" at nexus + "service/local/staging/deploy/maven2"
  Some(repo)
}

releasePublishArtifactsAction := PgpKeys.publishSigned.value

sonatypeProfileName := "org.scala-lang"

// Invalid POM: Project URL missing, License information missing, SCM URL missing, Developer information missing

description := "Lightweight Modular Staging"

homepage := Some(url("https://scala-lms.github.io"))

licenses := List("BSD-like" -> url("http://github.com/TiarkRompf/virtualization-lms-core/tree/master/LICENSE"))

scmInfo := Some(ScmInfo(url("https://github.com/TiarkRompf/virtualization-lms-core"), "git@github.com:TiarkRompf/virtualization-lms-core.git"))

// developers := List(Developer("tiarkrompf", "Tiark Rompf", "@tiarkrompf", url("http://github.com/tiarkrompf")))

pomExtra in Global := {
  <developers>
    <developer>
      <id>tiarkrompf</id>
      <name>Tiark Rompf</name>
      <url>https://github.com/tiarkrompf</url>
    </developer>
  </developers>
}

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

// NOTE: sonatypeRelease must be run explicitly, after `sbt release`