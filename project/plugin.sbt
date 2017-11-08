resolvers += Classpaths.sbtPluginReleases

// test coverage

// addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.1")

// addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.0.0.BETA1")

// release process

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")