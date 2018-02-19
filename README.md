[![Build Status](https://travis-ci.org/TiarkRompf/virtualization-lms-core.svg?branch=develop-macrovirt-0.9.x)](https://travis-ci.org/TiarkRompf/virtualization-lms-core)
[![Maven Central](https://img.shields.io/maven-central/v/org.scala-lang.lms/lms-core_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/org.scala-lang.lms/lms-core_2.11)

Lightweight Modular Staging (LMS) is a runtime code generation approach. 
This framework, LMS-Core, provides a library of core components for building 
high performance code generators and embedded compilers in Scala. 

### Background:

- [LMS website](http://scala-lms.github.io)

- [LMS paper](http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf)

- [Delite website](http://stanford-ppl.github.com/Delite/)


### How to use:

With [SBT](http://www.scala-sbt.org/), add the following to your `build.sbt`:

    resolvers += Resolver.sonatypeRepo("snapshots")
    
    libraryDependencies += "org.scala-lang.lms" %% "lms-core-macrovirt" % "0.9.0-SNAPSHOT"

See the [LMS tutorials repo](https://github.com/scala-lms/tutorials/tree/macro-lms-0.9.x) for code examples.

### How to build:

1. Install the [SBT](http://www.scala-sbt.org/) build tool.

2. Run `sbt test` to run the test suite.

3. Run `sbt publish-local` to install LMS-Core for use in other projects.


### License:

Copyright 2010-2014, EPFL; 2014-2017, Purdue University. Licensed under the revised BSD License.
