Lightweight Modular Staging (LMS) is a runtime code generation approach. 
This framework, LMS-Core, provides a library of core components for building 
high performance code generators and embedded compilers in Scala. 

Closely related projects are [Delite](https://github.com/stanford-ppl/Delite/),
a framework for heterogeneous parallel domain specific languages (DSLs),
and [Scala-Virtualized](https://github.com/tiarkrompf/scala-virtualized/),
a set of minimal extensions to the Scala compiler to make embedding DSLs
more seamless.

### Background:

- [LMS paper](http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf)

- [Delite website](http://stanford-ppl.github.com/Delite/)

- [Scala-Virtualized wiki](https://github.com/TiarkRompf/scala-virtualized/wiki)


### How to build:

1. Install the simple build tool ([SBT](http://www.scala-sbt.org/)). 
You will need a version 0.12.0 of the [sbt-launch.jar](http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.12.0/sbt-launch.jar). 
Follow the [installation instructions](http://www.scala-sbt.org/download.html#manual) on the SBT website.

2. Run `sbt test` to run the test suite.

3. Run `sbt publish-local` to install LMS-Core for use in other projects.
