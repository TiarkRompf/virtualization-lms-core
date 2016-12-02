[![Build Status](https://api.travis-ci.org/TiarkRompf/virtualization-lms-core.png)](https://travis-ci.org/TiarkRompf/virtualization-lms-core)

Lightweight Modular Staging (LMS) is a runtime code generation approach. 
This framework, LMS-Core, provides a library of core components for building 
high performance code generators and embedded compilers in Scala. 

Closely related projects are [Delite](https://github.com/stanford-ppl/Delite/),
a framework for heterogeneous parallel domain specific languages (DSLs),
and [Scala-Virtualized](https://github.com/tiarkrompf/scala-virtualized/),
a set of minimal extensions to the Scala compiler to make embedding DSLs
more seamless.

### NOTE:

This is branch `macro-trans`. It uses an unmodified Scala 2.11 compiler and
a set of macros to virtualize conditionals, variables, etc.

All of this is still experimental.

- Get and install the Scala-Virtualized macros:
  - `git clone -b scala-virtualized https://github.com/scala-lms/summer-of-lms-2014.git scala-virtualized`
  - `cd scala-virtualized; sbt publish-local`

- See here for the diff to `develop`:
  - [compare/develop...macro-trans](https://github.com/TiarkRompf/virtualization-lms-core/compare/develop...macro-trans)


### Background:

- [LMS website](http://scala-lms.github.io)

- [LMS paper](http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf)

- [Delite website](http://stanford-ppl.github.com/Delite/)

- [Scala-Virtualized wiki](https://github.com/TiarkRompf/scala-virtualized/wiki)


### How to build:

1. Install the [SBT](http://www.scala-sbt.org/) build tool.

2. Run `sbt test` to run the test suite.

3. Run `sbt publish-local` to install LMS-Core for use in other projects.


### License:

Copyright 2010-2014, EPFL. Licensed under the revised BSD License.
