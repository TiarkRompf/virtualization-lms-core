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
You will need version 0.12.0 of [sbt-launch.jar](http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.12.0/sbt-launch.jar).
Follow the [installation instructions](http://www.scala-sbt.org/download.html#manual) on the SBT website.

2. Run `sbt test` to run the test suite.

3. Run `sbt publish-local` to install LMS-Core for use in other projects.


### Structure

Here below, you will find some information about the structure of LMS organization.
This is to help with finding files and implementation details, as well as knowing
where and how to implement new features to improve LMS.

    src/main/scala/lms/
     +--internal/                 The sources of the code LMS library. To modify/extend LMS
     +--util/                     Utilities used by LMS
     +--ops/                      The sources of ops provided by LMS. The main entry point for DSL developers.
     |   |                          Note: The Scala generation traits reside inside the ops source files.
     |   +--collection/           The sources of collection ops
     +--transformers/             The sources of transformers.
     +--targets                   The sources of target specific code.
     |   +--clike
     |   |   +--ops
     |   |   +--codegen
     |   +--hdl
     |   |   +--ops
     |   |   +--codegen
     |   +--javascript
     |   |   +--ops
     |   |   +--codegen
     +--test-out/                 The test output files.
    src/test/scala/lms            The sources of the test files
     +--examples                  The sources of examples.


### Generating documentation

1. Install the latest [Sphinx framework](http://sphinx-doc.org/)

2. Run `docs/test` and if all tests pass `docs/sphinx:generate`
