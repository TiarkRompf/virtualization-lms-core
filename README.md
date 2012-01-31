This is the common embedding framework portion of the collaboration effort between PPL and EPFL. 

### How to build:

1. Download and build [`scala-virtualized`](http://github.com/TiarkRompf/scala-virtualized).
2. Create a file `local.properties` in the root project directory, containing the following line:

          scala.virtualized.home=<downloadpath>/build/pack
    
    Instead of `<downloadpath>` of course you use the actual path.
      
3. Run `sbt`

### How to use Eclipse with virtualization-lms-core:

See [`this page`](http://lamplmscore.epfl.ch/mediawiki/) for instructions.