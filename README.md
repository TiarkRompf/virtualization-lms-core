This is the common embedding framework portion of the collaboration effort between PPL and EPFL,
with some minor tweaks for use with [JavaScript as an embedded DSL in Scala](http://github.com/namin/lms-sandbox). 

### Setup:

1. Setup Scala-Virtualized. From [`scala`](http://github.com/namin/scala):
  * Run `git checkout js`.
  * Run `ant`.
  * In case of failure, run `ant all.clean` and `./pull-binary-libs.sh`, then `ant` again.

2. Create a file `local.properties` in the root project directory, containing the following line:

          scala.virtualized.home=<downloadpath>/build/pack
    
    Instead of `<downloadpath>` of course you use the actual path to your Scala-Virtualized repository.
      
3. Run `sbt`. `test` to ensure everything works. Then `publish-local`.