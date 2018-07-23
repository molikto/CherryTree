# Cherry Tree


compile/run with `sbt ~reStart` (for now)


test with `sbt sharedJVM/test`, `sbt sharedJVM/testOnly xxx` etc.

release build with `sbt assembly` for the jar. more info here: https://github.com/vmunier/akka-http-scalajs.g8

## coding guideline

* static assertion is by assert and doc
* dynamic check use exception


## read the code?

* `model`: abstract and pure functions
    * `data`: data object definition, serialization
    * `cursor`: data type for cursor inside data
    * `range`: data type for range inside data, from left to right style
    * `operation`: operation definition on data
    * `conflict`, `transaction`: helper definitions
    * `ot` operational transformation for operations
* `server`: abstract server implementation
* `web` the web client
