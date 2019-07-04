# CherryTree


A structural editor for rich text outliners, collaboratively

Initially a project aimed as a commercial software, here is a demo for it: https://www.youtube.com/watch?v=97lAMRrAcF4&feature=youtu.be

--------------

compile/run/deploy as a standard Play app

for local deployment, you need a Postgres database instance at `localhost:5432/cherrytree`, details see `application.conf`

test with `sbt sharedJVM/test`, `sbt sharedJVM/testOnly xxx` etc.

## coding guideline

* static assertion is by assert and doc
* dynamic check use exception


## read the code?

* `shared/.../model`: abstract and pure functions
    * `data`: data object definition, serialization
    * `cursor`: data type for cursor inside data
    * `range`: data type for range inside data, from left to right style
    * `operation`: operation definition on data
    * `conflict`, `transaction`: helper definitions
    * `ot` operational transformation for operations
* `shared/.../client`: abstract client
    * `shared/.../undoer`, etc.: part of client functionality separated out
* `jvm`: abstract server implementation
* `js` the web client

