# Cherry Tree


compile/run with `sbt ~reStart` (for now)


test with `sbt sharedJVM/test`, `sbt sharedJVM/testOnly xxx` etc.

release build with `sbt assembly` for the jar. more info here: https://github.com/vmunier/akka-http-scalajs.g8

## coding guideline

* static assertion is by assert and doc
* dynamic check use exception

## debug deploy

```
curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
curl --silent --location https://rpm.nodesource.com/setup_10.x | sudo bash -
sudo yum -y install java git sbt nodejs
//  ssh-keygen -t rsa -b 4096 -C "user@domain.com"
git clone git@github.com:molikto/CherryTree.git
mv CherryTree codename-cherry-tree
cd codename-cherry-tree
./remote_server.sh
```


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

