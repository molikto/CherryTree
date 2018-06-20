// fast development turnaround when using sbt ~re-start
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6" exclude("org.apache.maven", "maven-plugin-api"))

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.5")

addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.8-0.6")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.23")

addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.13.0")

