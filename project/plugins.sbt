
resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

// Use Scala.js 1.x
//addSbtPlugin("com.vmunier"               % "sbt-web-scalajs"           % "1.0.8")
//addSbtPlugin("org.scala-js"              % "sbt-scalajs"               % "1.0.0-M3")

addSbtPlugin("com.vmunier"               % "sbt-web-scalajs"           % "1.0.8-0.6")
addSbtPlugin("org.scala-js"              % "sbt-scalajs"               % "0.6.23")

addSbtPlugin("com.typesafe.play"         % "sbt-plugin"                % "2.6.15")
addSbtPlugin("com.typesafe.sbt"          % "sbt-gzip"                  % "1.0.2")
addSbtPlugin("com.typesafe.sbt"          % "sbt-digest"                % "1.1.4")
addSbtPlugin("com.typesafe.sbt"          % "sbt-native-packager"       % "1.3.5")
addSbtPlugin("org.portable-scala"        % "sbt-scalajs-crossproject"  % "0.5.0")
addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.13.1")
