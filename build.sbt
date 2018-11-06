import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}




lazy val server = (project in file("jvm")).settings(
  sharedSettings,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  pipelineStages := Seq(digest, gzip),
  // triggers scalaJSPipeline when using compile or continuous compilation
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  libraryDependencies ++= Deps.server.value ++ Seq(ehcache, guice, filters),
  WebKeys.packagePrefix in Assets := "public/",
  routesImport += "utils.route.Binders._",
  // https://github.com/playframework/twirl/issues/105
  TwirlKeys.templateImports := Seq(),
).enablePlugins(PlayScala, WebScalaJSBundlerPlugin).dependsOn(sharedJvm)

lazy val client = (project in file("js")).settings(
  sharedSettings,
  webpackBundlingMode := BundlingMode.LibraryAndApplication(),
  npmDependencies in Compile ++= Deps.clientJs,
  libraryDependencies ++= Deps.client.value
).enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin, ScalaJSWeb).
  dependsOn(sharedJs)

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(
    sharedSettings,
    scalaVersion := Version.scala,
    libraryDependencies ++= Deps.shared.value,
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jsConfigure(_.enablePlugins(ScalaJSWeb, ScalaJSBundlerPlugin))

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

scalaVersion := Version.scala

autoCompilerPlugins := true


val sharedSettings = Seq(
  // acyclic
  scalaVersion := Version.scala,
  libraryDependencies += "com.lihaoyi" %% "acyclic" % Version.acyclic % "provided",
  resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    Resolver.jcenterRepo,
    Resolver.sonatypeRepo("releases")
  ),
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    //"-Xfatal-warnings", // Fail the compilation if there are any warnings.
    //"-Xlint", // Enable recommended additional warnings.
    //"-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-nullary-override", // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    // Play has a lot of issues with unused imports and unsued params
    // https://github.com/playframework/playframework/issues/6690
    // https://github.com/playframework/twirl/issues/105
    "-Xlint:-unused,_",
    //"-P:acyclic:force",
  ),
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % Version.acyclic)
)

// loads the server project at sbt startup
onLoad in Global ~= (_ andThen ("project server" :: _))
