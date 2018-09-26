import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}




lazy val server = (project in file("jvm")).settings(
  sharedSettings,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  pipelineStages := Seq(digest, gzip),
  // triggers scalaJSPipeline when using compile or continuous compilation
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  libraryDependencies ++= Deps.server.value ++ Seq(guice),
  WebKeys.packagePrefix in Assets := "public/",
  managedClasspath in Runtime += (packageBin in Assets).value
).enablePlugins(SbtWeb, JavaAppPackaging, PlayScala, WebScalaJSBundlerPlugin).
  dependsOn(sharedJvm)

lazy val client = (project in file("js")).settings(
  sharedSettings,
  webpackBundlingMode := BundlingMode.LibraryAndApplication(),
  npmDependencies in Compile ++= Deps.clientJs,
  libraryDependencies ++= Deps.client.value
).enablePlugins(ScalaJSPlugin, ScalaJSWeb, ScalaJSBundlerPlugin).
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
  resolvers += Resolver.sonatypeRepo("releases"),
//  scalacOptions += "-P:acyclic:force",
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % Version.acyclic)
)

// loads the server project at sbt startup
onLoad in Global ~= (_ andThen ("project server" :: _))
