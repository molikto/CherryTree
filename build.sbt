lazy val server = (project in file("jvm")).settings(
  scalaVersion := Version.scala,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  // triggers scalaJSPipeline when using compile or continuous compilation
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  libraryDependencies ++= Deps.server.value,
  WebKeys.packagePrefix in Assets := "public/",
  managedClasspath in Runtime += (packageBin in Assets).value
).enablePlugins(SbtWeb, JavaAppPackaging, WebScalaJSBundlerPlugin).
  dependsOn(sharedJvm)

lazy val client = (project in file("js")).settings(
  scalaVersion := Version.scala,
  scalaJSUseMainModuleInitializer := true,
  npmDependencies in Compile ++= Deps.clientJs,
  libraryDependencies ++= Deps.client.value
).enablePlugins(ScalaJSPlugin, ScalaJSWeb, ScalaJSBundlerPlugin).
  dependsOn(sharedJs)

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(
    scalaVersion := Version.scala,
    libraryDependencies ++= Deps.shared.value,
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jsConfigure(_.enablePlugins(ScalaJSWeb, ScalaJSBundlerPlugin))

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

scalaVersion := Version.scala

// loads the server project at sbt startup
onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value
