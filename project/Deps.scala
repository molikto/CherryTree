object Version {
  val akka = "10.1.3"
  val autowire = "0.2.6"
  val boopickle = "1.3.0"
  val monocle = "1.4.0"
  val scala = "2.12.6"
  val scalajsDom = "0.9.6"
  val scalajsScripts = "1.1.1"
  val scalatags = "0.6.7"
  val acyclic = "0.1.7"
  val monix = "2.3.3"
  val utest = "0.6.4"
}

object Deps {
  import sbt._
  import org.scalajs.sbtplugin.ScalaJSPlugin
  import org.scalajs.sbtplugin.ScalaJSPlugin._
  import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._



  val shared = Def.setting(
    "com.lihaoyi" %%% "autowire" % Version.autowire ::
    "com.lihaoyi" %%% "scalatags" % Version.scalatags  ::
    "io.suzaku" %%% "boopickle" % Version.boopickle ::
    "com.lihaoyi" %%% "utest" % Version.utest % "test" ::
    "io.monix" %%% "monix" % Version.monix ::
    "com.softwaremill.quicklens" %%% "quicklens" % "1.4.11" ::
    "io.lemonlabs" %%% "scala-uri" % "1.1.4" ::
      Nil)

  val server = Def.setting(shared.value ++ (
    "com.typesafe.akka" %%% "akka-http" % Version.akka ::
    //"com.vladsch.flexmark" % "flexmark-all" % "0.34.6" ::
    "com.vmunier" %%% "scalajs-scripts" % Version.scalajsScripts ::
      Nil))

  val client = Def.setting(shared.value ++ (
    "org.scala-js" %%% "scalajs-dom" % Version.scalajsDom ::
      Nil))

  val clientJs = Seq(
  )
}
