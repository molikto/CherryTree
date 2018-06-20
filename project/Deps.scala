object Version {
  val akka = "10.0.11"
  val autowire = "0.2.6"
  val boopickle = "1.3.0"
  val monocle = "1.4.0"
  val scala = "2.12.6"
  val scalajsDom = "0.9.3"
  val scalajsScripts = "1.1.1"
  val scalatags = "0.6.7"
  val acyclic = "0.1.7"
  val monix = "2.3.3"
  val utest = "0.6.4"
  val scalaJsReact = "1.2.0"
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
      Nil)

  val server = Def.setting(shared.value ++ (
    "com.typesafe.akka" %%% "akka-http" % Version.akka ::
    "com.vmunier" %%% "scalajs-scripts" % Version.scalajsScripts ::
      Nil))

  val client = Def.setting(shared.value ++ (
    "org.scala-js" %%% "scalajs-dom" % Version.scalajsDom ::
    // "com.github.lukajcb" %%% "rxscala-js" % "0.15.0" ::
    "com.github.japgolly.scalajs-react" %%% "core" % Version.scalaJsReact ::
    "com.github.japgolly.scalajs-react" %%% "extra" % Version.scalaJsReact ::
      Nil))

  val clientJs = Seq(
    // "rxjs" -> "5.5.6",
    "react" -> "15.6.1",
    "react-dom" -> "15.6.1"
  )
}
