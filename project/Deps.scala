object Version {
  val akka = "10.0.10"
  val autowire = "0.2.6"
  val boopickle = "1.2.6"
  val monocle = "1.4.0"
  val scala = "2.12.4"
  val scalajsDom = "0.9.3"
  val scalajsScripts = "1.1.1"
  val scalatags = "0.6.7"
}

object Deps {
  import sbt._
  import org.scalajs.sbtplugin.ScalaJSPlugin
  import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

  val shared = Def.setting(
    "com.lihaoyi" %%% "autowire" % Version.autowire ::
    "com.lihaoyi" %%% "scalatags" % Version.scalatags  ::
    "io.suzaku" %%% "boopickle" % Version.boopickle ::
    "com.lihaoyi" %%% "utest" % "0.6.0" % "test" ::
    "io.monix" %%% "monix" % "2.3.3" ::
      Nil)

  val server = Def.setting(shared.value ++ (
    "com.typesafe.akka" %%% "akka-http" % Version.akka ::
    "com.vmunier" %%% "scalajs-scripts" % Version.scalajsScripts ::
      Nil))

  val client = Def.setting(shared.value ++ (
    "org.scala-js" %%% "scalajs-dom" % Version.scalajsDom ::
    // "com.github.lukajcb" %%% "rxscala-js" % "0.15.0" ::
    "com.github.japgolly.scalajs-react" %%% "core" % "1.1.1" ::
    "com.github.japgolly.scalajs-react" %%% "extra" % "1.1.1" ::
      Nil))

  val clientJs = Seq(
    // "rxjs" -> "5.5.6",
    "react" -> "15.6.1",
    "react-dom" -> "15.6.1"
  )

}
