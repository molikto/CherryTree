object Version {
  val autowire = "0.2.6"
  val boopickle = "1.3.0"
  val monocle = "1.4.0"
  val scala = "2.12.6"
  val scalajsDom = "0.9.6"
  val scalajsScripts = "1.1.1"
  val silhouette = "5.0.5"
  val scalatags = "0.6.7"
  val acyclic = "0.1.7"
  val monix = "2.3.3"
  val utest = "0.6.4"
  val playMailer = "6.0.1"
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

  val server = Def.setting(shared.value ++ Seq(
    //"com.vladsch.flexmark" % "flexmark-all" % "0.34.6" ::
    "com.vmunier" %% "scalajs-scripts" % Version.scalajsScripts,
    "net.codingwell" %% "scala-guice" % "4.1.0",
    "com.typesafe.play" %% "play-slick" % "3.0.0",
    "com.typesafe.play" %% "play-slick-evolutions" % "3.0.0",
    "com.google.inject.extensions" % "guice-grapher" % "4.1.0",
    "com.h2database" % "h2" % "1.4.197",
    "com.mohiva" %% "play-silhouette" % Version.silhouette,
    "com.mohiva" %% "play-silhouette-password-bcrypt" % Version.silhouette,
    "com.mohiva" %% "play-silhouette-persistence" % Version.silhouette,
    "com.mohiva" %% "play-silhouette-crypto-jca" % Version.silhouette,
    "com.adrianhurt" %% "play-bootstrap" % "1.4-P26-B4-SNAPSHOT",
    "com.iheart" %% "ficus" % "1.4.3",
    "com.typesafe.play" %% "play-mailer" % Version.playMailer,
    "com.typesafe.play" %% "play-mailer-guice" % Version.playMailer,
    "com.enragedginger" %% "akka-quartz-scheduler" % "1.6.1-akka-2.5.x",
  ))

  val client = Def.setting(shared.value ++ (
    "org.scala-js" %%% "scalajs-dom" % Version.scalajsDom ::
      Nil))

  val clientJs = Seq(
    "commonmark" -> "0.28.1"
  )
}
