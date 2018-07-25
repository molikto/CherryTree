package jvm.server

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.file.{Files, OpenOption}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import model.data.Node


class WebServer() {



  val api = new server.Server() {

    override def debugSave(a: String, bs: Array[Byte]): Unit = {
      val file = new File(a)
      Files.write(file.toPath, bs)
    }

    override def debugLoad(a: String): Array[Byte] = {
      val file = new File(a)
      if (!file.exists()) {
        Array()
      } else {
        Files.readAllBytes(file.toPath)
      }
    }
  }
  val router = new HttpRouter(api)
  def run() {
    implicit val system: ActorSystem = ActorSystem("server-system")
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val config = ConfigFactory.load()
    val interface = config.getString("http.interface")
    val port = config.getInt("http.port")

    import system.dispatcher
    Http().bindAndHandle(router.apply(), interface, port)
    println("Server current dir: " + new File(".").getAbsolutePath)
    println(s"Server online at http://$interface:$port")
  }
}
