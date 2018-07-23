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


  val file = new File("saved")

  val api = new server.Server() {
    import model._
    override def save(a: Node): Unit = {
      val bs = Pickle.intoBytes(a)(implicitly, Node.pickler)
      Files.write(file.toPath, bs.array())
    }

    override def load(): Node = {
      if (!file.exists()) {
        Node.empty
      } else {
        val rs = Files.readAllBytes(file.toPath)
        val b = Unpickle[Node](Node.pickler).fromBytes(ByteBuffer.wrap(rs))
        b
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

    Http().bindAndHandle(router.route, interface, port)
    println("Server current dir: " + new File(".").getAbsolutePath)
    println(s"Server online at http://$interface:$port")
  }
}
