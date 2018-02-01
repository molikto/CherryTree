package server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import shared.server.ApiImpl


class HttpServer() {


  val api = new ApiImpl()
  val service = new MainRouter(api)
  def run() {
    implicit val system = ActorSystem("server-system")
    implicit val materializer = ActorMaterializer()

    val config = ConfigFactory.load()
    val interface = config.getString("http.interface")
    val port = config.getInt("http.port")

    import system.dispatcher
    Http().bindAndHandle(service.apply(), interface, port)
    println(s"Server online at http://$interface:$port")
  }
}
