package client

import autowire._
import com.softwaremill.quicklens._


import model._

import api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


object ClientInitializer {
  def init(server: Client.Proxy, token: Authentication.Token): Future[Client] = {
    transform(server.init(token).call()).map { it => new Client(server, it, token) }
  }
}
