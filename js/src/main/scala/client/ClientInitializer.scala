package client

import shared.data.Authentication

import scala.util.{Failure, Success}
import autowire._
import boopickle.Default._
import scala.concurrent.ExecutionContext.Implicits.global


import scala.concurrent.Future


object ClientInitializer {
  def init(server: Server, token: Authentication.Token): Future[Client] = {
    server.init(token).call().map { it =>new Client(server, it) }
  }
}
