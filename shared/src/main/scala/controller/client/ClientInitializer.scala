package controller.client

import autowire._
import com.softwaremill.quicklens._


import model._
import controller.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


object ClientInitializer {
  def init(server: Server, token: Authentication.Token): Future[ClientModel] = {
    transform(server.init(token).call()).map { it => new ClientModel(server, it, token) }
  }
}