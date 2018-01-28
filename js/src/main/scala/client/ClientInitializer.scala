package client

import shared.data.Authentication

import scala.util.{Failure, Success}
import autowire._
import boopickle.Default._
import client.net.AutowireServer
import scala.concurrent.ExecutionContext.Implicits.global


import scala.concurrent.Future


object ClientInitializer {
  def init(token: Authentication.Token): Future[Client] = {
    api.init(token).call().map { it =>new Client(it) }
  }
}
