package shared.client

import autowire._
import com.softwaremill.quicklens._
import shared.data._
import boopickle.Default._
import shared.api.Authentication

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


object ClientInitializer {
  def init(server: Server, token: Authentication.Token): Future[ClientModel] = {
    server.init(token).call().map { it => new ClientModel(server, it, token) }
  }
}
