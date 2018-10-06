package client

import com.softwaremill.quicklens._
import model._
import api._
import model.data.Node

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


object ClientInitializer {
  def init(documentId: String): Future[Client] = {
    Future.successful(new Client(documentId, ClientInit(Node.create(), 0, ServerStatus(1, false, false))))
    //transform(server.init(token).call()).map { it => new Client("debugDoc", server, it, token) }
  }
}
