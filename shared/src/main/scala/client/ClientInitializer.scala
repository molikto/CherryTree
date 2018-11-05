package client

import java.nio.ByteBuffer

import com.softwaremill.quicklens._
import model._
import api._
import model.data.Node

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


object ClientInitializer {
  def init(api: Api, documentId: String, nodeId: String): Future[Client] = {
    api.request[InitRequest, InitResponse](s"/document/$documentId/init", InitRequest()).map { value =>
      val c = new Client(documentId, value, api)
      c.start()
      c
    }
  }
}
