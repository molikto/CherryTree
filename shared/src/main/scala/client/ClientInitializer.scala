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
  def init(api: Api, documentId: String): Future[Client] = {
    api.request(s"/document/$documentId/init", Pickle.intoBytes[InitRequest](InitRequest())).map { value =>
      val init = Unpickle[InitResponse](implicitly).fromBytes(value)(unpickleState)
      if (debug_transmit) {
        println(init)
      }
      val c = new Client(documentId, init, api)
      c.start()
      c
    }
  }
}
