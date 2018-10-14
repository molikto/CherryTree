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
  def init(documentId: String): Future[Client] = {
    model.apiRequest(s"/document/$documentId/init", ByteBuffer.wrap(Array.empty[Byte])).map { value =>
      val init = Unpickle[ClientInit](implicitly)(value)
      if (debug_transmit) {
        println(init)
      }
      new Client(documentId, init)
    }
  }
}
