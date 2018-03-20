package server

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, RouteResult}
import akka.util.ByteString
import java.nio.ByteBuffer

import boopickle.Pickler

import scala.concurrent.{ExecutionContext, Future}
import shared._
import shared.util._
import shared.api.Api
import shared.data0._
import shared.server.CherryTreeServer

class ApiRouter(val service: Api) extends autowire.Server[ByteBuffer, Pickler, Pickler] {

  implicit val debug = true

  override def read[R: Pickler](p: ByteBuffer) = boopickle.Default.Unpickle[R].fromBytes(debugged(p))
  override def write[R: Pickler](r: R) = boopickle.Default.Pickle.intoBytes(debugged(r))

  def dispatch(url: Seq[String])(implicit ec: ExecutionContext): RequestContext => Future[RouteResult] =
    entity(as[ByteString]) { entity =>
      val body = boopickle.Default.Unpickle[Map[String, ByteBuffer]].fromBytes(entity.asByteBuffer)
      val request: Future[ByteBuffer] = route[Api](service)(autowire.Core.Request(url, body))
      onSuccess(request)(buffer => complete(ByteString(buffer)))
    }
}
