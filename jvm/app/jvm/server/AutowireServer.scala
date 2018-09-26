package jvm.server

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, RouteResult}
import akka.util.ByteString
import java.nio.ByteBuffer

import boopickle.Pickler
import api.Api

import scala.concurrent.{ExecutionContext, Future}
import model._
import util._

class AutowireServer(val service: Api) extends autowire.Server[ByteBuffer, Pickler, Pickler] {

  implicit val debug = false

  override def read[R: Pickler](p: ByteBuffer) = boopickle.Default.Unpickle[R].fromBytes(debugged(p))
  override def write[R: Pickler](r: R) = boopickle.Default.Pickle.intoBytes(debugged(r))

  def dispatch(url: Seq[String])(implicit ec: ExecutionContext): RequestContext => Future[RouteResult] =
    entity(as[ByteString]) { entity =>
      val body = boopickle.Default.Unpickle[Map[String, ByteBuffer]].fromBytes(entity.asByteBuffer)
      val request: Future[ByteBuffer] = route[Api](service)(autowire.Core.Request(url, body))
      onSuccess(request)(buffer => complete(ByteString(buffer)))
    }
}
