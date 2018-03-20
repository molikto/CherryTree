package shared.client

import java.nio.ByteBuffer

import boopickle.Pickler
import shared.api.Api
import shared.data0._
import shared.server.CherryTreeServer

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



class TestApiAdapter(service: Api) extends autowire.Client[ByteBuffer, Pickler, Pickler] {

  object server extends autowire.Server[ByteBuffer, Pickler, Pickler] {
    override def read[R: Pickler](p: ByteBuffer) = boopickle.Default.Unpickle[R].fromBytes(p)
    override def write[R: Pickler](r: R) = boopickle.Default.Pickle.intoBytes(r)
    def route(url: Seq[String], byteBuffer: ByteBuffer): Future[ByteBuffer] = {
      val body = boopickle.Default.Unpickle[Map[String, ByteBuffer]].fromBytes(byteBuffer)
      route[Api](service)(autowire.Core.Request(url, body))
    }
  }
  override def doCall(req: Request): Future[ByteBuffer] = {
    val data = boopickle.Default.Pickle.intoBytes(req.args)
    server.route(req.path, data)
  }
  override def read[Result: Pickler](p: ByteBuffer) = boopickle.Default.Unpickle[Result].fromBytes(p)
  override def write[Result: Pickler](r: Result) = boopickle.Default.Pickle.intoBytes(r)
}
