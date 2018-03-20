package client.net

import java.nio.ByteBuffer

import org.scalajs.dom

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray._
import shared.data0._
import shared.util._


class JsAutowireAdapter extends autowire.Client[ByteBuffer, Pickler, Pickler] {
  override def doCall(req: Request): Future[ByteBuffer] = {
    dom.ext.Ajax.post(
      url = "/api/" + req.path.mkString("/"),
      data = boopickle.Default.Pickle.intoBytes(req.args),
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
  }

  override def read[Result: Pickler](p: ByteBuffer): Result =  boopickle.Default.Unpickle[Result].fromBytes(debugged(p))
  override def write[Result: Pickler](r: Result): ByteBuffer = boopickle.Default.Pickle.intoBytes(r)
}
