package web

import java.io.Closeable
import java.nio.ByteBuffer

import boopickle.Pickler
import client.LocalStorage
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.ext.Ajax.InputData
import org.scalajs.dom.raw.{Event, MessageEvent, WebSocket}
import org.scalajs.dom.{XMLHttpRequest, window}
import scalatags.JsDom.all.s

import scala.concurrent.Future
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.concurrent.ExecutionContext.Implicits.global


class WebSocketHolder extends Closeable {
  var webSocket: WebSocket = null
  var stopped: Boolean = false
  var observable: PublishSubject[Either[String, Throwable]] = null
  var backoff = 1000

  override def close(): Unit = {
    stopped = true
    webSocket.close()
  }
}

object WebApi extends client.Api {

  override val localStorage: LocalStorage = new LocalStorage {
    override def set(key: String, str: String): Unit = {
      window.localStorage.setItem(key, str)
    }

    override def remove(key: String): Unit = {
      window.localStorage.removeItem(key)
    }

    override def get(key: String): Option[String] = {
      Option(window.localStorage.getItem(key))
    }
  }

  def delete(url: String): Future[XMLHttpRequest] = {
    dom.ext.Ajax.delete(
      url = url,
      responseType = "arraybuffer",
      headers = Map("Csrf-Token" -> "nocheck")
    )
  }

  override def requestBytes(url: String, body: ByteBuffer, method: String = "POST"): Future[ByteBuffer] = {
    dom.ext.Ajax.apply(method = method,
      url = url,
      data = body,
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream", "Csrf-Token" -> "nocheck"),
      timeout = 0,
      withCredentials = false
    ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
  }

  override def setupWebSocket(path: String): (Closeable, Observable[Either[String, Throwable]]) = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    val url = s"$wsProtocol://${dom.document.location.host}$path"
    val holder = new WebSocketHolder()
    holder.observable = PublishSubject[Either[String, Throwable]]()
    def create(): Unit = {
      if (holder.stopped) return
      holder.backoff = 600000 min (holder.backoff * 2)
      val ws = new WebSocket(url)
      if (model.debug_webSocket) println(s"trying to create WebSocket with backoff ${holder.backoff}")
      ws.onopen = { event: Event =>
        holder.backoff = 1000
      }
      ws.onclose = { event: Event =>
        if (holder.stopped) {
          holder.observable.onComplete()
        } else {
          window.setTimeout(() => create(), holder.backoff)
        }
        if (model.debug_webSocket) println(s"WebSocket closed")
      }
      ws.onerror = { event: Event =>
        holder.observable.onNext(Right(new Exception("websocket error")))
        if (model.debug_webSocket) println(s"WebSocket error")
        //window.setTimeout(() => create(), holder.backoff)
      }
      ws.onmessage = { event: MessageEvent =>
        holder.observable.onNext(Left(event.data.toString))
      }
      holder.webSocket = ws
    }
    create()
    (holder, holder.observable)
  }
}
