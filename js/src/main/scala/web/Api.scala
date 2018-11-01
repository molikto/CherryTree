package web

import java.io.Closeable
import java.nio.ByteBuffer

import client.LocalStorage
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom
import org.scalajs.dom.raw.{Event, MessageEvent, WebSocket}
import org.scalajs.dom.window
import scalatags.JsDom.all.s

import scala.concurrent.Future
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.concurrent.ExecutionContext.Implicits.global


class WebSocketHolder extends Closeable {
  var webSocket: WebSocket = null
  var stopped: Boolean = false
  var observable: PublishSubject[String] = null
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
  override def request(url: String, body: ByteBuffer): Future[ByteBuffer] = {
    dom.ext.Ajax.post(
      url = url,
      data = body,
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
  }

  override def setupWebSocket(path: String): (Closeable, Observable[String]) = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    val url = s"$wsProtocol://${dom.document.location.host}$path"
    val holder = new WebSocketHolder()
    holder.observable = PublishSubject[String]()
    def create(): Unit = {
      if (holder.stopped) return
      holder.backoff = 600000 min (holder.backoff * 2)
      val ws = new WebSocket(url)
      ws.onopen = { event: Event =>
        holder.backoff = 1000
      }
      ws.onclose = { event: Event =>
        if (holder.stopped) {
          holder.observable.onComplete()
        } else {
          window.setTimeout(() => create(), holder.backoff)
        }
      }
      ws.onerror = { event: Event =>
        window.setTimeout(() => create(), holder.backoff)
      }
      ws.onmessage = { event: MessageEvent =>
        holder.observable.onNext(event.data.toString)
      }
      holder.webSocket = ws
    }
    create()
    (holder, holder.observable)
  }
}
