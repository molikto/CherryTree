package web.ui

import java.nio.ByteBuffer

import client.{Client, ClientInitializer, LocalStorage}
import org.scalajs.dom
import command.Key
import model.data.Content
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.raw._
import org.scalajs.dom.{html, window}
import org.scalajs.dom.{document, html, window}
import scalatags.JsDom.all._
import view.EditorInterface
import web.view._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.{Failure, Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}


object ClientInitializerView {


  class WebSocketHolder {
    var webSocket: WebSocket = null
    var stopped: Boolean = false
    var observable: PublishSubject[String] = null
    var backoff = 1000
  }

  private var globalInitialized = false
  def initializeGlobal(): Unit = {
    if (!globalInitialized) {
      model.isMac = dom.window.navigator.userAgent.toLowerCase().contains("mac")
      globalInitialized = true

      client.localStorage = new LocalStorage {
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
      model.parseFromCommonMarkMarkdown = web.util.parseFromCommonMarkMarkdown
      model.parseFromHtml = web.util.parseFromHtml
      model.apiRequest = (url: String, body: ByteBuffer) => {
        dom.ext.Ajax.post(
          url = url,
          data = body,
          responseType = "arraybuffer",
          headers = Map("Content-Type" -> "application/octet-stream")
        ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
      }
      model.setupWebSocket = (path: String) => {
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
        (holder.webSocket, holder.observable)
      }

      model.stopWebSocket = (any: Any) => {
        val holder = any.asInstanceOf[WebSocketHolder]
        holder.stopped = true
        holder.webSocket.close()
      }
    }
  }
}
/**
  * when created, starts connection with server
  * @param where an id of a html element, it is assumed that you don't modify it after handling to us
  */
@JSExportTopLevel("ClientInitializerView")
class ClientInitializerView(where: String, documentId: String, global: Boolean) {
  ClientInitializerView.initializeGlobal()

  private val rootView = el[dom.html.Element](where)

  var client: Option[Client] = None

  private def goConnecting(): Unit = {
    onlyChild(rootView, {
      import scalatags.JsDom.all._
      p("connecting")
    }.render)
    val ec = scala.concurrent.ExecutionContext.Implicits.global
    ClientInitializer.init(documentId).onComplete {
      case Success(c) =>
        goClient(c)
      case Failure(exception) =>
        goFailure(exception)
    }(ec)
  }

  goConnecting()


  private def goFailure(exception: Throwable): Unit = {
    onlyChild(rootView, {
      import scalatags.JsDom.all._
      exception.printStackTrace()
      div(
        p(s"failed: ${exception.getMessage}"),
        button("retry", onclick := { () => goConnecting() })
      )
    }.render)
  }


  private def goClient(client: Client): Unit = {
    this.client = Some(client)
    removeAllChild(rootView)
    new ClientView(rootView, client, global)
  }
}
