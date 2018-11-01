package web.ui

import java.io.Closeable
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
import web.WebApi
import web.view._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.{Failure, Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}


object ClientInitializerView {



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

    }
  }
}

/**
  */
@JSExportTopLevel("ClientInitializerView")
class ClientInitializerView(where: String, global: Boolean) {
  ClientInitializerView.initializeGlobal()

  private val rootView = el[dom.html.Element](where)

  var client: Option[Client] = None

  private def goConnecting(): Unit = {
    onlyChild(rootView, {
      import scalatags.JsDom.all._
      p("connecting")
    }.render)
    val documentId = rootView.getAttribute("data-document-id")
    val ec = scala.concurrent.ExecutionContext.Implicits.global
    ClientInitializer.init(WebApi, documentId).onComplete {
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
