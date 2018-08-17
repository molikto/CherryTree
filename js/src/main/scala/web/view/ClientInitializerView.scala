package web.view

import api.{Api, Authentication}
import client.{Client, ClientInitializer}
import model.LocalStorage
import org.scalajs.dom
import web.net.JsAutowireClient
import command.Key
import model.data.Content
import org.scalajs.dom.raw._
import org.scalajs.dom.{html, window}
import org.scalajs.dom.{document, html, window}
import scalatags.JsDom.all._
import view.EditorInterface
import web.view._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.{Failure, Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global


object ClientInitializerView {

  model.oldDocVersion = false

  private var globalInitialized = false
  def initializeGlobal(): Unit = {
    if (!globalInitialized) {
      model.isMac = dom.window.navigator.userAgent.toLowerCase().contains("mac")
      globalInitialized = true

      model.localStorage = new LocalStorage {
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

    }
  }
}
/**
  * when created, starts connection with server
  * @param where an id of a html element, it is assumed that you don't modify it after handling to us
  */
@JSExportTopLevel("ClientInitializerView")
class ClientInitializerView(where: String) {
  ClientInitializerView.initializeGlobal()

  private val rootView = el[dom.html.Element](where)
  private val token = Authentication.Token(System.currentTimeMillis().toString)

  private val server = new JsAutowireClient()[Api]

  var client: Option[Client] = None

  private def goConnecting(): Unit = {
    onlyChild(rootView, {
      import scalatags.JsDom.all._
      p("connecting")
    }.render)
    ClientInitializer.init(server, token).onComplete {
      case Success(c) =>
        goClient(c)
      case Failure(exception) =>
        goFailure(exception)
    }
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
    client.start()
    new ClientView(rootView, client)
  }
}
