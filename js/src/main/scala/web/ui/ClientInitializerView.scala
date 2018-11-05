package web.ui


import client.{Client, ClientInitializer, LocalStorage}
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement
import web.WebApi
import web.view._

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global


object ClientInitializerView {



  private var globalInitialized = false
  def initializeGlobal(): Unit = {
    if (!globalInitialized) {
      model.isMac = dom.window.navigator.userAgent.toLowerCase().contains("mac")
      globalInitialized = true

      model.parseFromCommonMarkMarkdown = web.util.parseFromCommonMarkMarkdown
      model.parseFromHtml = web.util.parseFromHtml

    }
  }
}

/**
  */
@JSExportTopLevel("ClientInitializerView")
class ClientInitializerView(rootView: HTMLElement, documentId: String, nodeId: String, global: Boolean) {
  ClientInitializerView.initializeGlobal()


  var client: Option[Client] = None

  private def goConnecting(): Unit = {
    onlyChild(rootView, {
      import scalatags.JsDom.all._
      p("connecting")
    }.render)
    val ec = scala.concurrent.ExecutionContext.Implicits.global
    ClientInitializer.init(WebApi, documentId, nodeId).onComplete {
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
