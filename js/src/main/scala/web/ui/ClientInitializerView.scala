package web.ui


import java.util.UUID

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
      globalInitialized = true
      platform.isMac = dom.window.navigator.userAgent.toLowerCase().contains("mac")
      platform.parseFromCommonMarkMarkdown = web.util.parseFromCommonMarkMarkdown
      platform.parseFromHtml = web.util.parseFromHtml
      platform.formatDate = web.util.formatDate
      CodeMirror.modeURL = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.39.2/mode/%N/%N.min.js"
    }
  }
}

/**
  */
@JSExportTopLevel("ClientInitializerView")
class ClientInitializerView(rootView: HTMLElement, documentId: UUID, nodeId: Option[UUID], global: Boolean) {
  ClientInitializerView.initializeGlobal()
  import scalatags.JsDom.all._


  val bg = div(
    position := "absolute",
    width := "100%",
    height := "100%",
    cls := "ct-document-view-background",
    zIndex := "-2"
  ).render
  rootView.appendChild(bg)


  private def removeExceptBg(): Unit = {
    var i = rootView.childNodes.length - 1
    while (i >= 0) {
      val v = rootView.childNodes(i)
      if (v != bg) {
        rootView.removeChild(v)
      }
      i -= 1
    }
  }

  private def goConnecting(): Unit = {
    removeExceptBg()
    rootView.appendChild(div(
        width := "100%", height := "100",
        p("connecting")
      ).render)
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
    removeExceptBg()
    rootView.appendChild({
      import scalatags.JsDom.all._
      exception.printStackTrace()
      div(
        p(s"failed: ${exception.getMessage}"),
        button("retry", onclick := { () => goConnecting() })
      )
    }.render)
  }

  private var clientView: ClientView = null

  private def goClient(client: Client): Unit = {
    removeExceptBg()
    clientView = new ClientView(rootView, client, () => {
      try {
        clientView.destroy()
      } catch {
        case e: Exception => {
          e.printStackTrace() // TODO error reporting
          throw e
        }
      }
      goClient(client)
    }, global)
  }
}
