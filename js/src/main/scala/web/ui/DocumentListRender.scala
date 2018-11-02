package web.ui

import api.ListResult
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.annotation.JSExportTopLevel
import scalatags.JsDom.all._
import web.WebApi
import model._
import api._
import org.scalajs.dom.html.Input
import org.scalajs.dom.{Blob, FileReader, UIEvent, raw}
import org.w3c.dom.DOMError
import org.w3c.dom.events.{Event, MouseEvent}
import web.ui.content.ContentView
import web.view.View

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scala.util.{Failure, Success}

@JSExportTopLevel("DocumentListRender")
class DocumentListRender(rootView: HTMLElement) extends View {

  dom = div(
  ).render

  attachToNode(rootView)



  WebApi.request[Seq[ListResult]]("/documents").onComplete {
    case Success(res) =>
      res.foreach(item => {
        val contentView = ContentView.create(item.title, None)
        val conta = a(href := s"/document/${item.id}").render
        contentView.attachToNode(conta)
        dom.appendChild(conta)
        dom.appendChild(hr().render)
      })
    case Failure(exception) =>
      exception.printStackTrace()
  }
}
