package web.ui

import api.ListResult
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.annotation.JSExportTopLevel
import scalatags.JsDom.all._
import web.WebApi
import model._
import api._
import web.ui.content.ContentView

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

@JSExportTopLevel("DocumentListRender")
class DocumentListRender(rootView: HTMLElement) {


  WebApi.request[Seq[ListResult]]("/documents").onComplete {
    case Success(res) =>
      res.foreach(item => {
        val contentView = ContentView.create(item.title, None)
        val a = div().render
        contentView.attachToNode(a)
        rootView.appendChild(hr().render)
        rootView.appendChild(a)
      })
    case Failure(exception) =>
      exception.printStackTrace()
  }
}
