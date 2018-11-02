package web.ui

import java.nio.ByteBuffer

import api.ListResult
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLElement, ProgressEvent}

import scala.scalajs.js.annotation.JSExportTopLevel
import scalatags.JsDom.all._
import web.WebApi
import model._
import api._
import org.scalajs.dom.html.{Form, Input}
import org.scalajs.dom.{Blob, FileReader, UIEvent, raw}
import org.w3c.dom.DOMError
import web.ui.content.ContentView
import web.view.View

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer, Uint8Array}
import scala.util.{Failure, Success}

@JSExportTopLevel("DocumentListRender")
class DocumentListRender(rootView: HTMLElement) extends View {

  val list = div(
     `class` := "ct-document-style"
  ).render
  val ff: Form =
    form(action := "/documents/upload/boopickle", method := "post", enctype := "multipart/form-data",
      div(`class` := "custom-file",
        input(`type` := "file", `class` := "custom-file-input", id := "boopickle", name := "file", onchange := ((ev: UIEvent) => {
          ff.submit()
        })),
        tag("label")(`class` := "custom-file-label", `for` := "boopickle", "Import Boopickle")
      )
    ).render
  dom = div(
    list,
    ff
  ).render

  attachToNode(rootView)



  WebApi.request[Seq[ListResult]]("/documents").onComplete {
    case Success(res) =>
      res.foreach(item => {
        val contentView = ContentView.create(item.title, None)
        val conta = a(href := s"/document/${item.id}").render
        contentView.attachToNode(conta)
        list.appendChild(conta)
        list.appendChild(hr().render)
      })
    case Failure(exception) =>
      exception.printStackTrace()
  }
}
