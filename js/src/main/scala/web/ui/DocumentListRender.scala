package web.ui

import java.nio.ByteBuffer
import java.util.Date

import api.ListResult
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLElement, ProgressEvent}

import scala.scalajs.js.annotation.JSExportTopLevel
import scalatags.JsDom.all._
import web.WebApi
import model._
import api._
import org.scalajs.dom.html.{Form, Input}
import org.scalajs.dom._
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

  private val list = div(
  ).render
  private val ff: Form =
    form(
      marginTop := "120px",
      action := "/documents/upload/boopickle", method := "post", enctype := "multipart/form-data",
      div(`class` := "custom-file",
        input(`type` := "file", `class` := "custom-file-input", id := "boopickle", name := "file", onchange := ((ev: UIEvent) => {
          ff.submit()
        })),
        tag("label")(`class` := "custom-file-label", `for` := "boopickle", Messages("new.from.backup"))
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
        list.appendChild(div(
          a(
            `class` := "ct-document-style document-list-title",
            href := s"/document/${item.id}", contentView),
          p(
            `class` := "secondary-text",
            Messages("last.updated", formatDate(item.updatedTime))
          ),
          div(
            display := "flex",
            p(
              `class` := "secondary-text",
              flex := "1 1",
              Messages("created.at", formatDate(item.createdTime))),
            div(`class` := "dropdown",
              a(`class` := "secondary-text-button dropdown-toggle",
                href := "",
                attr("data-toggle") := "dropdown",
                Messages("options")
              ),
              ul(`class` := "dropdown-menu", role := "menu",
                a(`class` := "dropdown-item", Messages("backup"), attr("download") := "", href := s"/document/json/${item.id}")
              )
            )
          )
        ).render)
        list.appendChild(hr(`class` := "small-hr").render)
      })
    case Failure(exception) =>
      exception.printStackTrace()
  }
}
