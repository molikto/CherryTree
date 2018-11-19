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
import org.scalajs.dom
import org.w3c.dom.DOMError
import web.ui.content.ContentView
import web.view.View

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer, Uint8Array}
import scala.util.{Failure, Success}
import web.ui
import web.ui.doc.LaTeXMacroCache


@JSExportTopLevel("DocumentListRender")
class DocumentListRender(rootView: HTMLElement) extends View {

  private val modal = ui.createModal()

  private val list = div(
  ).render
  private val ff: Form =
    form(
      display := "inline",
      action := "/documents/upload/json", method := "post", enctype := "multipart/form-data",
      span(cls := "fileinput fileinput-new secondary-text", attr("data-provides") := "fileinput",
        display := "inline",
        marginBottom := "0px",
        span(cls := "btn-file secondary-text-button",
          verticalAlign := "baseline",
          span(cls := "fileinput-new dropdown-toggle", "Create from backup"),
          input(`type` := "file", id := "upload_json", attr("cursor") := "pointer", name := "file", onchange := ((ev: UIEvent) => {
            ff.submit()
          }))
        )
      )
    ).render

  private val createDoc: Form =
    form(
      marginRight := "12px",
      display := "inline",
      action := "/documents/create", method := "post", enctype := "multipart/form-data",
      span(
        cls := "secondary-text-button dropdown-toggle",
        Messages("create.new.document"),
        onclick := ((ev: MouseEvent) => {
          createDoc.submit()
        }))
    ).render

  dom = div(
    list,
    p(
      marginTop := "24px",
      createDoc,
      ff,
    ),
    modal.root
  ).render

  attachToNode(rootView)


  WebApi.get[Seq[ListResult]]("/documents/json").onComplete {
    case Success(res) =>
      res.foreach(item => {
        val contentView = ContentView.create(item.title, None, LaTeXMacroCache.empty)
        list.appendChild(div(
          a(
            cls := "ct-document-style document-list-title",
            href := s"/document/${item.id}", contentView),
          p(
            cls := "secondary-text",
            Messages("last.updated", formatDate(item.updatedTime))
          ),
            p(
            cls := "secondary-text",
            Messages("created.at", formatDate(item.createdTime))),
          div(cls := "dropdown",
            a(cls := "secondary-text-button dropdown-toggle",
              href := "",
              attr("data-toggle") := "dropdown",
              Messages("options")
            ),
            ul(cls := "dropdown-menu", role := "menu",
              if (item.permissionLevel >= PermissionLevel.Admin) {
                a(cls := "dropdown-item", Messages("delete"), href := "", onclick := ((ev: MouseEvent) => {
                  modal.body.textContent = Messages("delete.confirm.message")
                  modal.positive.textContent = Messages("confirm")
                  modal.negative.textContent = Messages("cancel")
                  modal.positive.onclick = (ev: MouseEvent) => {
                    modal.positiveForm.action = s"/document/${item.id}/delete"
                    modal.positiveForm.submit()
                  }
                  jQ(modal.root).modal()
                  ev.preventDefault()
                })) : Frag
              } else {
                SeqFrag(Seq.empty[Frag])
              },
              a(cls := "dropdown-item", Messages("backup"), attr("download") := "", href := s"/document/json/${item.id}"),
              a(cls := "dropdown-item", "Share", href := "", onclick := ((ev: MouseEvent) => {
                val person = org.scalajs.dom.window.prompt("Enter collabrator email")
                if (person == null || person == "") {
                  //WebApi.request(s"/document/${item.id}/share")
                } else {

                }
              }))
            )
          )
        ).render)
        list.appendChild(hr(cls := "small-hr").render)
      })
    case Failure(exception) =>
      exception.printStackTrace()
  }
}
