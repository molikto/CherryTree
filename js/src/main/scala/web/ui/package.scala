package web

import java.nio.ByteBuffer
import java.util.Date

import api.ListResult
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLElement, ProgressEvent}

import scala.scalajs.js.annotation.JSExportTopLevel
import scalatags.JsDom.all._
import web.{Implicits, WebApi}
import model._
import api._
import command.Key
import web._
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


class ModalUi(
  val root: HTMLElement,
  //val title: HTMLElement,
  val body: HTMLElement,
  val positiveForm: Form,
  val positive: HTMLElement,
  val negative: HTMLElement
)

package object ui extends Implicits {

  val EmptyStr = "∅"
  val EvilChar =  "\u200B"


  def createModal(): ModalUi = {
//    val title =
//      h5(cls := "modal-title", "Modal title").render
    val body =
      p("Modal body text goes here.").render

    val positive =
      button(
        `type` := "button",
        cls := "btn btn-primary",
        "Save changes").render
    val positiveForm =     form(
      enctype := "multipart/form-data",
      method := "post",
      positive,
    ).render

    val nagetive =
      button(
        `type` := "button",
        cls := "btn btn-secondary",
        attr("data-dismiss") := "modal",
        "Close").render
    val root = div(
      cls := "modal",
      tabindex := "-1",
      role := "dialog",
      div(
        cls := "modal-dialog",
        role := "document",
        div(
          cls := "modal-content",
//          div(
//            cls := "modal-header",
//            title,
//            button(
//              `type` := "button",
//              cls := "close",
//              attr("data-dismiss") := "modal",
//              attr("aria-label") := "Close",
//              span(
//                attr("aria-hidden") := "true",
//                "×")
//            )
//          ),
          div(
            cls := "modal-body",
            body
          ),
          div(
            cls := "modal-footer",
            positiveForm,
            nagetive
          )
        )
      )
    ).render
    new ModalUi(root, body, positiveForm, positive, nagetive)
  }

  // https://developer.mozilla.org/zh-CN/docs/Web/API/KeyboardEvent/key/Key_Values
  val KeyMap: Map[String, Key.V] = {
    import command.Key._
    Map(
      "Home" -> Home,
      "End" -> End,
      "ArrowLeft" -> Left,
      "ArrowRight" -> Right,
      "ArrowUp" -> Up,
      "ArrowDown" -> Down,
      "Enter" -> Enter,
      "PageDown" -> PageDown,
      "PageUp" -> PageUp,
      "Backspace" -> Backspace,
      "Tab" -> Tab,
      "Escape" -> Escape,
      "Shift" -> Shift,
      "Meta" -> Meta,
      "Control" -> Ctrl,
      "Delete" -> Delete,
      "Alt" -> Alt
    )
  }

  val jQ: js.Dynamic = js.Dynamic.global.jQuery

  val CodeMirror: js.Dynamic = js.Dynamic.global.CodeMirror

  val KaTeX = window.asInstanceOf[js.Dynamic].katex


  def Messages(key: String, args: js.Any*): String = {
    val arr: Seq[js.Any] = Seq(key : js.Any) ++ args
    window.asInstanceOf[js.Dynamic].applyDynamic("Messages")(arr: _*).asInstanceOf[String]
  }

  def svgSourceToBackgroundStr(svg: String): String = {
    val encoded = window.btoa(svg)
    "url(data:image/svg+xml;base64," + encoded + ")"
  }
}
