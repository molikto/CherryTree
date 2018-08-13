package web.view

import web._
import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import web.view.doc.DocumentView
import web.view._

import scala.scalajs.js

class SourceEditDialog(
  protected val layer: OverlayLayer,
  override protected val covering: () => HTMLElement
) extends CoveringOverlay {

  private val ta = textarea(
    flex := "1 1 auto",
    width := "100%",
    height := "100%",
    name := "code"
  ).render

  dom= form(
    display := "flex",
    flexDirection := "column-reverse",
    padding := "8px",
    `class` := "ct-card",
    ta).render

  val codeMirror = CodeMirror.fromTextArea(ta, jsObject(a => {
    a.lineNumbers = true
    a.styleActiveLine = true
    a.matchBrackets = true
    a.keyMap = "vim"
    a.lineWrapping = true
    a.showCursorWhenSelecting = true
    a.inputStyle = "contenteditable"
    a.theme = "oceanic-next"
  }))

  codeMirror.getWrapperElement().asInstanceOf[HTMLElement].style.height = "100%"


  {
    val vs = codeMirror.getWrapperElement().asInstanceOf[HTMLElement].getElementsByClassName("CodeMirror-vscrollbar")
    for (i <- 0 until vs.length) {
      vs.item(i).classList.add("ct-scroll")
    }
  }

  var isInnerNormal = true

  CodeMirror.on(codeMirror, "vim-keypress", (e: js.Dynamic) => {
    if (isInnerNormal && e.asInstanceOf[String] == "<Esc>") {
      dismiss()
    }
  })


  CodeMirror.on(codeMirror, "vim-mode-change", (e: js.Dynamic) => {
    val isNormal = e.mode.asInstanceOf[String] == "normal"
    if (!isNormal) isInnerNormal = false
    else window.setTimeout(() => {
      isInnerNormal = true
    }, 0)
  })


  override def focus(): Unit = {
    codeMirror.focus()
  }


  override protected def onDismiss(): Unit = {
    super.onDismiss()
    if(nDismiss != null) {
      nDismiss(codeMirror.getValue().asInstanceOf[String])
      nDismiss = null
    }
  }

  var nDismiss: String => Unit = null
  // codeMirror.getScrollerElement.asInstanceOf[HTMLElement].classList.add("ct-scroll")

  def documentEdit(a: String, onDismiss: String => Unit): Unit = {
    this.nDismiss = onDismiss
    super.show()
    codeMirror.setValue(a)
  }
}
