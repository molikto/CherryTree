package web.view

import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.doc.DocumentView
import web.view._

class SourceEditDialog(protected val layer: OverlayLayer) extends Overlay {

  private val ta = textarea(
    flex := "1 1 auto",
    width := "100%",
    name := "code"
  ).render

  private val dialog = form(
    display := "flex",
    flexDirection := "column-reverse",
    flex := "1 1 auto",
    height := "100%",
    padding := "8px",
    `class` := "ct-card",
    ta).render

  dom = div(
    position := "absolute",
    width := "100%",
    height := "100%",
    display := "flex",
    dialog
  ).render

  val codeMirror = CodeMirror.fromTextArea(ta, jsObject(a => {
    a.lineNumbers = true
    a.styleActiveLine = true
    a.matchBrackets = true
    a.keyMap = "vim"
    a.showCursorWhenSelecting = true
    a.inputStyle = "contenteditable"
    a.theme = "oceanic-next"
  }))


  override def focus(): Unit = {
    codeMirror.focus()
  }

  // codeMirror.getScrollerElement.asInstanceOf[HTMLElement].classList.add("ct-scroll")

  def documentEdit(a: String, followElement: HTMLElement): Unit = {
    codeMirror.setValue(a)
    val f = followElement.getBoundingClientRect()
    val g = layer.parent.getBoundingClientRect()
    dom.style.paddingLeft = (((f.left - g.left) max 0.0) + 48) + "px"
    dom.style.paddingBottom = (((g.bottom - f.bottom) max 0.0) + 48) + "px"
    dom.style.paddingTop = (((f.top - g.top) max 0.0) + 48) + "px"
    dom.style.paddingRight = (((g.right - f.right) max 0.0) + 48) + "px"
    super.showOverlay()
  }
}
