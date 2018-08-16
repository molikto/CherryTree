package web.view.content

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
import web.view.{EmptyStr, removeAllChild, theme}
import web.view._

import scala.scalajs.js

class SourceView(
  var c: model.data.Content.Code
) extends ContentView[model.data.Content.Code, model.operation.Content.Code]  {

  // background := "#304148",
  private val preCode = pre(`class` := "ct-code-pre cm-s-oceanic-next").render

  private val remainingView = p(
    `class` := "ct-sans",
    marginTop := "0px",
    marginLeft := "4px",
    marginBottom := "4px",
    fontSize := "70%",
    color := theme.disalbedInfo,
    "").render
  dom = div(preCode, remainingView).render


  override def updateContent(c: model.data.Content.Code, trans: model.operation.Content.Code, viewUpdated: Boolean): Unit = {
    this.c = c
    updateContent()
  }

  def updateContent() {
    removeAllChild(preCode)
    updateCodeMirror()
  }

  def updateCodeMirror(): Unit = {
    val lines = c.unicode.str.lines
    val look = lines.take(5).toVector
    val remaining = lines.size
    val totalSize = remaining + look.size
    CodeMirror.runMode(look.mkString("\n"), c.asSourceMime, preCode)
    if (remaining > 0) {
      remainingView.textContent = s"$totalSize lines"
    } else if (c.unicode.isBlank) {
      remainingView.textContent = "empty code block"
    } else {
      remainingView.textContent = ""
    }
  }

  updateCodeMirror()


}
