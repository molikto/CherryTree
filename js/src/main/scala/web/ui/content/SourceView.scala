package web.ui.content

import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view._
import web.ui._

import scala.scalajs.js

class SourceView(
  initData: model.data.Content.Code
) extends StaticCodeView {

  // background := "#304148",
  private val preCode = pre(`class` := "ct-code-pre cm-s-oceanic-next").render

  private val remainingView = p(
    `class` := "ct-sans ct-hint-color",
    marginTop := "0px",
    marginLeft := "4px",
    marginBottom := "4px",
    fontSize := "70%",
    "").render
  dom = div(contenteditable := "false", preCode, remainingView).render

  protected override def onUpdateContent(contentData: model.data.Content.Code) {
    removeAllChild(preCode)
    val lines = contentData.unicode.str.lines
    val look = lines.take(5).toVector
    val remaining = lines.size
    val totalSize = remaining + look.size
    CodeMirror.runMode(look.mkString("\n"), contentData.ty.codeMirror, preCode)
    val sourceType = SourceEditType.all.find(contentData.ty != EmptyCodeType && _.ct == contentData.ty)
    if (remaining > 0) {
      val sourceString = sourceType.map(_.name + ", ").getOrElse("")
      remainingView.textContent = sourceString + s"$totalSize lines"
    } else if (contentData.unicode.isBlank) {
      remainingView.textContent = if (sourceType.isEmpty) "empty code block" else sourceType.get.name + ", empty"
    } else {
      remainingView.textContent = sourceType.map(_.name).getOrElse("")
    }
  }

  updateContent(initData)
}
