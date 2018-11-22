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

private [content] class SourceView(
  initData: model.data.Content.Code,
  val infinite: Boolean = false
) extends StaticCodeView {


  // background := "#304148",
  private val preCode = pre(cls := "ct-code-pre cm-s-oceanic-next").render

  private val remainingView = p(
    cls := "ct-sans ct-hint-color",
    marginTop := "0px",
    marginLeft := "4px",
    marginBottom := "4px",
    fontSize := "70%",
    "").render
  dom = div(contenteditable := "false", preCode, remainingView).render

  protected override def onUpdateContent(contentData: model.data.Content.Code) {
    removeAllChild(preCode)
    val sourceType = SourceEditType.all.find(contentData.ty != EmptyCodeType && _.ct == contentData.ty)
    val toRun = if (infinite) {
      remainingView.textContent = sourceType.map(_.name).getOrElse("")
      contentData.unicode.str
    } else {
      val lines = contentData.unicode.str.lines
      val look = lines.take(5).toVector
      val remaining = lines.size
      val totalSize = remaining + look.size
      if (remaining > 0) {
        val sourceString = sourceType.map(_.name + ", ").getOrElse("")
        remainingView.textContent = sourceString + s"$totalSize lines"
      } else if (contentData.unicode.isBlank) {
        remainingView.textContent = if (sourceType.isEmpty) "empty code block" else sourceType.get.name + ", empty"
      } else {
        remainingView.textContent = sourceType.map(_.name).getOrElse("")
      }
      look.mkString("\n")
    }
    val cmMode = contentData.ty.codeMirror
    if (cmMode != "" && !CodeMirror.modes.asInstanceOf[js.Object].hasOwnProperty(cmMode)) {
      CodeMirror.requireMode(cmMode, () => {
        if (this.contentData == contentData) {
          CodeMirror.runMode(toRun, cmMode, preCode)
        }
      });
    } else {
      CodeMirror.runMode(toRun, cmMode, preCode)
    }
  }

  updateContent(initData)
}
