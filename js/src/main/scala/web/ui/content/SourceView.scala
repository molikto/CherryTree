package web.ui.content

import scalatags.JsDom.all._
import web.view._
import web.ui.dialog.SourceEditOverlay

class SourceView(
  initData: model.data.Content.Code
) extends StaticCodeView {


  // background := "#304148",
  private val preCode = pre(cls := "ct-code-pre cm-s-oceanic-next").render

  private val remainingView = p(
    cls := "ct-ui-font ct-hint-color",
    marginTop := "0px",
    marginLeft := "4px",
    marginBottom := "4px",
    fontSize := "0.70rem",
    "").render
  dom = div(contenteditable := "false", preCode, remainingView).render

  protected override def onUpdateContent(contentData: model.data.Content.Code) {
    removeAllChild(preCode)
    val sourceType = contentData.lang
    val lines = contentData.unicode.str.split('\n').toSeq
    val look = lines.take(5).toVector
    val remaining = lines.size
    val totalSize = remaining + look.size
    if (remaining > 0) {
      val sourceString = sourceType.displayNmae  + ", "
      remainingView.textContent = sourceString + s"$totalSize lines"
    } else if (contentData.unicode.isBlank) {
      remainingView.textContent = sourceType.displayNmae + ", empty"
    } else {
      remainingView.textContent = sourceType.displayNmae
    }
    val toRun = look.mkString("\n")
    SourceEditOverlay.renderSourceInto(toRun, contentData.lang, preCode)
  }

  updateContent(initData)
}
