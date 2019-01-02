package web.ui.content
import scalatags.JsDom.all._
import web.view._

private [content] class EmbeddedHtmlView(initData: model.data.Content.Code
) extends StaticCodeView  {

  dom = div(contenteditable := "false").render


  protected override def onUpdateContent(contentData: model.data.Content.Code): Unit = {
    try {
      dom.innerHTML = contentData.unicode.str
    } catch {
      case error: Throwable =>
        removeAllChild(dom)
        dom.appendChild(errorInline("embedded HTML error", error).render)
    }
  }

  updateContent(initData)
}
