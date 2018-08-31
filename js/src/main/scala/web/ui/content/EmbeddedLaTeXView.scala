package web.ui.content

import scalatags.JsDom.all._
import web.ui.doc.LaTeXMacroCache
import web.view._

class EmbeddedLaTeXView(initData: model.data.Content.Code
) extends StaticCodeView  {

  dom = div(contenteditable := "false").render


  override def refreshLaTeX(): Unit = {
    updateContent(contentData)
  }

  protected override def onUpdateContent(contentData: model.data.Content.Code): Unit = {
    LaTeXMacroCache.renderLaTeX(dom, contentData.unicode.str, 0, true)
  }

  updateContent(initData)
}
