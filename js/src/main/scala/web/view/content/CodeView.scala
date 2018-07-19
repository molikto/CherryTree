package web.view.content

import model.data.Unicode
import scalatags.JsDom.all._
import view.EditorInterface
import web.view.doc.DocumentView

class CodeView(documentView: DocumentView, controller: EditorInterface, var code: Unicode, var lang: String) extends ContentView[model.data.Content.Code, model.operation.Content.Code, model.mode.Content.Code] {

  dom = pre(code.toString).render


  override def updateContent(c: model.data.Content.Code, trans: model.operation.Content.Code, viewUpdated: Boolean): Unit = {
    code = c.unicode
    lang = c.lang
    dom = pre(code.toString).render
  }

  override def updateMode(aa: model.mode.Content.Code, viewUpdated: Boolean): Unit = {
  }

  override def clearMode(): Unit = {
    dom.classList.remove("ct-node-visual")
  }

  override def initMode(): Unit = {
    dom.classList.add("ct-node-visual")
  }
}
