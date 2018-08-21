package web.view.content

import model.data
import model.data.{Embedded, SourceCode}
import util.Rect
import view.EditorInterface
import web.view.View
import web.view.content.ContentViewEditor.General
import web.view.doc.DocumentView


object ContentView {
  type General = ContentView[data.Content, model.operation.Content]
  type Code = ContentView[data.Content.Code, model.operation.Content.Code]
  type Rich = ContentView[data.Content.Rich, model.operation.Content.Rich]

  def createFromCode(a: data.Content.Code): Code = {
    a.ty match {
      case Embedded("html") =>
        new EmbeddedHtmlView(a)
      case _ =>
        new SourceView(a)
    }
  }

  def matches(a: data.CodeType, v: Code): Boolean = {
    a match {
      case Embedded("html") => v.isInstanceOf[EmbeddedHtmlView]
      case _ => v.isInstanceOf[SourceView]
    }
  }

  def create(a: data.Content, editable: Boolean = false): General = {
    (a match {
      case data.Content.Rich(r) => new RichView(r)
      case s: data.Content.Code => if (editable) new WrappedCodeView(s) else  createFromCode(s)
    }).asInstanceOf[General]
  }
}

trait ContentView[T <: data.Content, O <: model.operation.Content] extends View {

  def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General

  var contentData: T
  def updateContent(c: T, trans: O, viewUpdated: Boolean): Unit = {
    contentData = c
    if (!viewUpdated) {
      updateContent()
    }
  }

  def updateContent()
}
