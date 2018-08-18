package web.view.content

import model.data
import model.data.{Embedded, SourceCode}
import util.Rect
import web.view.View


object ContentView {
  type General = ContentView[data.Content, model.operation.Content]
  type Code = ContentView[data.Content.Code, model.operation.Content.Code]

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

  def create(a: data.Content): General = {
    (a match {
      case data.Content.Rich(r) => new RichView(r)
      case s: data.Content.Code => createFromCode(s)
    }).asInstanceOf[General]
  }
}

trait ContentView[T <: data.Content, O <: model.operation.Content] extends View {
  var contentData: T
  def updateContent(c: T, trans: O, viewUpdated: Boolean, editorUpdated: Boolean): Unit = {
    contentData = c
    if (!viewUpdated) {
      updateContent()
    }
  }
  def updateContent()
}
