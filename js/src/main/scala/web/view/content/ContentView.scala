package web.view.content

import model.data
import util.Rect
import web.view.View


object ContentView {
  type General = ContentView[data.Content, model.operation.Content]

  def create(a: data.Content): View = {
    a match {
      case data.Content.Rich(r) => new RichView(r)
      case s: data.Content.Code => new SourceView(s)
    }
  }
}

trait ContentView[T <: data.Content, O <: model.operation.Content] extends View {
  var contentData: T
  def updateContent(c: T, trans: O, viewUpdated: Boolean)
}
