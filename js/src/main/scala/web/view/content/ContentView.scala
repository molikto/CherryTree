package web.view.content

import model._
import util.Rect
import web.view.View

object ContentView {
  type General = ContentView[data.Content, model.operation.Content, model.mode.Content]
}

trait ContentView[T <: data.Content, O <: model.operation.Content, M <: model.mode.Content] extends View {
  def updateContent(c: T, trans: O, viewUpdated: Boolean)

  def updateMode(aa: M, viewUpdated: Boolean)

  def clearMode(): Unit

  def initMode(): Unit

  def selectionRect: Rect
}
