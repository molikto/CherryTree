package web.view.content

import model._
import util.Rect
import web.view.View



object EditableContentView {
  type General = EditableContentView[data.Content, model.operation.Content, model.mode.Content]
}

trait EditableContentView[T <: data.Content, O <: model.operation.Content, M <: model.mode.Content] extends ContentView[T, O] {

  def updateMode(aa: M, viewUpdated: Boolean, fromUser: Boolean)

  def clearMode(): Unit

  def initMode(): Unit

  def selectionRect: Rect
}
