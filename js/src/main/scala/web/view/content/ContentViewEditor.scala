package web.view.content

import model._
import util.Rect
import web.view.View



object ContentViewEditor {
  type General = ContentViewEditor[data.Content, model.operation.Content, model.mode.Content]
}

abstract class ContentViewEditor[T <: data.Content, O <: model.operation.Content, M <: model.mode.Content](val contentView: ContentView[T, O]) {


  def updateContent(c: T, m: Option[M], trans: O, viewUpdated: Boolean, editorUpdated: Boolean): Unit = {
    contentView.updateContent(c, trans, viewUpdated)
  }

  def refreshRangeSelection(): Unit = {

  }

  def updateMode(aa: M, viewUpdated: Boolean, editorUpdated: Boolean, fromUser: Boolean)

  def clearMode(): Unit

  def selectionRect: Rect
}
