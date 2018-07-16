package web.view

import model._

trait ContentView extends View {
  def updateContent(c: data.Content, trans: operation.Content, viewUpdated: Boolean)

  def updateMode(aa: mode.Content, viewUpdated: Boolean)

  def clearMode(): Unit

  def initMode(): Unit
}
