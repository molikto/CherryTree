package web.view

import model.mode

trait ContentView extends View {
  def syncMode(aa: mode.Content)

  def clearMode(): Unit

  def initMode(): Unit
}
