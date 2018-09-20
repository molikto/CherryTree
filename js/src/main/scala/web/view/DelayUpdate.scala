package web.view

import client.Client
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}
import scalatags.JsDom.all._
import web.view.{UnselectableView, View}

import scala.util.Try


trait DelayUpdate extends  View {

  protected var previousUpdateTime = 0L
  private var scheduledUpdate: Int = -1

  /**
    * will also remove from parent
    * ALSO make sure you destroy child dom attachments!!!
    */
  override def destroy(): Unit = {
    if (scheduledUpdate != -1) {
      window.clearTimeout(scheduledUpdate)
    }
    super.destroy()
  }

  def renderDelayed(): Unit

  def checkShouldUpdate(): Boolean = {
    var t = System.currentTimeMillis()
    if (t - previousUpdateTime < 1000) {
      if (scheduledUpdate == -1) {
        scheduledUpdate = window.setTimeout(() => {
          scheduledUpdate = -1
          renderDelayed()
        }, previousUpdateTime + 1100 - t)
      }
      return false
    }
    previousUpdateTime = t
    if (scheduledUpdate != -1) {
      window.clearTimeout(scheduledUpdate)
      scheduledUpdate = -1
    }
    return true
  }
}

