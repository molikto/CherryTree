package web.view

import client.Client
import command.Key
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.Rect

import scala.collection.mutable.ArrayBuffer

abstract class Overlay extends View {
  def layer: OverlayLayer

  private val attached = false

  def showOverlay(): Unit = {
    if (!attached) {
      attachTo(layer)
    }
    dismissed = false
    dom.style.display = "block"
    layer.showOverlay(this)
  }

  def dismiss(): Unit = {
    if (!dismissed) {
      dismissed = true
      onDismiss()
    }
  }

  protected def onDismiss(): Unit = {
    dom.style.display = "none"
    layer.dismissOverlay(this)
  }

  private var dismissed = true


  /**
    * will also remove from parent
    * ALSO make sure you destroy child dom attachments!!!
    */
  override def destroy(): Unit = {
    dismiss()
    super.destroy()
  }
}
class OverlayLayer(base: View) extends View {

  private val showingOverlay = ArrayBuffer[Overlay]()
  def showOverlay(overlay: Overlay): Unit = {
    if (!showingOverlay.contains(overlay)) {
      if (showingOverlay.isEmpty) show()
      showingOverlay.append(overlay)
    }
  }

  def dismissOverlay(overlay: Overlay): Unit = {
    if (showingOverlay.contains(overlay)) {
      val index = showingOverlay.indexOf(overlay)
      showingOverlay.remove(index)
      if (showingOverlay.isEmpty) {
        dismiss()
      } else if (index == showingOverlay.size) {
        showingOverlay.last.focus()
      }
    }
  }


  dom = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    width := "100%",
    height := "100%",
    zIndex := "100",
    display := "none",
  ).render

  private def show(): Unit = {
    dom.style.display = "block"
  }


  private def dismiss(): Unit = {
    dom.style.display = "none"
    base.focus()
  }

}
