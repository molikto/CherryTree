package web.view

import client.Client
import command.Key
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.Rect

import scala.collection.mutable.ArrayBuffer


trait OverlayAnchor {
  def rect: Rect
  def onDismiss()
}
abstract class MountedOverlay[ANCHOR <: OverlayAnchor] extends Overlay {


  protected var anchor: ANCHOR = null.asInstanceOf[ANCHOR]

  def show(anchor: ANCHOR): Unit = {
    this.anchor = anchor
    setDomAttributeBy(anchor.rect)
    showOverlay()
  }

  override protected def onDismiss(): Unit = {
    anchor.onDismiss()
    anchor = null.asInstanceOf[ANCHOR]
    super.onDismiss()
  }

  def refresh(): Unit = {
    if (anchor != null) setDomAttributeBy(anchor.rect)
  }
}
abstract class Overlay extends View {

  protected def layer: OverlayLayer

  private var attached = false

  def showOverlay(): Unit = {
    if (!attached) {
      attached = true
      attachTo(layer)
    }
    if (dismissed) {
      dismissed = false
      dom.style.display = "block"
      layer.showOverlay(this)
      focus()
      window.setTimeout(() => {
        if (!dismissed) focus()
      }, 0)
    }
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

  def setDomAttributeBy(rect: Rect): Unit = {
    val bounding = toRect(layer.dom.getBoundingClientRect())
    val rec = rect.moveBy(-bounding.left, -bounding.top)
    //whereToShow(bounding, rec)
    dom.style.left = rec.left.toString + "px"
    dom.style.top = rec.bottom.toString + "px"
  }
}
class OverlayLayer(val parent: HTMLElement, base: View) extends View {

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
