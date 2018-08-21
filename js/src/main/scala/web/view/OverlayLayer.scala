package web.view

import client.Client
import command.Key
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.Rect

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js


object CoveringOverlay {

  def show(layer: OverlayLayer, dom: HTMLElement, coveringElement: HTMLElement): Unit = {
    val f = coveringElement.getBoundingClientRect()
    val g = layer.parent.getBoundingClientRect()
    dom.style.position = "absolute"
    val paddingLeft = (((f.left - g.left) max 0.0) + 48) + "px"
    val paddingBottom = (((g.bottom - f.bottom) max 0.0) + 48) + "px"
    val paddingTop = (((f.top - g.top) max 0.0) + 48) + "px"
    val paddingRight = (((g.right - f.right) max 0.0) + 48) + "px"
    dom.style.width = s"calc(100% - $paddingLeft - $paddingRight)"
    dom.style.height = s"calc(100% - $paddingTop - $paddingBottom)"
    dom.style.top = paddingTop
    dom.style.left = paddingLeft
  }
}

trait OverlayAnchor {
  def rect: Rect
}

trait MountedOverlay[ANCHOR <: OverlayAnchor] extends OverlayT[ANCHOR] {


  protected var anchor: ANCHOR = null.asInstanceOf[ANCHOR]

  override def show(anchor: ANCHOR): Unit = {
    this.anchor = anchor
    super.show(anchor)
    setDomAttributeBy(anchor.rect)
  }

  override protected def onDismiss(): Unit = {
    anchor = null.asInstanceOf[ANCHOR]
    super.onDismiss()
  }

  def refresh(): Unit = {
    if (!dismissed && anchor != null) setDomAttributeBy(anchor.rect)
  }

  def setDomAttributeBy(rect: Rect): Unit = {
    if (dom.clientWidth == 0) {
      if (!dismissed && anchor != null) {
        window.setTimeout(() => setDomAttributeBy(rect), 14)
      }
    } else {
      val bounding = toRect(layer.dom.getBoundingClientRect())
      val rec = rect.moveBy(-bounding.left, -bounding.top)
      var view = Rect(rec.left, rect.bottom, dom.clientWidth, dom.clientHeight)
      if (view.bottom > bounding.height) {
        view = view.moveBy(0, -rec.height - view.height)
      }
      if (view.top < 0) {
        view = view.moveBy(0, -view.top)
      }
      if (view.right > bounding.width) {
        view = view.moveBy(bounding.width - view.right, 0)
      }
      if (view.left < 0) {
        view = view.moveBy(-view.left, 0)
      }
      //whereToShow(bounding, rec)
      dom.style.position = "absolute"
      dom.style.left = view.left.toString + "px"
      dom.style.top = view.top.toString + "px"
    }
  }
}

trait Overlay extends View {
  def dismiss()
}

trait OverlayT[T <: Any] extends Overlay {

  protected def layer: OverlayLayer

  private var attached = false

  protected var opt: T = null.asInstanceOf[T]

  def show(t: T): Unit = {
    opt = t
    if (!attached) {
      attached = true
      attachTo(layer)
    }
    if (dismissed) {
      dismissed = false
      dom.style.display = "block"
      layer.onShowOverlay(this)
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
    opt = null.asInstanceOf[T]
    dom.style.display = "none"
    layer.onDismissOverlay(this)
  }

  protected var dismissed = true


  /**
    * will also remove from parent
    * ALSO make sure you destroy child dom attachments!!!
    */
  override def destroy(): Unit = {
    dismiss()
    super.destroy()
  }

}

class OverlayLayer(val parent: HTMLElement, base: View) extends View {

  private val showingOverlay = ArrayBuffer[Overlay]()

  def onShowOverlay(overlay: Overlay): Unit = {
    if (!showingOverlay.contains(overlay)) {
      if (showingOverlay.isEmpty) show()
      showingOverlay.append(overlay)
    }
  }

  def onDismissOverlay(overlay: Overlay): Unit = {
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


  private val clicker = div(
    position := "absolute",
    width := "100%",
    height := "100%"
  ).render

  dom = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    width := "100%",
    height := "100%",
    zIndex := "100",
    display := "none",
    clicker
  ).render

  private def show(): Unit = {
    dom.style.display = "block"
    clicker.addEventListener("click", clickEvent)
    clicker.addEventListener("dbclick", clickEvent)
  }

  private val clickEvent: js.Function1[MouseEvent, Unit] = (e: MouseEvent) => {
    dismissAllOverlay()
  }


  def dismissAllOverlay(): Unit = {
    while (showingOverlay.nonEmpty) {
      showingOverlay.head.dismiss()
    }
  }



  private def dismiss(): Unit = {
    clicker.removeEventListener("click", clickEvent)
    clicker.removeEventListener("dbclick", clickEvent)
    dom.style.display = "none"
    base.focus()
  }

}
