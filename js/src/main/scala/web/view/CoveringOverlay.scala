package web.view

import org.scalajs.dom.raw.HTMLElement

abstract class CoveringOverlay(val covering: () => HTMLElement) extends Overlay {

  override def showOverlay(): Unit = {
    val f = covering().getBoundingClientRect()
    val g = layer.parent.getBoundingClientRect()
    dom.style.paddingLeft = (((f.left - g.left) max 0.0) + 48) + "px"
    dom.style.paddingBottom = (((g.bottom - f.bottom) max 0.0) + 48) + "px"
    dom.style.paddingTop = (((f.top - g.top) max 0.0) + 48) + "px"
    dom.style.paddingRight = (((g.right - f.right) max 0.0) + 48) + "px"
    super.showOverlay()
  }
}
