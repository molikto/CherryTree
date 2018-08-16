package web.view

import org.scalajs.dom.raw.HTMLElement


class CoveringSourceEditDialog(
  protected val layer: OverlayLayer,
  val coveringElement: HTMLElement
) extends SourceEditOverlay[SourceEditOption] { // ordering is important
  override def show(opt: SourceEditOption): Unit = {
    CoveringOverlay.show(layer, dom, coveringElement)
    super.show(opt)
  }
}
