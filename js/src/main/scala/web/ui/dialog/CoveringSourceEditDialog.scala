package web.ui.dialog

import org.scalajs.dom.raw.HTMLElement
import view.SourceEditInterface
import web.view.{CoveringOverlay, OverlayLayer}
import web.view._


class CoveringSourceEditDialog(
  override val editor: SourceEditInterface,
  protected val layer: OverlayLayer,
  val coveringElement: HTMLElement
) extends SourceEditOverlay[SourceEditOption] { // ordering is important
  override def show(opt: SourceEditOption): Unit = {
    CoveringOverlay.show(layer, dom, coveringElement)
    super.show(opt)
  }
}
