package web.ui.dialog

import org.scalajs.dom.raw.HTMLElement
import web.view.{CoveringOverlay, OverlayLayer}
import web.view._


class CoveringSourceEditDialog(
  override val editor: _root_.view.EditorInterface,
  protected val layer: OverlayLayer,
  val coveringElement: HTMLElement
) extends SourceEditOverlay[SourceEditOption] { // ordering is important
  override def show(opt: SourceEditOption): Unit = {
    CoveringOverlay.show(layer, dom, coveringElement)
    super.show(opt)
  }
}
