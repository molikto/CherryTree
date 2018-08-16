package web.view


import model.data.Unicode

object LaTeXDialog {
  abstract class Anchor(str: Unicode, insert: Boolean) extends SourceEditOption(str, insert, "stex") with OverlayAnchor {
  }
}

class LaTeXDialog(override val layer: OverlayLayer) extends SourceEditOverlay[LaTeXDialog.Anchor] with MountedOverlay[LaTeXDialog.Anchor]  { // order is important!

  override def onAttach(): Unit = {
    super.onAttach()
    dom.style.width = "560px"
    dom.style.height = "280px"
  }
}
