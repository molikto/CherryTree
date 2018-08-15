package web.view


import model.data.Unicode

object LaTeXDialog {
  trait Anchor extends OverlayAnchor {
    def update(url: Unicode): Unit
  }
}

class LaTeXDialog(override val layer: OverlayLayer) extends MountedOverlay[LaTeXDialog.Anchor] {
}
