package web.view


import model.data.Unicode

object LaTeXDialog {
  trait Anchor extends OverlayAnchor {
    def update(url: Unicode): Unit
  }
}

class LaTeXDialog(override val layer: OverlayLayer) extends MountedOverlay[LaTeXDialog.Anchor] with SourceEditOverlay { // order is important!

  var uni: Unicode = Unicode.empty


  override def onAttach(): Unit = {
    dom.style.width = "560px"
    dom.style.height = "280px"
  }

  def show(anchor: LaTeXDialog.Anchor, insert: Boolean, uni: Unicode): Unit = {
    this.uni = uni
    beforeShowMounted(anchor)
    show(uni, insert, "stex", str => anchor.update(str))
  }


  override def show(anchor: LaTeXDialog.Anchor): Unit = {
    throw new IllegalStateException("Call another one!")
  }
}
