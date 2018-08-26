package web.ui.dialog

import model.data._
import model.mode.Content.CodeInside
import web.view._

object InlineCodeDialog {
  abstract class Anchor(editor: _root_.view.SourceEditInterface, str: Unicode, mode: CodeInside, ty: CodeType) extends SourceEditOption(editor, str, mode, ty) with OverlayAnchor {
  }
}

class InlineCodeDialog(override val layer: OverlayLayer) extends SourceEditOverlay[InlineCodeDialog.Anchor] with MountedOverlay[InlineCodeDialog.Anchor]  { // order is important!

  override def showLineNumber = false

  override protected def predefined: Seq[SourceEditType] = SourceEditOverlay.inlineOnly

  override def onAttach(): Unit = {
    super.onAttach()
    dom.style.width = "560px"
    dom.style.height = "140px"
  }
}
