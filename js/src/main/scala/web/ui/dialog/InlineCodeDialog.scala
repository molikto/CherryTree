package web.ui.dialog

import model.data._
import model.mode.Content.CodeInside
import settings.Settings
import web.view._

object InlineCodeDialog {
  abstract class Anchor(str: Unicode, mode: CodeInside, ty: CodeType, editable: Boolean) extends SourceEditOption(str, mode, ty, editable) with OverlayAnchor {
  }
}

class InlineCodeDialog(
  override val settings: Settings,
  override val editor: _root_.view.SourceEditInterface,
  override val layer: OverlayLayer) extends SourceEditOverlay[InlineCodeDialog.Anchor] with MountedOverlay[InlineCodeDialog.Anchor]  { // order is important!

  override def showLineNumber = false

  override protected def predefined: Seq[CodeType] = CodeType.inline

  override def onAttach(): Unit = {
    super.onAttach()
    dom.style.width = "560px"
    dom.style.height = "140px"
  }
}
