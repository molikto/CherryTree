package web.view


import web._
import model._
import model.data._
import model.mode.Content.CodeInside
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import web.view.doc.DocumentView

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

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
