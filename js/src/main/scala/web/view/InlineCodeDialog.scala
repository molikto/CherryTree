package web.view


import web._
import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import web.view.doc.DocumentView
import web.view._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

object InlineCodeDialog {
  abstract class Anchor(str: Unicode, insert: Boolean, ty: CodeType) extends SourceEditOption(str, insert, ty) with OverlayAnchor {
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
