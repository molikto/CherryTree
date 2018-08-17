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

object LaTeXDialog {
  abstract class Anchor(str: Unicode, insert: Boolean) extends SourceEditOption(str, insert, LaTeXEmbedded) with OverlayAnchor {
  }
}

class LaTeXDialog(override val layer: OverlayLayer) extends SourceEditOverlay[LaTeXDialog.Anchor] with MountedOverlay[LaTeXDialog.Anchor]  { // order is important!

  override def showLineNumber = false
  override def exitOnInputDollarSign: Boolean = true


  override protected def predefined: Seq[(String, CodeType)] = SourceEditOverlay.inlineOnly

  override protected def desc: HTMLElement = div(
    display := "flex",
    flexDirection := "column",
    justifyContent := "center",
    `class` := "ct-desc",
    "insert $ to exit").render

  override def onAttach(): Unit = {
    super.onAttach()
    dom.style.width = "560px"
    dom.style.height = "140px"
  }
}
