package web.ui

import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Span
import org.scalajs.dom.raw.{CompositionEvent, Element, ErrorEvent, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.ui.doc.DocumentView
import web.view._

package object content {



  def warningInline(str: String): JsDom.TypedTag[Span] = {
    span(display := "inline-block", span(`class` := "ct-warning-inline", str))
  }

  def errorInline(str: String, th: Throwable = null): JsDom.TypedTag[Span] = {
    span(display := "inline-block", span(`class` := "ct-error-inline", str))
  }
}
