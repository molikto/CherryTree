package web.view

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
import web.view.doc.DocumentView
import web.view._

package object content {



  def warningInline(str: String): JsDom.TypedTag[Span] = {
    span(`class` := "ct-warning-inline", contenteditable := "false", str)
  }

  def errorInline(str: String, th: Throwable = null): JsDom.TypedTag[Span] = {
    span(`class` := "ct-error-inline", contenteditable := "false", str)
  }
}
