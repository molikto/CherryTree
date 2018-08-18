package web.view.content
import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Span
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.doc.DocumentView
import web.view._

import scala.scalajs.js

class EmbeddedHtmlView(var contentData: model.data.Content.Code
) extends ContentView.Code {

  dom = div().render

  updateContent()

  override def updateContent(): Unit = {
    try {
      dom.innerHTML = contentData.unicode.str
    } catch {
      case error: Throwable =>
        removeAllChild(dom)
        dom.appendChild(errorInline("embedded HTML error", error).render)
    }
  }
}
