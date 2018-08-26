package web.ui.content
import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Span
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import org.w3c.dom.Element
import org.w3c.dom.html.HTMLElement
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.ui.doc.DocumentView
import web.view._

import scala.scalajs.js

class EmbeddedHtmlView(initData: model.data.Content.Code
) extends StaticCodeView  {

  dom = div(contenteditable := "false").render


  protected override def onUpdateContent(contentData: model.data.Content.Code): Unit = {
    try {
      dom.innerHTML = contentData.unicode.str
    } catch {
      case error: Throwable =>
        removeAllChild(dom)
        dom.appendChild(errorInline("embedded HTML error", error).render)
    }
  }

  updateContent(initData)
}
