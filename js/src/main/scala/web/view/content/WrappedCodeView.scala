package web.view.content

import model._
import model.data.{Content, _}
import model.mode.Content.CodeInside
import model.operation.Content
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.doc.DocumentView
import web.view.{EmptyStr, removeAllChild, theme}
import web.view._

import scala.scalajs.js



class WrappedCodeView(
  initData: model.data.Content.Code
) extends ContentView.Code {

  override def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General =
    new CodeViewEditor(documentView, controller, this).asInstanceOf[ContentViewEditor.General]
  setInitialContent(initData)

  private[content] var codeView: ContentView.Code = ContentView.createFromCode(initData)

  protected override def onUpdateContent(contentData: model.data.Content.Code): Unit = {
    if (ContentView.matches(contentData.ty, codeView)) {
      codeView.updateContent(contentData)
    } else {
      codeView.destroy()
      codeView = ContentView.createFromCode(contentData)
      codeView.attachToNode(dom)
    }
  }


  override def destroy(): Unit = {
    codeView.destroy()
    super.destroy()
  }

  protected override def onUpdateContent(c: model.data.Content.Code, trans: operation.Content.Code, viewUpdated: Boolean): Unit = {
    if (ContentView.matches(c.ty, codeView)) {
      codeView.updateContent(c, trans, viewUpdated)
    } else {
      codeView.destroy()
      codeView = ContentView.createFromCode(c)
      codeView.attachToNode(dom)
    }
  }

  dom = div(
  ).render

  override def onAttach(): Unit = {
    super.onAttach()
    codeView.attachToNode(dom)
  }
}
